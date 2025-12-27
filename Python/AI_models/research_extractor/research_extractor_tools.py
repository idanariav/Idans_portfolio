"""
LangChain tools for the research extractor agent.

Each tool wraps a function from the original pipeline with proper
schemas and docstrings for the agent to understand when to use them.
"""

import os
import re
import json
import ast
from typing import Dict, Any
from datetime import datetime
from concurrent.futures import ThreadPoolExecutor, TimeoutError as FuturesTimeoutError
from langchain.tools import tool
from dotenv import load_dotenv
from openai import OpenAI

from research_extractor_api_utils import (
    fetch_web_api,
    fetch_semantic_scholar_metadata,
    fetch_openalex_metadata,
    fetch_google_books_metadata,
)
from research_extractor_prompts import (
    get_analyze_reference_prompt,
    get_generate_note_prompt,
)
from research_extractor_constants import (
    MODEL,
    FOLDER_MAP,
    FETCH_TIMEOUT,
    OPENROUTER_API_BASE,
    PATTERN_DOUBLE_NEWLINE,
    PATTERN_NUMBERED_LIST,
    PATTERN_DOI,
    PATTERN_ARXIV,
    PATTERN_CORPUSID,
    PATTERN_ISBN,
    PATTERN_URL,
    PATTERN_CITATION,
    PATTERN_INVALID_FILENAME_CHARS,
    PATTERN_INVALID_FILENAME_CHARS_ALT,
    ARTICLE_DOMAINS,
    VIDEO_DOMAINS,
    DEFAULT_TITLE,
    DEFAULT_AUTHOR,
    DEFAULT_PUBLISHED_DATE,
    DEFAULT_YEAR,
    DEFAULT_SUMMARY,
    DEFAULT_URL_TEXT,
    DEFAULT_DESCRIPTION,
    DEFAULT_OUTPUT_FOLDER,
    MARKDOWN_TAG_BIBLIOGRAPHY,
    FRONTMATTER_VERSION,
    FRONTMATTER_PUBLISH_DEFAULT,
)

load_dotenv()

# Initialize OpenAI client
client = OpenAI(
    api_key=os.getenv("OPENROUTER_API_KEY"),
    base_url=OPENROUTER_API_BASE,
)


def call_llm(prompt: str, json_format: bool = False) -> Any:
    kwargs = {"model": MODEL, "messages": [{"role": "user", "content": prompt}]}
    if json_format:
        kwargs["response_format"] = {"type": "json_object"}
    return client.chat.completions.create(**kwargs)

def with_timeout(func, timeout: int = FETCH_TIMEOUT):
    def wrapper(*args, **kwargs):
        with ThreadPoolExecutor(max_workers=1) as executor:
            future = executor.submit(func, *args, **kwargs)
            try:
                return future.result(timeout=timeout)
            except FuturesTimeoutError:
                return {"error": f"Timeout ({timeout}s)"}
            except Exception as e:
                return {"error": str(e)}
    return wrapper

def safe_json_parse(data: str) -> Dict:
    """Parse JSON string or return dict as-is. Handles malformed JSON gracefully."""
    if not isinstance(data, str):
        return data
    
    try:
        return json.loads(data)
    except json.JSONDecodeError as e:
        # Try to fix common JSON issues
        try:
            # Attempt 1: Replace literal backslashes with escaped backslashes
            fixed = data.replace('\\', '\\\\')
            return json.loads(fixed)
        except:
            pass
        
        try:
            # Attempt 2: Use ast.literal_eval as fallback
            return ast.literal_eval(data)
        except:
            pass
        
        # If all else fails, return error dict
        return {"error": f"Failed to parse JSON: {str(e)}", "raw_data": data[:200]}


def get_timestamp_metadata() -> Dict[str, str]:
    """Generate timestamp metadata fields for markdown frontmatter.
    
    Returns:
        Dict with UUID, Created, Modified timestamps
    """
    now = datetime.now()
    
    return {
        "uuid": now.strftime("%Y%m%d%H%M%S"),
        "created": now.strftime("%Y-%m-%d %H:%M"),
        "modified": now.strftime("%Y-%m-%d %H:%M"),
    }


@tool
def parse_references_file(file_path: str) -> Dict[str, Any]:
    """Load and parse references from text file. Splits by double newlines or numbered lists.
    
    Args: file_path - Absolute path to text file
    Returns: {references: [list], count: int} or {error: str}
    """
    try:
        with open(file_path, "r", encoding="utf-8") as f:
            text = f.read()
        
        refs = [r.strip() for r in re.split(PATTERN_DOUBLE_NEWLINE, text) if r.strip()]
        if len(refs) <= 1:
            refs = [r.strip() for r in re.split(PATTERN_NUMBERED_LIST, text) if r.strip()]
        
        return {"references": refs, "count": len(refs)}
    except FileNotFoundError:
        return {"error": f"File not found: {file_path}"}
    except Exception as e:
        return {"error": str(e)}


@tool
def analyze_reference(reference: str) -> Dict[str, Any]:
    """Analyze reference: classify source type AND extract identifier in one LLM call. Includes validation.
    
    Args: reference - Text to analyze
    Returns: {source_type, identifier_type, identifier_value, is_valid, validation_reason, url?} or {error}
    """
    try:
        prompt = get_analyze_reference_prompt(reference)
        resp = call_llm(prompt, json_format=True)
        result = json.loads(resp.choices[0].message.content)
        
        # Ensure identifier_value is never None
        if result.get("identifier_value") is None:
            result["identifier_value"] = ""
        
        return result
    except Exception as e:
        return {"error": str(e)}


@tool
def fetch_paper_metadata(identifier_info: str) -> Dict[str, Any]:
    """Fetch paper metadata from Semantic Scholar/OpenAlex. Includes prepared content for note generation.
    
    Args: identifier_info - JSON with identifier_type and identifier_value
    Returns: {title, authors, year, abstract, tldr, url, source, content_for_note} or {error: str}
    """
    try:
        info = safe_json_parse(identifier_info)
        id_type = info.get("identifier_type")
        id_value = info.get("identifier_value")
        
        def _fetch():
            # Try Semantic Scholar first
            result = fetch_semantic_scholar_metadata(id_type, id_value)
            if result:
                return result
            
            # Fallback to OpenAlex
            return fetch_openalex_metadata(id_type, id_value)
        
        metadata = with_timeout(_fetch, timeout=FETCH_TIMEOUT)()
        
        # Inline prepare_content_for_note logic
        if metadata and "error" not in metadata:
            abstract = metadata.get("abstract", "")
            tldr = metadata.get("tldr", "")
            if tldr and abstract:
                metadata["content_for_note"] = f"{tldr}\n\n{abstract}".strip()
            else:
                metadata["content_for_note"] = tldr or abstract or ""
        
        return metadata
    except Exception as e:
        return {"error": str(e)}


@tool
def fetch_web_content(identifier_info: str) -> Dict[str, Any]:
    """Fetch article/lecture/post from web (NYT API, HTML scraping, Tavily). Includes prepared content for note generation.
    
    Args: identifier_info - JSON with identifier_value and optional url
    Returns: {title, content, authors, url, content_for_note} or {error: str}
    """
    try:
        info = safe_json_parse(identifier_info)
        result = with_timeout(fetch_web_api, timeout=FETCH_TIMEOUT)(info)
        
        # Inline prepare_content_for_note logic
        if result and "error" not in result:
            result["content_for_note"] = result.get("content") or result.get("abstract", "")
        
        return result if result else {"error": "No content fetched"}
    except Exception as e:
        return {"error": str(e)}



@tool
def prepare_content_for_note(metadata: str, source_type: str) -> Dict[str, str]:
    """Extract and format content from metadata for note generation.
    
    Research Papers: combines tldr + abstract
    Other types: extracts content field
    
    Args: metadata (JSON from fetch), source_type
    Returns: {content: str} or {error: str}
    """
    try:
        meta = safe_json_parse(metadata)
        
        if source_type == "Research Paper":
            abstract = meta.get("abstract", "")
            tldr = meta.get("tldr", "")
            if tldr and abstract:
                content = f"{tldr}\n\n{abstract}".strip()
            else:
                content = tldr or abstract or ""
        else:
            content = meta.get("content") or meta.get("abstract", "")
        
        if not content:
            return {"error": "No content in metadata"}
        
        return {"content": content}
    except Exception as e:
        return {"error": str(e)}


@tool
def generate_note(source_type: str, content: str) -> Dict[str, Any]:
    """Generate structured note: summary, topics, body_sections.
    
    Args: source_type, content
    Returns: {summary: str, topics: [str], body_sections: {}} or {error: str}
    """
    try:
        prompt = get_generate_note_prompt(source_type, content)
        resp = call_llm(prompt, json_format=True)
        return json.loads(resp.choices[0].message.content)
    except Exception as e:
        return {"error": str(e)}


@tool
def save_markdown(metadata: str, note: str, source_type: str, origin: str, output_dir: str) -> Dict[str, str]:
    """Save formatted markdown with YAML frontmatter to categorized folder.
    
    Args: metadata (JSON string or dict), note (JSON string or dict), source_type, origin, output_dir
    Returns: {file_path: str} or {error: str}
    """
    try:
        # Handle both dict and JSON string inputs
        if isinstance(metadata, dict):
            meta = metadata
        else:
            meta = safe_json_parse(metadata)
        
        if isinstance(note, dict):
            note_data = note
        else:
            note_data = safe_json_parse(note)
        
        # Check for parse errors
        if "error" in meta:
            return {"error": f"Failed to parse metadata: {meta.get('error')}"}
        if "error" in note_data:
            return {"error": f"Failed to parse note: {note_data.get('error')}"}
        
        authors = "\n".join(f"  - [[{a}]]" for a in meta.get("authors", [DEFAULT_AUTHOR]))
        topics = "\n".join(f"  - [[{t} (MOC)]]" for t in note_data.get("topics", [])[:3])
        body = "\n\n".join(f"## {k}\n\n{v}" for k, v in note_data.get("body_sections", {}).items())
        
        # Generate timestamp metadata
        timestamps = get_timestamp_metadata()
        
        md_content = f"""---
UUID: {timestamps['uuid']}
Created: {timestamps['created']}
Modified: {timestamps['modified']}
tags:
  - {MARKDOWN_TAG_BIBLIOGRAPHY}
Version: {FRONTMATTER_VERSION}
publish: {str(FRONTMATTER_PUBLISH_DEFAULT).lower()}
Authors:
{authors}
Summary: {note_data.get('summary', '')}
Year: {meta.get('year', DEFAULT_YEAR)}
Topic:
{topics}
Link: {meta.get('url', '')}
Title: {meta.get('title', DEFAULT_TITLE)}
Source-Type: {source_type}
Origin: {origin}
---

{body}
"""
        
        folder = FOLDER_MAP.get(source_type, DEFAULT_OUTPUT_FOLDER)
        safe_title = re.sub(PATTERN_INVALID_FILENAME_CHARS, "", meta.get("title", DEFAULT_TITLE))
        file_path = os.path.join(output_dir, folder, f"{safe_title}.md")
        
        os.makedirs(os.path.dirname(file_path), exist_ok=True)
        with open(file_path, "w", encoding="utf-8") as f:
            f.write(md_content)
        
        return {"file_path": file_path}
    except Exception as e:
        return {"error": str(e)}


@tool
def fetch_book_metadata(book_title: str) -> Dict[str, Any]:
    """Fetch book metadata from Google Books API.
    
    Args: book_title - Title of the book
    Returns: {title, authors, published_date, description, isbn, page_count, categories, link} or {error}
    """
    return fetch_google_books_metadata(book_title)


@tool
def save_book_to_reading_list(book_metadata: str, origin: str, output_dir: str) -> Dict[str, str]:
    """Save book to reading list markdown file (append mode with duplicate detection).
    
    Args:
        book_metadata: JSON string or dict with book metadata
        origin: Source reference (e.g., "[[Book Title (book)]]")
        output_dir: Base directory for saving
    
    Returns:
        Dict with file_path on success, skipped on duplicate, or error on failure
    """
    try:
        # Parse metadata
        meta = safe_json_parse(book_metadata)
        if "error" in meta:
            return {"error": f"Invalid metadata: {meta['error']}"}
        
        # Sanitize origin for filename (remove [[, ]], (book))
        sanitized_origin = origin.replace("[[", "").replace("]]", "").replace("(book)", "").strip()
        file_path = os.path.join(output_dir, DEFAULT_OUTPUT_FOLDER, f"{sanitized_origin} (reading material).md")
        
        # Check if file exists and for duplicates
        file_exists = os.path.exists(file_path)
        isbn = meta.get("isbn")
        if file_exists and isbn:
            with open(file_path, "r", encoding="utf-8") as f:
                existing_content = f.read()
            
            # Check if ISBN already exists in content
            if f"ISBN: {isbn}" in existing_content:
                return {"skipped": f"Book already in reading list (ISBN: {isbn})", "file_path": file_path}
        
        # Create markdown content (simple format for multi-book list)
        title = meta.get('title', DEFAULT_TITLE)
        authors = ", ".join(meta.get("authors", []))
        published = meta.get('published_date', DEFAULT_PUBLISHED_DATE)
        main_category = meta.get("categories", [DEFAULT_CATEGORY])[0] if meta.get("categories") else DEFAULT_CATEGORY
        description = meta.get('description', DEFAULT_DESCRIPTION)
        identifiers = f"ISBN: {isbn}" if isbn else "No ISBN available"
        if meta.get('page_count'):
            identifiers += f" | Pages: {meta.get('page_count')}"
        
        # Build book entry content
        book_entry = f"""## {title}
**Author:** {authors}  
**Publish Date:** {published}  
**Category:** {main_category}  
**Identifiers:** {identifiers}  

**Description:** {description}

---

"""
        
        os.makedirs(os.path.dirname(file_path), exist_ok=True)
        
        # Add frontmatter only if creating new file
        if not file_exists:
            timestamps = get_timestamp_metadata()
            frontmatter = f"""---
UUID: {timestamps['uuid']}
Created: {timestamps['created']}
Modified: {timestamps['modified']}
tags:
  - {MARKDOWN_TAG_BIBLIOGRAPHY}
Version: {FRONTMATTER_VERSION}
publish: {str(FRONTMATTER_PUBLISH_DEFAULT).lower()}
---

# Reading Material: {sanitized_origin}

"""
            with open(file_path, "w", encoding="utf-8") as f:
                f.write(frontmatter)
        
        # Append book entry
        with open(file_path, "a", encoding="utf-8") as f:
            f.write(book_entry)
        
        return {"file_path": file_path}
    except Exception as e:
        return {"error": str(e)}


# ============================================================================
# HYBRID MODE: Fast Deterministic Classification
# ============================================================================

def classify_reference_fast(reference: str) -> Dict[str, Any]:
    """
    Fast deterministic classification using regex patterns.
    Avoids LLM calls for obvious references.
    
    Returns:
        {
            "is_obvious": bool,
            "confidence": float,  # 0.0-1.0
            "source_type": str or None,
            "identifier_type": str or None,
            "identifier_value": str or None,
            "url": str or None,
            "reason": str
        }
    """
    result = {
        "is_obvious": False,
        "confidence": 0.0,
        "source_type": None,
        "identifier_type": None,
        "identifier_value": None,
        "url": None,
        "reason": ""
    }
    
    # Pattern 1: Clear DOI (highest confidence)
    doi_match = re.search(PATTERN_DOI, reference)
    if doi_match:
        result.update({
            "is_obvious": True,
            "confidence": 0.95,
            "source_type": "Research Paper",
            "identifier_type": "DOI",
            "identifier_value": doi_match.group(1),
            "reason": "Clear DOI pattern"
        })
        return result
    
    # Pattern 2: ArXiv ID
    arxiv_match = re.search(PATTERN_ARXIV, reference, re.IGNORECASE)
    if arxiv_match:
        result.update({
            "is_obvious": True,
            "confidence": 0.95,
            "source_type": "Research Paper",
            "identifier_type": "ArXiv",
            "identifier_value": f"arXiv:{arxiv_match.group(1)}",
            "reason": "Clear ArXiv ID"
        })
        return result
    
    # Pattern 3: Semantic Scholar CorpusID
    if "semanticscholar.org" in reference.lower() or "corpusid" in reference.lower():
        corpus_match = re.search(PATTERN_CORPUSID, reference)
        if corpus_match:
            result.update({
                "is_obvious": True,
                "confidence": 0.90,
                "source_type": "Research Paper",
                "identifier_type": "CorpusID",
                "identifier_value": corpus_match.group(1),
                "reason": "Clear CorpusID"
            })
            return result
    
    # Pattern 4: ISBN for books
    isbn_match = re.search(PATTERN_ISBN, reference, re.IGNORECASE)
    if isbn_match:
        result.update({
            "is_obvious": True,
            "confidence": 0.90,
            "source_type": "Book",
            "identifier_type": "ISBN",
            "identifier_value": isbn_match.group(1),
            "reason": "Clear ISBN"
        })
        return result
    
    # Pattern 5: Clear web URLs with known domains
    url_match = re.search(PATTERN_URL, reference)
    if url_match:
        domain = url_match.group(1).lower()
        url_full = url_match.group(0)
        
        # Known article domains
        if any(d in domain for d in ARTICLE_DOMAINS):
            result.update({
                "is_obvious": True,
                "confidence": 0.85,
                "source_type": "Article",
                "identifier_type": "URL",
                "identifier_value": url_full,
                "url": url_full,
                "reason": "Recognized news/article domain"
            })
            return result
        
        # YouTube/lecture domains
        if any(d in domain for d in VIDEO_DOMAINS):
            result.update({
                "is_obvious": True,
                "confidence": 0.85,
                "source_type": "Lecture",
                "identifier_type": "URL",
                "identifier_value": url_full,
                "url": url_full,
                "reason": "Video platform URL"
            })
            return result
    
    # Pattern 6: Standard citation format with year (Author (Year). Title)
    standard_citation = re.search(PATTERN_CITATION, reference)
    if standard_citation and len(standard_citation.group(3)) > 20:
        title = standard_citation.group(3).split('.')[0].strip()
        result.update({
            "is_obvious": True,
            "confidence": 0.75,
            "source_type": "Research Paper",
            "identifier_type": "Title",
            "identifier_value": title,
            "reason": "Standard academic citation format"
        })
        return result
    
    # Ambiguous case - needs LLM
    result["reason"] = "No clear pattern - requires agent analysis"
    return result


# ============================================================================
# Direct (non-tool) versions for fast path
# ============================================================================

def fetch_paper_metadata_direct(identifier_type: str, identifier_value: str) -> Dict[str, Any]:
    """Direct version of fetch_paper_metadata without tool wrapper."""
    try:
        def _fetch():
            result = fetch_semantic_scholar_metadata(identifier_type, identifier_value)
            if result:
                return result
            return fetch_openalex_metadata(identifier_type, identifier_value)
        
        metadata = with_timeout(_fetch, timeout=FETCH_TIMEOUT)()
        
        # Add content_for_note
        if metadata and "error" not in metadata:
            abstract = metadata.get("abstract", "")
            tldr = metadata.get("tldr", "")
            if tldr and abstract:
                metadata["content_for_note"] = f"{tldr}\n\n{abstract}".strip()
            else:
                metadata["content_for_note"] = tldr or abstract or ""
        
        return metadata
    except Exception as e:
        return {"error": str(e)}


def fetch_web_content_direct(identifier_value: str, url: str = None) -> Dict[str, Any]:
    """Direct version of fetch_web_content without tool wrapper."""
    try:
        info = {"identifier_value": identifier_value}
        if url:
            info["url"] = url
        
        result = with_timeout(fetch_web_api, timeout=FETCH_TIMEOUT)(info)
        
        # Add content_for_note
        if result and "error" not in result:
            result["content_for_note"] = result.get("content") or result.get("abstract", "")
        
        return result if result else {"error": "No content fetched"}
    except Exception as e:
        return {"error": str(e)}


def fetch_book_metadata_direct(identifier_type: str, identifier_value: str) -> Dict[str, Any]:
    """Direct version of fetch_book_metadata without tool wrapper."""
    try:
        result = with_timeout(fetch_google_books_metadata, timeout=FETCH_TIMEOUT)(
            identifier_type, identifier_value
        )
        return result if result else {"error": "No book data found"}
    except Exception as e:
        return {"error": str(e)}


def generate_note_direct(source_type: str, content: str) -> Dict[str, Any]:
    """Direct version of generate_note without tool wrapper."""
    try:
        prompt = get_generate_note_prompt(source_type, content)
        resp = call_llm(prompt, json_format=True)
        return json.loads(resp.choices[0].message.content)
    except Exception as e:
        return {"error": str(e)}


def save_markdown_direct(metadata: Dict, note: Dict, source_type: str, origin: str, output_dir: str) -> Dict[str, str]:
    """Direct version of save_markdown without tool wrapper."""
    try:
        # Use existing logic from save_markdown tool
        authors = "\n".join(f"  - [[{a}]]" for a in metadata.get("authors", [DEFAULT_AUTHOR]))
        topics = "\n".join(f"  - [[{t} (MOC)]]" for t in note.get("topics", [])[:3])
        body = "\n\n".join(f"## {k}\n\n{v}" for k, v in note.get("body_sections", {}).items())
        
        timestamps = get_timestamp_metadata()
        
        md_content = f"""---
UUID: {timestamps['uuid']}
Created: {timestamps['created']}
Modified: {timestamps['modified']}
tags:
  - {MARKDOWN_TAG_BIBLIOGRAPHY}
Version: {FRONTMATTER_VERSION}
publish: {str(FRONTMATTER_PUBLISH_DEFAULT).lower()}
Authors:
{authors}
Topics:
{topics}
Origin: {origin}
---

# {metadata.get('title', DEFAULT_TITLE)}

**Summary**: {note.get('summary', DEFAULT_SUMMARY)}

**Year**: {metadata.get('year', DEFAULT_YEAR)}

**URL**: [{metadata.get('url', DEFAULT_URL_TEXT)}]({metadata.get('url', '')})

{body}
"""
        
        folder = FOLDER_MAP.get(source_type, DEFAULT_OUTPUT_FOLDER)
        folder_path = os.path.join(output_dir, folder)
        os.makedirs(folder_path, exist_ok=True)
        
        safe_title = re.sub(PATTERN_INVALID_FILENAME_CHARS_ALT, '', metadata.get('title', DEFAULT_TITLE))[:100]
        file_path = os.path.join(folder_path, f"{safe_title}.md")
        
        with open(file_path, "w", encoding="utf-8") as f:
            f.write(md_content)
        
        return {"file_path": file_path}
    except Exception as e:
        return {"error": str(e)}


def save_book_to_reading_list_direct(metadata: Dict, origin: str, output_dir: str) -> Dict[str, str]:
    """Direct version of save_book_to_reading_list without tool wrapper."""
    try:
        sanitized_origin = origin.replace("[[", "").replace("]]", "").replace("(book)", "").strip()
        file_path = os.path.join(output_dir, DEFAULT_OUTPUT_FOLDER, f"{sanitized_origin} (reading material).md")
        
        file_exists = os.path.exists(file_path)
        isbn = metadata.get("isbn")
        if file_exists and isbn:
            with open(file_path, "r", encoding="utf-8") as f:
                existing_content = f.read()
            if f"ISBN: {isbn}" in existing_content:
                return {"skipped": f"Book already in reading list (ISBN: {isbn})", "file_path": file_path}
        
        title = metadata.get('title', DEFAULT_TITLE)
        authors = ", ".join(metadata.get("authors", []))
        published = metadata.get('published_date', DEFAULT_PUBLISHED_DATE)
        main_category = metadata.get("categories", [DEFAULT_CATEGORY])[0] if metadata.get("categories") else DEFAULT_CATEGORY
        description = metadata.get('description', DEFAULT_DESCRIPTION)
        identifiers = f"ISBN: {isbn}" if isbn else "No ISBN available"
        
        book_entry = f"""
### {title}
- **Authors**: {authors}
- **Published**: {published}
- **Category**: {main_category}
- **Identifiers**: {identifiers}
- **Description**: {description}

---
"""
        
        os.makedirs(os.path.dirname(file_path), exist_ok=True)
        with open(file_path, "a", encoding="utf-8") as f:
            f.write(book_entry)
        
        return {"file_path": file_path}
    except Exception as e:
        return {"error": str(e)}


def process_reference_deterministic(
    reference: str,
    classification: Dict[str, Any],
    origin: str,
    output_dir: str
) -> Dict[str, Any]:
    """
    Fast deterministic processing without agent overhead.
    Returns result with status, or None to trigger agent fallback.
    """
    source_type = classification["source_type"]
    identifier_type = classification["identifier_type"]
    identifier_value = classification["identifier_value"]
    url = classification.get("url")
    
    try:
        # Step 1: Fetch metadata based on source type
        if source_type == "Research Paper":
            metadata = fetch_paper_metadata_direct(identifier_type, identifier_value)
        elif source_type == "Book":
            metadata = fetch_book_metadata_direct(identifier_type, identifier_value)
        else:
            # Article, Lecture, or other web content
            metadata = fetch_web_content_direct(identifier_value, url)
        
        if "error" in metadata:
            return None
        
        # Step 2: Book handling path
        if source_type == "Book":
            save_result = save_book_to_reading_list_direct(metadata, origin, output_dir)
            if "error" in save_result:
                return None
            
            return {
                "status": "success",
                "path": save_result.get("file_path"),
                "source_type": source_type
            }
        
        # Step 3: Paper/Article/Lecture handling path
        # Generate note
        content = metadata.get("content_for_note", "")
        if not content:
            return None
        
        note = generate_note_direct(source_type, content)
        if "error" in note:
            return None
        
        # Save markdown
        save_result = save_markdown_direct(metadata, note, source_type, origin, output_dir)
        if "error" in save_result:
            return None
        
        return {
            "status": "success",
            "path": save_result.get("file_path"),
            "source_type": source_type
        }
    
    except Exception as e:
        # Any exception triggers agent fallback
        return None


# Export all tools as a list for easy agent initialization
# Optimized tool list - deprecated tools removed to reduce agent overhead
TOOLS = [
    parse_references_file,
    analyze_reference,  # Combines classify + extract + validate in one LLM call
    fetch_paper_metadata,  # Now includes content_for_note field
    fetch_web_content,  # Now includes content_for_note field
    generate_note,
    fetch_book_metadata,
    save_book_to_reading_list,
    save_markdown,
]
