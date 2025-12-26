"""
LangChain tools for the research extractor agent.

Each tool wraps a function from the original pipeline with proper
schemas and docstrings for the agent to understand when to use them.
"""

import os
import re
import json
from anyio import sleep
import requests
from typing import Dict, Any
from concurrent.futures import ThreadPoolExecutor, TimeoutError as FuturesTimeoutError
from langchain.tools import tool
from dotenv import load_dotenv
from openai import OpenAI

from research_extractor_api_utils import fetch_web_content as fetch_web_api
from research_extractor_prompts import (
    get_classify_source_prompt,
    get_extract_identifier_prompt,
    get_generate_note_prompt,
)
from research_extractor_constants import (
    MODEL,
    SEMANTIC_SCHOLAR_API_URL,
    SEMANTIC_SCHOLAR_FIELDS,
    SEMANTIC_SCHOLAR_RATE_LIMIT_DELAY,
    SEMANTIC_SCHOLAR_MAX_RETRIES,
    OPENALEX_API_URL,
    FOLDER_MAP,
    FETCH_TIMEOUT,
    OPENROUTER_API_BASE,
    MIN_IDENTIFIER_LENGTH,
    MIN_WORD_CONTENT_LENGTH,
    MEANINGLESS_PATTERNS,
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
            import ast
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
    from datetime import datetime
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
        
        refs = [r.strip() for r in re.split(r"\n\s*\n", text) if r.strip()]
        if len(refs) <= 1:
            refs = [r.strip() for r in re.split(r"\.\s+\d{1,2}\.\s+", text) if r.strip()]
        
        return {"references": refs, "count": len(refs)}
    except FileNotFoundError:
        return {"error": f"File not found: {file_path}"}
    except Exception as e:
        return {"error": str(e)}


@tool
def classify_source_type(reference: str) -> Dict[str, str]:
    """Classify reference as: Research Paper|Article|Book|Lecture|Post|Quote
    
    Args: reference - Text to classify
    Returns: {source_type: str} or {error: str}
    """
    try:
        prompt = get_classify_source_prompt(reference)
        resp = call_llm(prompt, json_format=True)
        result = json.loads(resp.choices[0].message.content)
        return {"source_type": result["source_type"]}
    except Exception as e:
        return {"error": str(e)}


@tool
def extract_identifier(source_type: str, reference: str) -> Dict[str, Any]:
    """Extract DOI|CorpusID|ArXiv|Title from reference. Adds URL if present.
    
    Args: source_type, reference
    Returns: {identifier_type: str, identifier_value: str, url?: str} or {error: str}
    """
    try:
        prompt = get_extract_identifier_prompt(source_type, reference)
        resp = call_llm(prompt, json_format=True)
        identifier = json.loads(resp.choices[0].message.content)
        
        # Ensure identifier_value is never None - convert to empty string
        if identifier.get("identifier_value") is None:
            identifier["identifier_value"] = ""
        
        if source_type != "Research Paper":
            url_match = re.search(r'https?://[^\s]+', reference)
            if url_match:
                identifier["url"] = url_match.group(0)
        
        return identifier
    except Exception as e:
        return {"error": str(e)}


@tool
def validate_identifier(identifier_value: str) -> Dict[str, Any]:
    """Check if identifier is meaningful (not Ibid/loc.cit/etc, >5 chars, has words).
    
    Args: identifier_value
    Returns: {is_valid: bool, reason: str}
    """
    if not identifier_value or len(identifier_value) < MIN_IDENTIFIER_LENGTH:
        return {"is_valid": False, "reason": "Empty or too short"}
    
    for pattern in MEANINGLESS_PATTERNS:
        if re.search(pattern, identifier_value, re.IGNORECASE):
            return {"is_valid": False, "reason": "Meaningless pattern"}
    
    words = re.findall(r'[a-z]+', identifier_value, re.IGNORECASE)
    if not words or len(''.join(words)) < MIN_WORD_CONTENT_LENGTH:
        return {"is_valid": False, "reason": "Insufficient content"}
    
    return {"is_valid": True, "reason": "Valid"}


@tool
def fetch_paper_metadata(identifier_info: str) -> Dict[str, Any]:
    """Fetch paper from Semantic Scholar by DOI|CorpusID|ArXiv|Title. 30s timeout.
    Falls back to OpenAlex if Semantic Scholar fails or returns no results.
    Includes retry logic with exponential backoff for rate limits.
    
    Args: identifier_info - JSON with identifier_type and identifier_value
    Returns: {title, authors, year, abstract, tldr, url} or {error: str}
    """
    try:
        import time
        info = safe_json_parse(identifier_info)
        
        def _fetch_from_semantic_scholar():
            """Try Semantic Scholar API first."""
            id_type = info.get("identifier_type")
            id_value = info.get("identifier_value")
            
            if id_type in ["DOI", "CorpusID", "ArXiv"]:
                paper_id = f"{id_type if id_type != 'ArXiv' else 'ARXIV'}:{id_value}"
                url = f"{SEMANTIC_SCHOLAR_API_URL.replace('/search', '')}/{paper_id}"
                params = {"fields": SEMANTIC_SCHOLAR_FIELDS}
            else:
                url = SEMANTIC_SCHOLAR_API_URL
                params = {"query": id_value, "limit": 1, "fields": SEMANTIC_SCHOLAR_FIELDS}
            
            # Add API key if available for higher rate limits
            api_key = os.getenv("SEMANTIC_SCHOLAR_API_KEY")
            headers = {"x-api-key": api_key} if api_key else {}
            
            # Retry logic with exponential backoff
            for attempt in range(SEMANTIC_SCHOLAR_MAX_RETRIES):
                # Base delay to avoid rate limits
                if attempt == 0:
                    time.sleep(SEMANTIC_SCHOLAR_RATE_LIMIT_DELAY)
                
                r = requests.get(url, params=params, headers=headers, timeout=FETCH_TIMEOUT)
                
                # Handle rate limit (429)
                if r.status_code == 429:
                    if attempt < SEMANTIC_SCHOLAR_MAX_RETRIES - 1:
                        wait_time = SEMANTIC_SCHOLAR_RATE_LIMIT_DELAY * (2 ** attempt)  # Exponential backoff
                        time.sleep(wait_time)
                        continue
                    else:
                        return None  # Return None to trigger OpenAlex fallback
                
                r.raise_for_status()
                data = r.json()
                
                if data.get("error"):
                    return None  # Return None to trigger OpenAlex fallback
                
                paper = data if id_type in ["DOI", "CorpusID", "ArXiv"] else data.get("data", [None])[0]
                if not paper:
                    return None  # Return None to trigger OpenAlex fallback
                
                return {
                    "title": paper.get("title"),
                    "authors": [a["name"] for a in paper.get("authors", [])],
                    "year": paper.get("year"),
                    "abstract": paper.get("abstract"),
                    "tldr": paper.get("tldr", {}).get("text") if paper.get("tldr") else None,
                    "url": paper.get("url"),
                    "source": "Semantic Scholar"
                }
            
            return None  # Return None to trigger OpenAlex fallback
        
        def _fetch_from_openalex():
            """Fallback to OpenAlex API."""
            id_type = info.get("identifier_type")
            id_value = info.get("identifier_value")
            
            # Build OpenAlex URL
            if id_type == "DOI":
                url = f"{OPENALEX_API_URL}/doi:{id_value}"
            elif id_type == "Title":
                url = f"{OPENALEX_API_URL}?filter=title.search:{id_value}"
            else:
                # For other types, try title search
                url = f"{OPENALEX_API_URL}?filter=title.search:{id_value}"
            
            # OpenAlex recommends polite pool with email
            params = {"mailto": os.getenv("USER_EMAIL", "user@example.com")}
            
            r = requests.get(url, params=params, timeout=FETCH_TIMEOUT)
            r.raise_for_status()
            data = r.json()
            
            # Handle search results vs direct lookup
            if "results" in data:
                if not data["results"]:
                    return {"error": "No results from OpenAlex"}
                work = data["results"][0]
            else:
                work = data
            
            # Parse OpenAlex response format
            authors = [authorship["author"]["display_name"] for authorship in work.get("authorships", [])]
            
            # Reconstruct abstract from inverted index if available
            abstract = None
            if work.get("abstract_inverted_index"):
                inv_index = work["abstract_inverted_index"]
                words = [""] * (max([max(positions) for positions in inv_index.values()]) + 1)
                for word, positions in inv_index.items():
                    for pos in positions:
                        words[pos] = word
                abstract = " ".join(words)
            
            return {
                "title": work.get("title"),
                "authors": authors,
                "year": work.get("publication_year"),
                "abstract": abstract,
                "tldr": None,  # OpenAlex doesn't have TLDR
                "url": work.get("doi") or work.get("id"),
                "source": "OpenAlex"
            }
        
        def _fetch():
            # Try Semantic Scholar first
            result = _fetch_from_semantic_scholar()
            if result:
                return result
            
            # Fallback to OpenAlex
            try:
                return _fetch_from_openalex()
            except Exception as e:
                return {"error": f"Both Semantic Scholar and OpenAlex failed: {str(e)}"}
        
        return with_timeout(_fetch, timeout=FETCH_TIMEOUT)()
    except Exception as e:
        return {"error": str(e)}


@tool
def fetch_web_content(identifier_info: str) -> Dict[str, Any]:
    """Fetch article/lecture/post from web (NYT API, HTML scraping, Tavily). 30s timeout.
    
    Args: identifier_info - JSON with identifier_value and optional url
    Returns: {title, content, authors, url} or {error: str}
    """
    try:
        info = safe_json_parse(identifier_info)
        result = with_timeout(fetch_web_api, timeout=FETCH_TIMEOUT)(info)
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
def save_markdown(metadata: str, note: str, origin: str, output_dir: str) -> Dict[str, str]:
    """Save formatted markdown with YAML frontmatter to categorized folder.
    
    Args: metadata (JSON string or dict), note (JSON string or dict), origin, output_dir
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
        
        authors = "\n".join(f"  - [[{a}]]" for a in meta.get("authors", ["Unknown"]))
        topics = "\n".join(f"  - [[{t} (MOC)]]" for t in note_data.get("topics", [])[:3])
        body = "\n\n".join(f"## {k}\n\n{v}" for k, v in note_data.get("body_sections", {}).items())
        
        # Generate timestamp metadata
        timestamps = get_timestamp_metadata()
        
        md_content = f"""---
UUID: {timestamps['uuid']}
Created: {timestamps['created']}
Modified: {timestamps['modified']}
tags:
  - Type/Bibliography
Version: 1
publish: false
Authors:
{authors}
Summary: {note_data.get('summary', '')}
Year: {meta.get('year', '')}
Topic:
{topics}
Link: {meta.get('url', '')}
Title: {meta.get('title', '')}
Source-Type: {meta.get('source_type', '')}
Origin: {origin}
---

{body}
"""
        
        folder = FOLDER_MAP.get(meta.get("source_type", "Misc"), "Misc")
        safe_title = re.sub(r"[\\/:*?\"<>|]", "", meta.get("title", "Untitled"))
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
    
    Args:
        book_title: Title of the book to search for
    
    Returns:
        Dict containing:
        - title: Full book title
        - authors: List of author names
        - published_date: Publication date
        - description: Book description
        - isbn: ISBN identifiers
        - page_count: Number of pages
        - categories: Book categories/genres
        - link: Google Books link
        Or {error: str} on failure
    """
    try:
        import requests
        from research_extractor_constants import FETCH_TIMEOUT
        
        url = "https://www.googleapis.com/books/v1/volumes"
        params = {"q": book_title, "maxResults": 1}
        
        # Add API key if available (optional but recommended for higher rate limits)
        google_books_api_key = os.getenv("GOOGLE_BOOKS_API_KEY")
        if google_books_api_key:
            params["key"] = google_books_api_key.strip()
        
        response = requests.get(url, params=params, timeout=FETCH_TIMEOUT)
        response.raise_for_status()
        
        data = response.json()
        
        if "items" not in data or len(data["items"]) == 0:
            return {"error": "No books found for this title"}
        
        book = data["items"][0]["volumeInfo"]
        
        # Extract ISBN
        isbn = None
        if "industryIdentifiers" in book:
            for identifier in book["industryIdentifiers"]:
                if identifier["type"] in ["ISBN_13", "ISBN_10"]:
                    isbn = identifier["identifier"]
                    break
        
        return {
            "title": book.get("title", ""),
            "authors": book.get("authors", []),
            "published_date": book.get("publishedDate", ""),
            "description": book.get("description", ""),
            "isbn": isbn,
            "page_count": book.get("pageCount"),
            "categories": book.get("categories", []),
            "link": book.get("infoLink", ""),
        }
    except Exception as e:
        return {"error": str(e)}


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
        file_path = os.path.join(output_dir, "Misc", f"{sanitized_origin} (reading material).md")
        
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
        title = meta.get('title', 'Untitled')
        authors = ", ".join(meta.get("authors", []))
        published = meta.get('published_date', 'Unknown')
        main_category = meta.get("categories", ["Uncategorized"])[0] if meta.get("categories") else "Uncategorized"
        description = meta.get('description', 'No description available.')
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
  - Type/Bibliography
Version: 1
publish: false
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


# Export all tools as a list for easy agent initialization
TOOLS = [
    parse_references_file,
    classify_source_type,
    extract_identifier,
    validate_identifier,
    fetch_paper_metadata,
    fetch_web_content,
    prepare_content_for_note,
    generate_note,
    fetch_book_metadata,
    save_book_to_reading_list,
    save_markdown,
]
