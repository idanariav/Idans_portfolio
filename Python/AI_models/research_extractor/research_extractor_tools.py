"""
LangChain tools for the research extractor agent.

Each tool wraps a function from the original pipeline with proper
schemas and docstrings for the agent to understand when to use them.
"""

import os
import re
import json
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
    PATTERN_AUTHOR_YEAR,
    PATTERN_JOURNAL_INFO,
    PATTERN_INVALID_FILENAME_CHARS,
    PATTERN_INVALID_FILENAME_CHARS_ALT,
    ARTICLE_DOMAINS,
    VIDEO_DOMAINS,
    NARRATIVE_STARTERS,
    METHODOLOGY_MARKERS,
    CROSS_REFERENCE_PATTERNS,
    MIN_REFERENCE_LENGTH,
    DEFAULT_TITLE,
    DEFAULT_AUTHOR,
    DEFAULT_PUBLISHED_DATE,
    DEFAULT_YEAR,
    DEFAULT_SUMMARY,
    DEFAULT_URL_TEXT,
    DEFAULT_DESCRIPTION,
    DEFAULT_OUTPUT_FOLDER,
    DEFAULT_CATEGORY,
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
    """Parse JSON string or return dict as-is."""
    if isinstance(data, dict):
        return data
    try:
        return json.loads(data)
    except (json.JSONDecodeError, ValueError):
        return {"error": "Invalid JSON", "raw_data": data[:200]}


def get_timestamp_metadata() -> Dict[str, str]:
    """Generate timestamp metadata fields."""
    now = datetime.now()
    return {
        "uuid": now.strftime("%Y%m%d%H%M%S"),
        "created": now.strftime("%Y-%m-%d %H:%M"),
        "modified": now.strftime("%Y-%m-%d %H:%M"),
    }


def _generate_filename(metadata: Dict) -> str:
    """Generate standardized filename: 'Author - Title (reference)'."""
    authors = metadata.get("authors", [DEFAULT_AUTHOR])
    title = metadata.get("title", DEFAULT_TITLE)
    
    # Use first author, add "et al." if multiple authors
    if len(authors) > 1:
        author_str = f"{authors[0]} et al."
    else:
        author_str = authors[0] if authors else DEFAULT_AUTHOR
    
    # Remove invalid filename characters
    author_clean = re.sub(PATTERN_INVALID_FILENAME_CHARS, "", author_str)
    title_clean = re.sub(PATTERN_INVALID_FILENAME_CHARS, "", title)
    
    # Construct filename with truncation to avoid OS limits
    filename = f"{author_clean} - {title_clean} (reference)"
    return filename[:200]  # Keep reasonable length


def _extract_origin_from_file(file_path: str) -> list:
    """Extract existing Origin values from markdown file's YAML frontmatter."""
    try:
        with open(file_path, "r", encoding="utf-8") as f:
            content = f.read()
        
        # Extract frontmatter
        if not content.startswith("---"):
            return []
        
        # Find end of frontmatter
        end_idx = content.find("---", 3)
        if end_idx == -1:
            return []
        
        frontmatter = content[3:end_idx]
        
        # Parse Origin field (can be single line or multi-line list)
        origins = []
        in_origin_section = False
        
        for line in frontmatter.split("\n"):
            if line.startswith("Origin:"):
                in_origin_section = True
                # Check if origin is on same line (old format)
                value = line.split("Origin:", 1)[1].strip()
                if value:
                    origins.append(value)
            elif in_origin_section:
                # Check if this is a list item
                stripped = line.strip()
                if stripped.startswith("- "):
                    origins.append(stripped[2:].strip())
                elif not stripped or not stripped.startswith(" "):
                    # End of Origin section
                    break
        
        return origins
    except Exception:
        return []


def _build_markdown_content(metadata: Dict, note: Dict, source_type: str, origin: Any) -> str:
    """Shared markdown content builder."""
    authors = "\n".join(f"  - [[{a}]]" for a in metadata.get("authors", [DEFAULT_AUTHOR]))
    topics = "\n".join(f"  - [[{t} (MOC)]]" for t in note.get("topics", [])[:3])
    body = "\n\n".join(f"## {k}\n\n{v}" for k, v in note.get("body_sections", {}).items())
    timestamps = get_timestamp_metadata()
    
    # Handle origin as either string or list
    if isinstance(origin, list):
        origin_yaml = "\n".join(f"  - {o}" for o in origin)
    else:
        origin_yaml = f"  - {origin}"
    
    return f"""---
UUID: {timestamps['uuid']}
Created: {timestamps['created']}
Modified: {timestamps['modified']}
tags:
  - {MARKDOWN_TAG_BIBLIOGRAPHY}
Version: {FRONTMATTER_VERSION}
publish: {str(FRONTMATTER_PUBLISH_DEFAULT).lower()}
Authors:
{authors}
Summary: {note.get('summary', '')}
Year: {metadata.get('year', DEFAULT_YEAR)}
Topic:
{topics}
Link: {metadata.get('url', '')}
Title: {metadata.get('title', DEFAULT_TITLE)}
Source-Type: {source_type}
Origin:
{origin_yaml}
---

{body}
"""


def is_compound_reference(reference: str) -> tuple[bool, list[str]]:
    """Detect if a reference entry contains multiple citations.
    
    Args:
        reference: The reference text to check
    
    Returns:
        (is_compound, [list of extracted citations]) tuple
    """
    # Find all author-year patterns
    author_year_matches = list(re.finditer(PATTERN_AUTHOR_YEAR, reference))
    
    # Find all URLs
    url_matches = list(re.finditer(PATTERN_URL, reference))
    
    # Check for multiple URLs (strong indicator of compound reference)
    if len(url_matches) >= 2:
        return (True, split_compound_reference(reference, author_year_matches or url_matches))
    
    # Check for multiple author-year patterns
    if len(author_year_matches) < 2:
        return (False, [])
    
    # Check if they look like separate complete citations
    # (not just multiple mentions of same work)
    has_journal_info = bool(re.search(PATTERN_JOURNAL_INFO, reference))
    has_quotes = reference.count('"') >= 2  # Title in quotes
    
    # If we have 2+ author-year patterns AND journal info, likely compound
    if has_journal_info or has_quotes:
        return (True, split_compound_reference(reference, author_year_matches))
    
    return (False, [])


def split_compound_reference(reference: str, author_year_matches: list) -> list[str]:
    """Split a compound reference into individual citations using LLM.
    
    Args:
        reference: The compound reference text
        author_year_matches: List of regex match objects for author-year patterns
    
    Returns:
        List of individual citation strings
    """
    try:
        prompt = f"""<task>Split this compound reference into separate individual citations</task>

<compound_reference>
{reference}
</compound_reference>

<rules>
- This entry contains {len(author_year_matches)} citations
- Extract each complete citation (author, year, title, publication info)
- Remove narrative text ("The first paper...", "The study shows...")
- Each citation should be standalone and complete
- Preserve all bibliographic details (journal, volume, pages)
</rules>

<output_format>
JSON with key "citations" containing array of strings:
{{
  "citations": [
    "Author1 (Year1). Title1. Journal1, Volume1, Pages1.",
    "Author2 (Year2). Title2. Journal2, Volume2, Pages2.",
    ...
  ]
}}
</output_format>

Extract and return JSON:"""
        
        resp = call_llm(prompt, json_format=True)
        result = json.loads(resp.choices[0].message.content)
        citations = result.get("citations", [])
        
        # Filter out empty or too-short results
        citations = [c for c in citations if len(c.strip()) >= MIN_REFERENCE_LENGTH]
        
        return citations if citations else [reference]  # Fallback to original
    
    except Exception:
        # On error, return original reference
        return [reference]


def is_non_citation(reference: str) -> tuple[bool, str]:
    """Deterministically detect obvious non-citations to avoid wasting LLM calls.
    
    Args:
        reference: The reference text to check
    
    Returns:
        (is_invalid, reason) tuple
    """
    ref_lower = reference.lower()
    
    # Check 1: Too short
    if len(reference.strip()) < MIN_REFERENCE_LENGTH:
        return (True, "Too short (< 20 chars)")
    
    # Check 2: Cross-references (ibid, op. cit., etc.)
    for pattern in CROSS_REFERENCE_PATTERNS:
        if re.search(pattern, reference, re.IGNORECASE):
            return (True, "Cross-reference only")
    
    # Check 3: Narrative/commentary text (but allow if contains embedded citation)
    # A citation typically has a year in parentheses like (2009) or a URL
    has_year_pattern = re.search(r'\(\d{4}\)', reference)
    has_url = re.search(PATTERN_URL, reference)
    
    for pattern in NARRATIVE_STARTERS:
        if re.search(pattern, reference, re.IGNORECASE):
            # If narrative text but contains a year pattern or URL, it's likely introducing a citation
            if not has_year_pattern and not has_url:
                return (True, "Narrative text without citation")
            # Otherwise let it pass - e.g., "For comprehensive overview, see Smith (2009)..."
    
    # Check 4: Study methodology descriptions
    for pattern in METHODOLOGY_MARKERS:
        if re.search(pattern, reference, re.IGNORECASE):
            return (True, "Study description")
    
    return (False, "")


@tool
def parse_references_file(file_path: str) -> Dict[str, Any]:
    """Load and parse references from text file. Splits by double newlines or numbered lists.
    Filters out obvious non-citations deterministically and splits compound references.
    
    Args: file_path - Absolute path to text file
    Returns: {references: [list], skipped: [{ref, reason}], split: [{original, count}], valid_count: int, skipped_count: int} or {error: str}
    """
    try:
        with open(file_path, "r", encoding="utf-8") as f:
            text = f.read()
        
        # Split by double newlines or numbered lists
        raw_refs = [r.strip() for r in re.split(PATTERN_DOUBLE_NEWLINE, text) if r.strip()]
        if len(raw_refs) <= 1:
            raw_refs = [r.strip() for r in re.split(PATTERN_NUMBERED_LIST, text) if r.strip()]
        
        # Filter out obvious non-citations and split compounds
        valid_refs = []
        skipped_refs = []
        split_refs = []
        
        for ref in raw_refs:
            # Check for non-citations first
            is_invalid, reason = is_non_citation(ref)
            if is_invalid:
                skipped_refs.append({"reference": ref[:100], "reason": reason})
                continue
            
            # Check for compound references
            is_compound, citations = is_compound_reference(ref)
            if is_compound and len(citations) > 1:
                split_refs.append({"original": ref[:100], "count": len(citations)})
                valid_refs.extend(citations)
            else:
                valid_refs.append(ref)
        
        return {
            "references": valid_refs,
            "skipped": skipped_refs,
            "split": split_refs,
            "valid_count": len(valid_refs),
            "skipped_count": len(skipped_refs),
            "split_count": len(split_refs)
        }
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
        meta = safe_json_parse(metadata) if not isinstance(metadata, dict) else metadata
        note_data = safe_json_parse(note) if not isinstance(note, dict) else note
        
        if "error" in meta:
            return {"error": f"Failed to parse metadata: {meta.get('error')}"}
        if "error" in note_data:
            return {"error": f"Failed to parse note: {note_data.get('error')}"}
        
        folder = FOLDER_MAP.get(source_type, DEFAULT_OUTPUT_FOLDER)
        filename = _generate_filename(meta)
        file_path = os.path.join(output_dir, folder, f"{filename}.md")
        
        # Check if file exists and merge origins
        final_origin = origin
        if os.path.exists(file_path):
            existing_origins = _extract_origin_from_file(file_path)
            if existing_origins:
                # Merge origins, avoid duplicates
                all_origins = existing_origins + [origin]
                final_origin = list(dict.fromkeys(all_origins))  # Remove duplicates while preserving order
        
        md_content = _build_markdown_content(meta, note_data, source_type, final_origin)
        
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


def _extract_minimal_metadata(citation_text: str) -> Dict[str, Any]:
    """Core logic for extracting basic metadata from citation text."""
    prompt = f"""Extract bibliographic data from this citation:

{citation_text}

Extract: author names, year (4-digit), title, venue.
Return JSON: {{"title": str, "authors": [str], "year": str, "publication_venue": str, "summary": str, "topics": [str], "body_sections": {{}}}}"""
    
    resp = call_llm(prompt, json_format=True)
    result = json.loads(resp.choices[0].message.content)
    
    # Set defaults
    result.setdefault("title", "Untitled Citation")
    result.setdefault("authors", ["Unknown"])
    result.setdefault("year", "Unknown")
    result.setdefault("summary", "Unresolvable reference - no digital metadata available.")
    result.setdefault("topics", ["Uncategorized"])
    result.setdefault("body_sections", {"Citation": citation_text})
    result["url"] = ""
    result["content_for_note"] = citation_text
    
    return result


@tool
def create_minimal_note(citation_text: str, source_type: str) -> Dict[str, Any]:
    """Extract basic bibliographic info from citation text when no API lookup is possible.
    
    Args: citation_text, source_type
    Returns: {title, authors, year, summary, topics, body_sections} or {error}
    """
    try:
        return _extract_minimal_metadata(citation_text)
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
        
        # Check if it looks like a research paper (journal indicators)
        has_volume = re.search(r'\d+,\s*\d+', reference)  # volume, page pattern
        has_journal_indicators = re.search(r'\b(Journal|Proceedings|Conference|Bulletin|Review|Science)\b', reference, re.IGNORECASE)
        
        if has_volume or has_journal_indicators:
            # Research paper - try title search
            result.update({
                "is_obvious": True,
                "confidence": 0.75,
                "source_type": "Research Paper",
                "identifier_type": "Title",
                "identifier_value": title,
                "reason": "Academic citation with title - will try API search"
            })
        else:
            # Newspaper, magazine, or other - try title search for articles
            result.update({
                "is_obvious": True,
                "confidence": 0.70,
                "source_type": "Article",
                "identifier_type": "Title",
                "identifier_value": title,
                "reason": "Citation with title - will try search"
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
        folder = FOLDER_MAP.get(source_type, DEFAULT_OUTPUT_FOLDER)
        filename = _generate_filename(metadata)
        file_path = os.path.join(output_dir, folder, f"{filename}.md")
        
        # Check if file exists and merge origins
        final_origin = origin
        if os.path.exists(file_path):
            existing_origins = _extract_origin_from_file(file_path)
            if existing_origins:
                # Merge origins, avoid duplicates
                all_origins = existing_origins + [origin]
                final_origin = list(dict.fromkeys(all_origins))  # Remove duplicates while preserving order
        
        md_content = _build_markdown_content(metadata, note, source_type, final_origin)
        
        os.makedirs(os.path.dirname(file_path), exist_ok=True)
        with open(file_path, "w", encoding="utf-8") as f:
            f.write(md_content)
        
        return {"file_path": file_path}
    except Exception as e:
        return {"error": str(e)}


def create_minimal_note_direct(citation_text: str, source_type: str) -> Dict[str, Any]:
    """Direct version of create_minimal_note without tool wrapper."""
    try:
        return _extract_minimal_metadata(citation_text)
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
            
            # If title search fails, fallback to minimal note
            if metadata and "error" in metadata and identifier_type == "Title":
                metadata = create_minimal_note_direct(reference, "Unresolvable")
                source_type = "Unresolvable"
        
        elif source_type == "Article" and identifier_type == "Title":
            # Try paper search first (some articles are in academic DBs)
            metadata = fetch_paper_metadata_direct(identifier_type, identifier_value)
            
            # If not found, fallback to minimal note
            if metadata and "error" in metadata:
                metadata = create_minimal_note_direct(reference, "Unresolvable")
                source_type = "Unresolvable"
        
        elif source_type == "Book":
            metadata = fetch_book_metadata_direct(identifier_type, identifier_value)
        
        elif source_type == "Unresolvable":
            # Extract basic info from citation text only
            metadata = create_minimal_note_direct(reference, source_type)
        
        else:
            # Article, Lecture, or other web content with URL
            metadata = fetch_web_content_direct(identifier_value, url)
        
        if "error" in metadata and source_type != "Unresolvable":
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
        
        # Step 3: Unresolvable path - save directly without generate_note
        if source_type == "Unresolvable":
            # metadata already has note structure from create_minimal_note_direct
            note = {
                "summary": metadata.get("summary", ""),
                "topics": metadata.get("topics", []),
                "body_sections": metadata.get("body_sections", {})
            }
            save_result = save_markdown_direct(metadata, note, source_type, origin, output_dir)
            if "error" in save_result:
                return None
            
            return {
                "status": "success",
                "path": save_result.get("file_path"),
                "source_type": source_type,
                "note": "Fallback: Title search failed, created minimal note"
            }
        
        # Step 4: Paper/Article/Lecture handling path
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


# Export tools for agent - optimized list
TOOLS = [
    parse_references_file,
    analyze_reference,
    fetch_paper_metadata,
    fetch_web_content,
    generate_note,
    fetch_book_metadata,
    save_book_to_reading_list,
    create_minimal_note,
    save_markdown,
]
