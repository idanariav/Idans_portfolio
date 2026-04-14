"""
LangChain tools for the research extractor agent.

Each tool wraps a function from the original pipeline with proper
schemas and docstrings for the agent to understand when to use them.
"""

import os
import re
import json
import uuid
from typing import Dict, Any, Optional, List, Tuple, Literal
from datetime import datetime
from concurrent.futures import ThreadPoolExecutor, TimeoutError as FuturesTimeoutError, as_completed
from difflib import SequenceMatcher
from langchain.tools import tool
from openai import OpenAI
from pydantic import BaseModel, Field, field_validator, model_validator

from research_extractor_api_utils import (
    fetch_web_api,
    fetch_semantic_scholar_metadata,
    fetch_openalex_metadata,
    fetch_google_books_metadata,
    clean_identifier,
)
from research_extractor_prompts import (
    get_analyze_reference_prompt,
    get_batch_analyze_references_prompt,
    get_batch_extract_minimal_metadata_prompt,
    get_generate_note_prompt,
)
from research_extractor_constants import (
    MODEL,
    FOLDER_MAP,
    RARE_TYPE_FOLDER,
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
    ARTICLE_DOMAINS,
    VIDEO_DOMAINS,
    NARRATIVE_STARTERS,
    METHODOLOGY_MARKERS,
    CROSS_REFERENCE_PATTERNS,
    QUOTE_PATTERNS,
    SECTION_HEADER_PATTERNS,
    BIOGRAPHICAL_PATTERNS,
    DERIVATION_PATTERNS,
    MIN_REFERENCE_LENGTH,
    DEFAULT_TITLE,
    DEFAULT_AUTHOR,
    DEFAULT_PUBLISHED_DATE,
    DEFAULT_YEAR,
    DEFAULT_DESCRIPTION,
    DEFAULT_OUTPUT_FOLDER,
    DEFAULT_CATEGORY,
    MARKDOWN_TAG_BIBLIOGRAPHY,
    FRONTMATTER_VERSION,
    FRONTMATTER_PUBLISH_DEFAULT,
    # File-level screening constants
    MIN_SUBSTANTIVE_LINES,
    MIN_CITATION_RATIO,
    # Content pre-processing patterns
    PATTERN_MARKDOWN_IMAGE_LINE,
    PATTERN_EBOOK_NAV_LINK_LINE,
    PATTERN_HORIZONTAL_RULE,
    PATTERN_SECTION_HEADER_LINE,
    # Phase 2 non-citation patterns
    MARKDOWN_HEADER_PATTERNS,
    SCRIPTURE_PATTERNS,
    COLLABORATOR_PATTERNS,
    MARKETING_PATTERNS,
    VAGUE_REFERENCE_PATTERNS,
)

# ============================================================================
# Pydantic Models for LLM Response Validation
# ============================================================================

SourceTypeValue = Literal[
    "Book", "Research Paper", "Article", "Other", "Unresolvable", "Invalid"
]
IdentifierTypeValue = Literal[
    "DOI", "arXiv", "CorpusID", "URL", "ISBN", "Title", "CitationText", "None"
]
ConfidenceLevelValue = Literal["high", "medium", "low"]

CONFIDENCE_MAP: Dict[str, float] = {"high": 0.90, "medium": 0.75, "low": 0.60}


_IDENTIFIER_TYPE_ALIASES: Dict[str, str] = {
    "Title + Author": "CitationText",
    "Author": "CitationText",
    "Title + Year": "Title",
}

class ReferenceClassificationResponse(BaseModel):
    """Validates a single reference classification from the LLM."""
    source_type: SourceTypeValue = "Unresolvable"
    identifier_type: IdentifierTypeValue = "Title"
    identifier_value: str = ""
    is_valid: bool = True
    validation_reason: str = ""
    confidence: ConfidenceLevelValue = "medium"
    rationale: str = ""

    @model_validator(mode="before")
    @classmethod
    def sanitize_llm_output(cls, data: Any) -> Any:
        """Coerce None values and normalize LLM-invented field values."""
        if not isinstance(data, dict):
            return data
        # Coerce None to defaults for all string fields
        _str_defaults = {
            "identifier_value": "",
            "validation_reason": "",
            "rationale": "",
        }
        for field, default in _str_defaults.items():
            if field in data and data[field] is None:
                data[field] = default
        # Normalize identifier_type: None, "null", or aliased values
        id_type = data.get("identifier_type")
        if id_type is None or id_type == "null":
            data["identifier_type"] = "Title"
        elif isinstance(id_type, str) and id_type in _IDENTIFIER_TYPE_ALIASES:
            data["identifier_type"] = _IDENTIFIER_TYPE_ALIASES[id_type]
        return data

    def to_classification_dict(self) -> Optional[Dict[str, Any]]:
        """Convert to the internal classification dict, or None if invalid."""
        if not self.is_valid:
            return None
        return {
            "is_obvious": True,
            "confidence": CONFIDENCE_MAP.get(self.confidence, 0.75),
            "source_type": self.source_type,
            "identifier_type": self.identifier_type,
            "identifier_value": self.identifier_value,
            "url": None,
            "reason": self.rationale or "LLM classification",
        }


class BatchClassificationResponse(BaseModel):
    """Validates a batch classification response from the LLM."""
    classifications: Dict[str, ReferenceClassificationResponse]


class MinimalMetadataResponse(BaseModel):
    """Validates a single minimal metadata extraction from the LLM."""
    title: str = "Untitled Citation"
    authors: List[str] = Field(default_factory=lambda: ["Unknown"])
    year: str = "Unknown"
    publication_venue: str = ""
    summary: str = "Unresolvable reference - no digital metadata available."
    topics: List[str] = Field(default_factory=lambda: ["Uncategorized"])

    @model_validator(mode="before")
    @classmethod
    def sanitize_llm_output(cls, data: Any) -> Any:
        """Coerce None values to defaults for string fields."""
        if not isinstance(data, dict):
            return data
        _str_defaults = {
            "title": "Untitled Citation",
            "year": "Unknown",
            "publication_venue": "",
            "summary": "Unresolvable reference - no digital metadata available.",
        }
        for field, default in _str_defaults.items():
            if field in data and data[field] is None:
                data[field] = default
        return data

    def to_metadata_dict(self, citation_text: str) -> Dict[str, Any]:
        """Convert to the internal metadata dict format."""
        return {
            "title": self.title,
            "authors": self.authors,
            "year": self.year,
            "publication_venue": self.publication_venue,
            "summary": self.summary,
            "topics": self.topics,
            "body_sections": {"Citation": citation_text},
            "url": "",
            "content_for_note": citation_text,
        }


class BatchMinimalMetadataResponse(BaseModel):
    """Validates a batch minimal metadata extraction response from the LLM."""
    extractions: Dict[str, MinimalMetadataResponse]


# Initialize OpenAI client
client = OpenAI(
    api_key=os.getenv("OPENROUTER_API_KEY"),
    base_url=OPENROUTER_API_BASE,
)


def _repair_json(text: str) -> str:
    """Best-effort repair of common LLM JSON issues before parsing."""
    # Strip markdown code fences
    text = text.strip()
    if text.startswith("```"):
        text = re.sub(r'^```(?:json)?\s*', '', text)
        text = re.sub(r'\s*```$', '', text)
    # Remove trailing commas before } or ]
    text = re.sub(r',\s*([}\]])', r'\1', text)
    return text


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
        "uuid": str(uuid.uuid4()),
        "created": now.strftime("%Y-%m-%d %H:%M"),
        "modified": now.strftime("%Y-%m-%d %H:%M"),
    }


def _generate_filename(metadata: Dict) -> str:
    """Generate standardized filename: 'Title (reference)'."""
    title = metadata.get("title", DEFAULT_TITLE)
    title_clean = re.sub(PATTERN_INVALID_FILENAME_CHARS, "", title)
    filename = f"{title_clean} (reference)"
    return filename[:200]  # Keep reasonable length


def _extract_origin_from_file(file_path: str) -> list:
    """Extract existing Origin values from markdown file's YAML frontmatter."""
    try:
        import yaml

        with open(file_path, "r", encoding="utf-8") as f:
            content = f.read()

        if not content.startswith("---"):
            return []

        end_idx = content.find("---", 3)
        if end_idx == -1:
            return []

        frontmatter_str = content[3:end_idx]
        frontmatter = yaml.safe_load(frontmatter_str)
        if not isinstance(frontmatter, dict):
            return []

        origin = frontmatter.get("Origin", [])
        if isinstance(origin, list):
            return origin
        if isinstance(origin, str):
            return [origin]
        return []
    except Exception:
        return []


def _format_topic_wikilink(topic: str, topic_info: Optional[Dict[str, List[str]]] = None) -> str:
    """Format a topic name as a wikilink with correct suffix.

    Args:
        topic: The topic name (e.g., "Ethics", "Game Theory", "Uncategorized")
        topic_info: Optional dict with 'maps', 'concepts', 'approved' lists.
            When provided, determines the wikilink format based on type.
            When None, defaults to (Map) suffix for backward compatibility.
    """
    if topic == "Uncategorized":
        return f'  - "[[Uncategorized]]"'
    if topic_info is None:
        return f'  - "[[{topic} (Map)]]"'
    if topic in topic_info.get("concepts", []):
        return f'  - "[[{topic}]]"'
    # Maps and approved topics get (Map) suffix
    return f'  - "[[{topic} (Map)]]"'


def _build_markdown_content(metadata: Dict, note: Dict, source_type: str, origin: Any,
                            topic_info: Optional[Dict[str, List[str]]] = None) -> str:
    """Shared markdown content builder."""
    authors = "\n".join(f'  - "[[{a}]]"' for a in metadata.get("authors", [DEFAULT_AUTHOR]))
    topics = "\n".join(
        _format_topic_wikilink(t, topic_info) for t in note.get("topics", [])[:3]
    )
    body = "\n\n".join(f"## {k}\n\n{v}" for k, v in note.get("body_sections", {}).items())
    timestamps = get_timestamp_metadata()

    # Handle origin as either string or list
    if isinstance(origin, list):
        origin_yaml = "\n".join(f'  - "{o}"' for o in origin)
    else:
        origin_yaml = f'  - "{origin}"'
    
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


def clean_raw_text(text: str) -> str:
    """Remove non-reference structural lines before splitting into entries.

    Strips standalone images, e-book navigation links, horizontal rules,
    and section headers that would otherwise become spurious entries.
    Only removes complete lines matching structural patterns; embedded
    content within reference paragraphs is untouched.
    """
    clean_lines = []
    for line in text.split('\n'):
        # Skip standalone markdown image lines
        if re.match(PATTERN_MARKDOWN_IMAGE_LINE, line):
            continue
        # Skip standalone e-book navigation links
        if re.match(PATTERN_EBOOK_NAV_LINK_LINE, line):
            continue
        # Skip horizontal rules
        if re.match(PATTERN_HORIZONTAL_RULE, line):
            continue
        # Skip standalone section header lines
        if re.match(PATTERN_SECTION_HEADER_LINE, line):
            continue
        clean_lines.append(line)
    return '\n'.join(clean_lines)


def screen_file_content(text: str) -> tuple[bool, str]:
    """Check if a file has enough extractable references to be worth processing.

    Performs cheap checks before any per-reference processing:
    1. Minimum substantive line count (catches empty/near-empty files)
    2. Citation density ratio (catches fiction/narrative-only files)

    Args:
        text: Full file text content

    Returns:
        (should_skip, reason) tuple. If should_skip is True, the file
        should not be processed further.
    """
    # Check 1: Count substantive lines (not blank, not headers, not images, not rules)
    substantive_count = 0
    for line in text.split('\n'):
        stripped = line.strip()
        if not stripped:
            continue
        if re.match(r'^#{1,6}\s+', stripped):
            continue
        if re.match(PATTERN_MARKDOWN_IMAGE_LINE, line):
            continue
        if re.match(PATTERN_HORIZONTAL_RULE, line):
            continue
        substantive_count += 1

    if substantive_count < MIN_SUBSTANTIVE_LINES:
        return (True, f"File has too few substantive lines ({substantive_count} < {MIN_SUBSTANTIVE_LINES})")

    # Check 2: Citation density - split by double newlines and count entries with signals
    entries = [e.strip() for e in re.split(PATTERN_DOUBLE_NEWLINE, text) if e.strip()]
    if not entries:
        return (True, "File has no parseable entries")

    citation_signals = 0
    for entry in entries:
        has_signal = (
            re.search(r'\b(?:19|20)\d{2}\b', entry) or  # Any year 1900-2099
            re.search(PATTERN_DOI, entry) or             # DOI
            re.search(PATTERN_URL, entry) or             # URL
            re.search(PATTERN_ISBN, entry, re.IGNORECASE) or  # ISBN
            re.search(PATTERN_ARXIV, entry, re.IGNORECASE) or  # arXiv
            re.search(r'_[A-Z][^_]{3,}_', entry)        # Italic title (_Title_)
        )
        if has_signal:
            citation_signals += 1

    ratio = citation_signals / len(entries)
    if ratio < MIN_CITATION_RATIO:
        return (True, f"File has low citation density ({citation_signals}/{len(entries)} = {ratio:.1%} < {MIN_CITATION_RATIO:.0%})")

    return (False, "")


def is_non_citation(reference: str) -> tuple[bool, str]:
    """Deterministically detect obvious non-citations to avoid wasting LLM calls.

    Uses a layered safety approach:
    - Strong citation signals (DOI, ISBN, arXiv) bypass ALL checks unconditionally
    - Structural patterns (images, rules, headers) are unconditionally filtered
    - Content-based patterns are guarded by year/URL presence checks

    Args:
        reference: The reference text to check

    Returns:
        (is_invalid, reason) tuple
    """
    ref_lower = reference.lower()

    # Safety signals: presence of these means likely a real citation
    has_year_pattern = re.search(r'\(\d{4}\)', reference)
    has_url = re.search(PATTERN_URL, reference)
    has_doi = re.search(PATTERN_DOI, reference)
    has_isbn = re.search(PATTERN_ISBN, reference, re.IGNORECASE)
    has_arxiv = re.search(PATTERN_ARXIV, reference, re.IGNORECASE)

    # Strong signal early exit: DOI, ISBN, or arXiv -> always valid
    if has_doi or has_isbn or has_arxiv:
        return (False, "")

    # Check 1: Too short
    if len(reference.strip()) < MIN_REFERENCE_LENGTH:
        return (True, "Too short (< 20 chars)")

    # Check 2: Horizontal rules (structural, never citations)
    if re.match(PATTERN_HORIZONTAL_RULE, reference):
        return (True, "Horizontal rule")

    # Check 3: Markdown headers (structural, never citations)
    for pattern in MARKDOWN_HEADER_PATTERNS:
        if re.search(pattern, reference):
            return (True, "Chapter or section header")

    # Check 4: Cross-references (ibid, op. cit., etc.)
    for pattern in CROSS_REFERENCE_PATTERNS:
        if re.search(pattern, reference, re.IGNORECASE):
            return (True, "Cross-reference only")

    # Check 5: Bible/scripture references (guarded)
    for pattern in SCRIPTURE_PATTERNS:
        if re.search(pattern, reference, re.IGNORECASE):
            if not has_year_pattern and not has_url:
                return (True, "Scripture reference (not academic citation)")

    # Check 6: Marketing/promotional content (guarded)
    for pattern in MARKETING_PATTERNS:
        if re.search(pattern, reference, re.IGNORECASE):
            if not has_year_pattern and not has_url:
                return (True, "Marketing or promotional content")

    # Check 7: Narrative/commentary text (guarded)
    for pattern in NARRATIVE_STARTERS:
        if re.search(pattern, reference, re.IGNORECASE):
            if not has_year_pattern and not has_url:
                return (True, "Narrative text without citation")

    # Check 8: Study methodology descriptions
    for pattern in METHODOLOGY_MARKERS:
        if re.search(pattern, reference, re.IGNORECASE):
            return (True, "Study description")

    # Check 9: Standalone quotes (Phase 1 improvements)
    for pattern in QUOTE_PATTERNS:
        if re.search(pattern, reference):
            return (True, "Standalone quote excerpt")

    # Check 10: Section/list headers
    for pattern in SECTION_HEADER_PATTERNS:
        if re.search(pattern, reference):
            return (True, "Section or list header")

    # Check 11: Collaborator/acknowledgment notes (guarded)
    for pattern in COLLABORATOR_PATTERNS:
        if re.search(pattern, reference, re.IGNORECASE):
            if not has_year_pattern and not has_url:
                return (True, "Collaborator acknowledgment without citation")

    # Check 12: Vague references without specific publications (guarded)
    for pattern in VAGUE_REFERENCE_PATTERNS:
        if re.search(pattern, reference, re.IGNORECASE):
            if not has_year_pattern and not has_url:
                return (True, "Vague reference without specific publication")

    # Check 13: Biographical narrative
    for pattern in BIOGRAPHICAL_PATTERNS:
        if re.search(pattern, reference, re.IGNORECASE):
            return (True, "Biographical narrative")

    # Check 14: Mathematical derivations (guarded)
    for pattern in DERIVATION_PATTERNS:
        if re.search(pattern, reference, re.IGNORECASE):
            if not has_year_pattern and not has_url:
                return (True, "Mathematical derivation")

    return (False, "")


def parse_references_from_text(text: str) -> Dict[str, Any]:
    """Core function to parse references from text content.

    Cleans structural artifacts, splits by double newlines or numbered lists,
    filters non-citations, and splits compound references.

    Args:
        text: Raw text content containing references

    Returns:
        Dict with references, skipped, split info, and counts
    """
    # Clean structural artifacts before splitting
    text = clean_raw_text(text)

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
        "raw_count": len(raw_refs),
        "valid_count": len(valid_refs),
        "skipped_count": len(skipped_refs),
        "split_count": len(split_refs)
    }


def extract_identifiers_from_text(text: str) -> List[Dict[str, str]]:
    """Extract all unique DOI and arXiv identifiers from raw text.

    Scans the entire document for identifier patterns regardless of
    reference splitting. Returns deduplicated list ordered by first
    appearance in the text.

    Args:
        text: Full document text

    Returns:
        List of dicts with identifier_type, identifier_value, source_type
    """
    identifiers = []
    seen: set = set()

    for match in re.finditer(PATTERN_DOI, text):
        doi = clean_identifier(match.group(1))
        if doi not in seen:
            seen.add(doi)
            identifiers.append({
                "identifier_type": "DOI",
                "identifier_value": doi,
                "source_type": "Research Paper",
            })

    for match in re.finditer(PATTERN_ARXIV, text, re.IGNORECASE):
        arxiv_id = match.group(1)
        key = f"arXiv:{arxiv_id}"
        if key not in seen:
            seen.add(key)
            identifiers.append({
                "identifier_type": "ArXiv",
                "identifier_value": key,
                "source_type": "Research Paper",
            })

    return identifiers


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
        
        return parse_references_from_text(text)
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


# ============================================================================
# Core Functions (used by both tools and direct calls)
# ============================================================================

def _fetch_paper_metadata_core(id_type: str, id_value: str) -> Dict[str, Any]:
    """Core logic for fetching paper metadata from Semantic Scholar/OpenAlex."""
    try:
        def _fetch():
            result = fetch_semantic_scholar_metadata(id_type, id_value)
            if result:
                return result
            return fetch_openalex_metadata(id_type, id_value)
        
        metadata = with_timeout(_fetch, timeout=FETCH_TIMEOUT)()
        
        if metadata and "error" not in metadata:
            abstract = metadata.get("abstract", "")
            tldr = metadata.get("tldr", "")
            metadata["content_for_note"] = f"{tldr}\n\n{abstract}".strip() if tldr and abstract else (tldr or abstract or "")
        
        return metadata
    except Exception as e:
        return {"error": str(e)}


def _fetch_web_content_core(identifier_value: str, url: str = None) -> Dict[str, Any]:
    """Core logic for fetching web content."""
    try:
        info = {"identifier_value": identifier_value}
        if url:
            info["url"] = url
        
        # fetch_web_api already manages its own thread pool and timeout via as_completed
        result = fetch_web_api(info)
        
        if result and "error" not in result:
            result["content_for_note"] = result.get("content") or result.get("abstract", "")
            return result

        return None
    except Exception:
        return None


def _generate_note_core(source_type: str, content: str,
                        allowed_topics: Optional[List[str]] = None) -> Dict[str, Any]:
    """Core logic for generating structured notes."""
    try:
        prompt = get_generate_note_prompt(source_type, content, allowed_topics=allowed_topics)
        resp = call_llm(prompt, json_format=True)
        result = json.loads(resp.choices[0].message.content)
        # Validate topics against allowed list
        if allowed_topics and "topics" in result:
            result["topics"] = [
                t if t in allowed_topics else "Uncategorized"
                for t in result["topics"]
            ]
        return result
    except Exception as e:
        return {"error": str(e)}


def _save_markdown_core(metadata: Dict, note: Dict, source_type: str, origin: str, output_dir: str,
                        unresolvable_dir: Optional[str] = None,
                        rare_types_dir: Optional[str] = None,
                        topic_info: Optional[Dict[str, List[str]]] = None) -> Dict[str, str]:
    """Core logic for saving markdown files. Appends Origin to existing files instead of overwriting."""
    try:
        folder = FOLDER_MAP.get(source_type, DEFAULT_OUTPUT_FOLDER)
        filename = _generate_filename(metadata)

        # Route Unresolvable to separate directory if configured
        if source_type == "Unresolvable" and unresolvable_dir:
            file_path = os.path.join(unresolvable_dir, f"{filename}.md")
        # Route rare types (Post, Quote) to inbox if configured
        elif source_type in ("Post", "Quote") and rare_types_dir:
            file_path = os.path.join(rare_types_dir, f"{filename}.md")
        else:
            file_path = os.path.join(output_dir, folder, f"{filename}.md")

        if os.path.exists(file_path):
            # File already exists - only append the new origin to avoid destroying manual edits
            existing_origins = _extract_origin_from_file(file_path)
            if origin in existing_origins:
                return {"file_path": file_path, "note": "Already exists with same origin"}
            # Append new origin to frontmatter
            with open(file_path, "r", encoding="utf-8") as f:
                content = f.read()
            # Insert new origin line before the closing ---
            origin_line = f'  - "{origin}"'
            # Find the Origin section and append
            if "Origin:" in content:
                content = content.replace("\n---\n", f"\n{origin_line}\n---\n", 1)
                with open(file_path, "w", encoding="utf-8") as f:
                    f.write(content)
            return {"file_path": file_path, "note": "Appended origin to existing file"}

        final_origin = origin
        md_content = _build_markdown_content(metadata, note, source_type, final_origin, topic_info=topic_info)

        os.makedirs(os.path.dirname(file_path), exist_ok=True)
        with open(file_path, "w", encoding="utf-8") as f:
            f.write(md_content)

        return {"file_path": file_path}
    except Exception as e:
        return {"error": str(e)}


def _save_book_to_reading_list_core(metadata: Dict, origin: str, output_dir: str) -> Dict[str, str]:
    """Core logic for saving books to reading list."""
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
        if metadata.get('page_count'):
            identifiers += f" | Pages: {metadata.get('page_count')}"
        
        book_entry = f"""## {title}
**Author:** {authors}  
**Publish Date:** {published}  
**Category:** {main_category}  
**Identifiers:** {identifiers}  

**Description:** {description}

---

"""
        
        os.makedirs(os.path.dirname(file_path), exist_ok=True)
        
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
        
        with open(file_path, "a", encoding="utf-8") as f:
            f.write(book_entry)
        
        return {"file_path": file_path}
    except Exception as e:
        return {"error": str(e)}


# ============================================================================
# Tool Wrappers (call core functions)
# ============================================================================

@tool
def fetch_paper_metadata(identifier_info: str) -> Dict[str, Any]:
    """Fetch paper metadata from Semantic Scholar/OpenAlex. Includes prepared content for note generation.
    
    Args: identifier_info - JSON with identifier_type and identifier_value
    Returns: {title, authors, year, abstract, tldr, url, source, content_for_note} or {error: str}
    """
    info = safe_json_parse(identifier_info)
    return _fetch_paper_metadata_core(info.get("identifier_type"), info.get("identifier_value"))


@tool
def fetch_web_content(identifier_info: str) -> Dict[str, Any]:
    """Fetch article/lecture/post from web (NYT API, HTML scraping, Tavily). Includes prepared content for note generation.
    
    Args: identifier_info - JSON with identifier_value and optional url
    Returns: {title, content, authors, url, content_for_note} or {error: str}
    """
    info = safe_json_parse(identifier_info)
    return _fetch_web_content_core(info.get("identifier_value"), info.get("url"))


@tool
def generate_note(source_type: str, content: str) -> Dict[str, Any]:
    """Generate structured note: summary, topics, body_sections.
    
    Args: source_type, content
    Returns: {summary: str, topics: [str], body_sections: {}} or {error: str}
    """
    return _generate_note_core(source_type, content)


@tool
def save_markdown(metadata: str, note: str, source_type: str, origin: str, output_dir: str) -> Dict[str, str]:
    """Save formatted markdown with YAML frontmatter to categorized folder.
    
    Args: metadata (JSON string or dict), note (JSON string or dict), source_type, origin, output_dir
    Returns: {file_path: str} or {error: str}
    """
    meta = safe_json_parse(metadata) if not isinstance(metadata, dict) else metadata
    note_data = safe_json_parse(note) if not isinstance(note, dict) else note
    
    if "error" in meta:
        return {"error": f"Failed to parse metadata: {meta.get('error')}"}
    if "error" in note_data:
        return {"error": f"Failed to parse note: {note_data.get('error')}"}
    
    return _save_markdown_core(meta, note_data, source_type, origin, output_dir)


@tool
def fetch_book_metadata(book_title: str) -> Dict[str, Any]:
    """Fetch book metadata from Google Books API.
    
    Args: book_title - Title of the book
    Returns: {title, authors, published_date, description, isbn, page_count, categories, link} or {error}
    """
    return fetch_google_books_metadata(book_title)


def _extract_minimal_metadata(citation_text: str, allowed_topics: Optional[List[str]] = None) -> Dict[str, Any]:
    """Core logic for extracting basic metadata from citation text."""
    if allowed_topics:
        topic_instruction = f'topics (select 1-3 from ONLY this list: {", ".join(allowed_topics)}; use "Uncategorized" if none fit)'
    else:
        topic_instruction = "topics"

    prompt = f"""Extract bibliographic data from this citation:

{citation_text}

Extract: author names, year (4-digit), title, venue, {topic_instruction}.
Return JSON: {{"title": str, "authors": [str], "year": str, "publication_venue": str, "summary": str, "topics": [str]}}"""

    resp = call_llm(prompt, json_format=True)
    raw = json.loads(resp.choices[0].message.content)
    result = MinimalMetadataResponse.model_validate(raw)
    metadata = result.to_metadata_dict(citation_text)
    # Validate topics against allowed list
    if allowed_topics and "topics" in metadata:
        metadata["topics"] = [
            t if t in allowed_topics else "Uncategorized"
            for t in metadata["topics"]
        ]
    return metadata


def extract_minimal_metadata_batch(
    citations: List[str],
    allowed_topics: Optional[List[str]] = None,
) -> Dict[int, Optional[Dict[str, Any]]]:
    """
    Batch extraction of minimal metadata from multiple citations in a single LLM call.

    Args:
        citations: List of citation text strings
        allowed_topics: Optional list of allowed topic names for constrained topic assignment.

    Returns:
        Dict mapping 0-based index to metadata dict.
        Returns empty dict on total failure (triggers per-reference fallback).
    """
    try:
        prompt = get_batch_extract_minimal_metadata_prompt(citations, allowed_topics=allowed_topics)
        resp = call_llm(prompt, json_format=True)
        raw = json.loads(_repair_json(resp.choices[0].message.content))
        batch_result = BatchMinimalMetadataResponse.model_validate(raw)

        output: Dict[int, Dict[str, Any]] = {}
        for i, citation_text in enumerate(citations):
            key = str(i)
            if key not in batch_result.extractions:
                output[i] = None
                continue
            metadata = batch_result.extractions[key].to_metadata_dict(citation_text)
            # Validate topics against allowed list
            if allowed_topics and "topics" in metadata:
                metadata["topics"] = [
                    t if t in allowed_topics else "Uncategorized"
                    for t in metadata["topics"]
                ]
            output[i] = metadata

        return output

    except Exception as e:
        print(f"⚠️  Batch minimal metadata extraction error: {e}")
        return {}


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
    meta = safe_json_parse(book_metadata)
    if "error" in meta:
        return {"error": f"Invalid metadata: {meta['error']}"}
    return _save_book_to_reading_list_core(meta, origin, output_dir)


# ============================================================================
# HYBRID MODE: Fast Deterministic Classification
# ============================================================================

def _make_classification(
    source_type: str,
    identifier_type: str,
    identifier_value: str,
    confidence: float,
    reason: str,
    url: str = None
) -> Dict[str, Any]:
    """Helper to create a classification result dict."""
    return {
        "is_obvious": True,
        "confidence": confidence,
        "source_type": source_type,
        "identifier_type": identifier_type,
        "identifier_value": identifier_value,
        "url": url,
        "reason": reason
    }


def _try_doi_pattern(reference: str) -> Optional[Dict[str, Any]]:
    """Try to match DOI pattern."""
    match = re.search(PATTERN_DOI, reference)
    if match:
        doi = clean_identifier(match.group(1))
        return _make_classification(
            "Research Paper", "DOI", doi, 0.95, "Clear DOI pattern"
        )
    return None


def _try_arxiv_pattern(reference: str) -> Optional[Dict[str, Any]]:
    """Try to match arXiv ID pattern."""
    match = re.search(PATTERN_ARXIV, reference, re.IGNORECASE)
    if match:
        arxiv_id = clean_identifier(match.group(1))
        return _make_classification(
            "Research Paper", "ArXiv", f"arXiv:{arxiv_id}", 0.95, "Clear ArXiv ID"
        )
    return None


def _try_corpus_id_pattern(reference: str) -> Optional[Dict[str, Any]]:
    """Try to match Semantic Scholar CorpusID."""
    if "semanticscholar.org" in reference.lower() or "corpusid" in reference.lower():
        match = re.search(PATTERN_CORPUSID, reference)
        if match:
            corpus_id = clean_identifier(match.group(1))
            return _make_classification(
                "Research Paper", "CorpusID", corpus_id, 0.90, "Clear CorpusID"
            )
    return None


def _try_isbn_pattern(reference: str) -> Optional[Dict[str, Any]]:
    """Try to match ISBN pattern."""
    match = re.search(PATTERN_ISBN, reference, re.IGNORECASE)
    if match:
        return _make_classification(
            "Book", "ISBN", match.group(1), 0.90, "Clear ISBN"
        )
    return None


def _try_url_pattern(reference: str) -> Optional[Dict[str, Any]]:
    """Try to match URL patterns and classify by domain."""
    match = re.search(PATTERN_URL, reference)
    if not match:
        return None
    
    domain = match.group(1).lower()
    url_full = match.group(0)
    
    if any(d in domain for d in ARTICLE_DOMAINS):
        return _make_classification(
            "Article", "URL", url_full, 0.85, "Recognized news/article domain", url_full
        )
    
    if any(d in domain for d in VIDEO_DOMAINS):
        return _make_classification(
            "Lecture", "URL", url_full, 0.85, "Video platform URL", url_full
        )
    
    return None


def _try_citation_pattern(reference: str) -> Optional[Dict[str, Any]]:
    """Try to match standard citation format (Author (Year). Title)."""
    match = re.search(PATTERN_CITATION, reference)
    if not match or len(match.group(3)) <= 20:
        return None
    
    title = match.group(3).split('.')[0].strip()
    
    # Check for academic indicators
    has_volume = re.search(r'\d+,\s*\d+', reference)
    has_journal = re.search(
        r'\b(Journal|Proceedings|Conference|Bulletin|Review|Science)\b',
        reference, re.IGNORECASE
    )
    
    if has_volume or has_journal:
        return _make_classification(
            "Research Paper", "Title", title, 0.85,
            "Academic citation with title - will try API search"
        )

    return _make_classification(
        "Article", "Title", title, 0.80, "Citation with title - will try search"
    )


def classify_reference_fast(reference: str) -> Dict[str, Any]:
    """
    Fast deterministic classification using regex patterns.
    Avoids LLM calls for obvious references.
    
    Returns:
        Dict with is_obvious, confidence, source_type, identifier_type,
        identifier_value, url, and reason fields.
    """
    # Try each pattern in priority order
    for pattern_fn in [
        _try_doi_pattern,
        _try_arxiv_pattern,
        _try_corpus_id_pattern,
        _try_isbn_pattern,
        _try_url_pattern,
        _try_citation_pattern,
    ]:
        result = pattern_fn(reference)
        if result:
            return result
    
    # No pattern matched - needs LLM
    return {
        "is_obvious": False,
        "confidence": 0.0,
        "source_type": None,
        "identifier_type": None,
        "identifier_value": None,
        "url": None,
        "reason": "No clear pattern - requires agent analysis"
    }


def classify_reference_llm(reference: str) -> Optional[Dict[str, Any]]:
    """
    LLM-based classification for references that regex patterns couldn't handle.
    Returns a classification dict compatible with process_reference_deterministic,
    or None if the reference is invalid/unclassifiable.
    """
    try:
        prompt = get_analyze_reference_prompt(reference)
        resp = call_llm(prompt, json_format=True)
        raw = json.loads(_repair_json(resp.choices[0].message.content))
        result = ReferenceClassificationResponse.model_validate(raw)
        return result.to_classification_dict()
    except Exception:
        return None


def classify_references_batch_llm(references: List[str]) -> Dict[int, Optional[Dict[str, Any]]]:
    """
    Batch LLM-based classification for multiple references in a single call.

    Args:
        references: List of reference strings to classify

    Returns:
        Dict mapping 0-based index to classification dict (or None if invalid).
        Returns empty dict on total failure (triggers per-reference fallback).
    """
    try:
        prompt = get_batch_analyze_references_prompt(references)
        resp = call_llm(prompt, json_format=True)
        raw = json.loads(_repair_json(resp.choices[0].message.content))
        batch_result = BatchClassificationResponse.model_validate(raw)

        output: Dict[int, Optional[Dict[str, Any]]] = {}
        for i in range(len(references)):
            key = str(i)
            if key not in batch_result.classifications:
                output[i] = None
                continue
            output[i] = batch_result.classifications[key].to_classification_dict()

        return output

    except Exception as e:
        print(f"⚠️  Batch classification error: {e}")
        return {}


# ============================================================================
# Metadata Validation (fuzzy matching)
# ============================================================================

def _similarity_score(s1: str, s2: str) -> float:
    """Calculate similarity between two strings (0.0 to 1.0)."""
    if not s1 or not s2:
        return 0.0
    return SequenceMatcher(None, s1.lower(), s2.lower()).ratio()


def _validate_metadata_match(reference: str, metadata: Dict[str, Any], threshold: float = 0.4) -> Tuple[bool, float]:
    """
    Validate that fetched metadata actually matches the reference.
    Uses fuzzy matching on title and authors.
    
    Returns: (is_valid, confidence_score)
    """
    if not metadata or "error" in metadata:
        return False, 0.0
    
    ref_lower = reference.lower()
    
    # Check title match
    title = metadata.get("title", "")
    title_score = _similarity_score(title, reference)
    
    # Also check if title words appear in reference
    if title:
        title_words = [w for w in title.lower().split() if len(w) > 3]
        words_in_ref = sum(1 for w in title_words if w in ref_lower)
        word_match_ratio = words_in_ref / len(title_words) if title_words else 0
        title_score = max(title_score, word_match_ratio)
    
    # Check author match  
    authors = metadata.get("authors", [])
    author_score = 0.0
    if authors:
        # Check if any author last name appears in reference
        for author in authors[:3]:  # Check first 3 authors
            last_name = author.split()[-1].lower() if author else ""
            if last_name and len(last_name) > 2 and last_name in ref_lower:
                author_score = max(author_score, 0.8)
                break
    
    # Combined score
    combined_score = max(title_score, author_score)
    
    return combined_score >= threshold, combined_score


# ============================================================================
# Deterministic Processing (uses core functions)
# ============================================================================

def _enrich_metadata_authors(metadata: Dict, reference: str) -> Dict:
    """Fill in missing authors from the reference text when API didn't provide them.

    Tries regex extraction first (cheap), falls back to nothing.
    Mutates and returns metadata dict.
    """
    authors = metadata.get("authors", [DEFAULT_AUTHOR])
    if authors and authors != [DEFAULT_AUTHOR] and authors != ["Unknown"]:
        return metadata

    # Try standard citation pattern: "Author, A. B. (Year). Title..."
    match = re.search(PATTERN_CITATION, reference)
    if match:
        author_str = match.group(1).strip()
        if author_str and len(author_str) > 2:
            metadata["authors"] = [author_str]
            return metadata

    # Try broader author-year pattern
    match = re.search(PATTERN_AUTHOR_YEAR, reference)
    if match:
        # Extract just the author portion (everything before the year parenthetical)
        author_part = re.sub(r'\s*\(\d{4}[a-z]?\)\s*$', '', match.group(0)).strip()
        if author_part and len(author_part) > 2:
            metadata["authors"] = [author_part]
            return metadata

    return metadata


def _save_as_unresolvable(
    reference: str,
    original_source_type: str,
    origin: str,
    output_dir: str,
    reason: str,
    allowed_topics: Optional[List[str]] = None,
    unresolvable_dir: Optional[str] = None,
    topic_info: Optional[Dict[str, List[str]]] = None,
) -> Optional[Dict[str, Any]]:
    """Fallback: extract minimal metadata from citation text and save as Unresolvable."""
    try:
        metadata = _extract_minimal_metadata(reference, allowed_topics=allowed_topics)
        _enrich_metadata_authors(metadata, reference)
        note = {
            "summary": metadata.get("summary", ""),
            "topics": metadata.get("topics", []),
            "body_sections": metadata.get("body_sections", {})
        }
        save_result = _save_markdown_core(metadata, note, "Unresolvable", origin, output_dir,
                                          unresolvable_dir=unresolvable_dir, topic_info=topic_info)
        if "error" in save_result:
            return None
        return {
            "status": "uncertain",
            "path": save_result.get("file_path"),
            "source_type": "Unresolvable",
            "original_source_type": original_source_type,
            "note": f"Fallback: {reason}",
        }
    except Exception:
        return None


def process_reference_deterministic(
    reference: str,
    classification: Dict[str, Any],
    origin: str,
    output_dir: str,
    pre_extracted_metadata: Optional[Dict[str, Any]] = None,
    allowed_topics: Optional[List[str]] = None,
    unresolvable_dir: Optional[str] = None,
    rare_types_dir: Optional[str] = None,
    topic_info: Optional[Dict[str, List[str]]] = None,
) -> Optional[Dict[str, Any]]:
    """
    Fast deterministic processing without agent overhead.

    All API lookup failures gracefully degrade to Unresolvable with minimal
    LLM-extracted metadata. Returns None only on catastrophic failure
    (e.g. LLM and filesystem both unavailable).

    Args:
        pre_extracted_metadata: If provided, used instead of calling
            _extract_minimal_metadata for Unresolvable/Other references.
        allowed_topics: Optional list of allowed topic names for constrained assignment.
        unresolvable_dir: Optional absolute path for Unresolvable output.
        rare_types_dir: Optional absolute path for Post/Quote output.
        topic_info: Optional dict with 'maps', 'concepts', 'approved' lists for formatting.
    """
    source_type = classification["source_type"]
    original_source_type = source_type
    identifier_type = classification["identifier_type"]
    identifier_value = classification["identifier_value"]
    url = classification.get("url")

    # Shared kwargs for _save_as_unresolvable calls
    _unresolvable_kwargs = dict(
        allowed_topics=allowed_topics,
        unresolvable_dir=unresolvable_dir,
        topic_info=topic_info,
    )

    try:
        metadata = None

        # Step 1: Fetch metadata based on source type
        if source_type == "Research Paper":
            metadata = _fetch_paper_metadata_core(identifier_type, identifier_value)

            # Validate metadata matches reference (especially for title searches)
            if metadata and "error" not in metadata and identifier_type == "Title":
                is_valid, score = _validate_metadata_match(reference, metadata)
                if not is_valid:
                    metadata = _extract_minimal_metadata(reference, allowed_topics=allowed_topics)
                    source_type = "Unresolvable"
            elif not metadata or "error" in metadata:
                metadata = _extract_minimal_metadata(reference, allowed_topics=allowed_topics)
                source_type = "Unresolvable"

        elif source_type == "Article" and identifier_type == "Title":
            metadata = _fetch_paper_metadata_core(identifier_type, identifier_value)

            if metadata and "error" not in metadata:
                is_valid, score = _validate_metadata_match(reference, metadata)
                if not is_valid:
                    metadata = _extract_minimal_metadata(reference, allowed_topics=allowed_topics)
                    source_type = "Unresolvable"
            else:
                metadata = _extract_minimal_metadata(reference, allowed_topics=allowed_topics)
                source_type = "Unresolvable"

        elif source_type == "Book":
            metadata = fetch_google_books_metadata(identifier_value)

        elif source_type == "Unresolvable" or source_type == "Other":
            # Use pre-extracted metadata if available, otherwise call LLM
            metadata = pre_extracted_metadata or _extract_minimal_metadata(reference, allowed_topics=allowed_topics)
            source_type = "Unresolvable"  # Normalize for consistent handling

        else:
            # Article, Lecture, or other web content with URL
            metadata = _fetch_web_content_core(identifier_value, url)

        if not metadata or ("error" in metadata and source_type != "Unresolvable"):
            metadata = _extract_minimal_metadata(reference, allowed_topics=allowed_topics)
            source_type = "Unresolvable"

        # Enrich missing authors from reference text (web APIs often lack author info)
        if source_type != "Book":
            _enrich_metadata_authors(metadata, reference)

        # Step 2: Book handling path
        if source_type == "Book":
            save_result = _save_book_to_reading_list_core(metadata, origin, output_dir)
            if "error" not in save_result:
                return {
                    "status": "success",
                    "path": save_result.get("file_path"),
                    "source_type": source_type
                }
            # Book save failed — degrade to Unresolvable
            metadata = _extract_minimal_metadata(reference, allowed_topics=allowed_topics)
            _enrich_metadata_authors(metadata, reference)
            source_type = "Unresolvable"

        # Step 3: Unresolvable path - save directly without generate_note LLM call
        if source_type == "Unresolvable":
            note = {
                "summary": metadata.get("summary", ""),
                "topics": metadata.get("topics", []),
                "body_sections": metadata.get("body_sections", {})
            }
            save_result = _save_markdown_core(metadata, note, source_type, origin, output_dir,
                                              unresolvable_dir=unresolvable_dir, topic_info=topic_info)
            if "error" in save_result:
                return None

            status = "uncertain" if original_source_type != source_type else "success"
            return {
                "status": status,
                "path": save_result.get("file_path"),
                "source_type": source_type,
                "original_source_type": original_source_type,
                "note": "Fallback: created minimal note from citation text"
            }

        # Step 4: Paper/Article/Lecture - generate note and save
        content = metadata.get("content_for_note", "")
        if not content:
            return _save_as_unresolvable(
                reference, original_source_type, origin, output_dir,
                "no content available for note generation",
                **_unresolvable_kwargs,
            )

        note = _generate_note_core(source_type, content, allowed_topics=allowed_topics)
        if "error" in note:
            return _save_as_unresolvable(
                reference, original_source_type, origin, output_dir,
                "note generation failed",
                **_unresolvable_kwargs,
            )

        save_result = _save_markdown_core(metadata, note, source_type, origin, output_dir,
                                          unresolvable_dir=unresolvable_dir,
                                          rare_types_dir=rare_types_dir,
                                          topic_info=topic_info)
        if "error" in save_result:
            return None

        return {
            "status": "success",
            "path": save_result.get("file_path"),
            "source_type": source_type
        }

    except Exception:
        # Final fallback: try to save as Unresolvable
        return _save_as_unresolvable(
            reference, original_source_type, origin, output_dir,
            "exception during processing",
            **_unresolvable_kwargs,
        )


# Export tools for agent (parse_references_file excluded - references are
# pre-parsed before agent invocation, so the agent never needs file access)
TOOLS = [
    analyze_reference,
    fetch_paper_metadata,
    fetch_web_content,
    generate_note,
    fetch_book_metadata,
    save_book_to_reading_list,
    create_minimal_note,
    save_markdown,
]
