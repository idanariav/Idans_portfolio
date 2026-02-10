"""
Centralized constants for the research extractor agent.

This module contains all configuration constants used across the research extractor
components including API endpoints, model settings, folder mappings, and timeouts.
"""

# LLM Configuration
# Using Gemini 2.5 Flash - has optional thinking capabilities without the
# strict reasoning_details requirement that Gemini 3 models have
MODEL = "google/gemini-2.5-flash"

# API Endpoints
SEMANTIC_SCHOLAR_API_URL = "https://api.semanticscholar.org/graph/v1/paper/search"
OPENALEX_API_URL = "https://api.openalex.org/works"
TAVILY_API_URL = "https://api.tavily.com/search"
NYT_API_URL = "https://api.nytimes.com/svc/search/v2/articlesearch.json"
OPENROUTER_API_BASE = "https://openrouter.ai/api/v1"

# Semantic Scholar Configuration
SEMANTIC_SCHOLAR_FIELDS = "title,authors,year,abstract,url,tldr"
SEMANTIC_SCHOLAR_RATE_LIMIT_DELAY = 3  # Base delay between requests (seconds)
SEMANTIC_SCHOLAR_MAX_RETRIES = 3  # Max retry attempts on rate limit

# Timeout Settings (seconds)
FETCH_TIMEOUT = 30
API_REQUEST_TIMEOUT = 20
NYT_API_TIMEOUT = 10

# Folder Mapping for Markdown Export
FOLDER_MAP = {
    "Research Paper": "Journals",
    "Article": "Articles",
    "Lecture": "Lectures",
    "Post": "Socials",
    "Quote": "Quotes",
    "Unresolvable": "Unresolvable",
}

# Hybrid Mode Configuration
HYBRID_MODE_ENABLED = True  # Enable fast deterministic path for obvious references
CONFIDENCE_THRESHOLD = 0.80  # Minimum confidence to use fast path (0.0-1.0)
FAST_PATH_STATS = True  # Track and display fast vs agent path usage

# NYT Domains
NYT_DOMAINS = [
    'nytimes.com',
    'www.nytimes.com',
    'query.nytimes.com',
    'archive.nytimes.com'
]
# ============================================================================
# Regex Patterns for Reference Parsing and File Operations
# ============================================================================

# File parsing patterns
PATTERN_DOUBLE_NEWLINE = r"\n\s*\n"
PATTERN_NUMBERED_LIST = r"\.\s+\d{1,2}\.\s+"

# Reference identification patterns
PATTERN_DOI = r'\b(10\.\d{4,}/[^\s]+)'
PATTERN_ARXIV = r'arXiv:\s*(\d{4}\.\d{4,5})'
PATTERN_CORPUSID = r'(?:CorpusID[:\s]+)?(\d{7,})'
PATTERN_ISBN = r'ISBN[:\s-]*(\d{10}|\d{13})'
PATTERN_URL = r'https?://([^\s]+)'
PATTERN_CITATION = r'^([A-Z][a-z]+(?:,?\s+[A-Z]\.?)+)\s+\((\d{4})\)\.?\s+(.+)'

# Compound reference detection (multiple author-year patterns)
PATTERN_AUTHOR_YEAR = r'[A-Z][a-z]+(?:,?\s+(?:[A-Z]\.?\s*)+)?(?:,?\s+(?:and|&)\s+[A-Z][a-z]+(?:,?\s+[A-Z]\.?)*)?(?:,?\s+et al\.?)?\s+\(\d{4}[a-z]?\)'
PATTERN_JOURNAL_INFO = r'[A-Z][^.]+(?:Journal|Review|Bulletin|Science|Proceedings)[^.]+,\s*\d+(?:,|\()'

# Invalid filename character patterns
PATTERN_INVALID_FILENAME_CHARS = r"[\\/:*?\"<>|]"

# Web domain patterns for classification
ARTICLE_DOMAINS = ['nytimes.com', 'wsj.com', 'bbc.com', 'cnn.com', 
                   'medium.com', 'theguardian.com', 'washingtonpost.com']
VIDEO_DOMAINS = ['youtube.com', 'youtu.be', 'vimeo.com', 'ted.com']

# ============================================================================
# Non-Citation Detection Patterns (Preprocessing)
# ============================================================================

# Narrative/commentary phrases that indicate non-bibliographic text
NARRATIVE_STARTERS = [
    r'^An excellent summary',
    r'^For a comprehensive',
    r'^The description of',
    r'^A detailed analysis',
    r'^See also',
    r'^For more information',
    r'^For further discussion',
    r'^As discussed in',
    r'^This is discussed',
    r'^For an overview',
]

# Study/survey methodology markers
METHODOLOGY_MARKERS = [
    r'survey conducted',
    r'fielded [A-Z][a-z]+',  # "fielded July"
    r'\bn\s*=\s*\d+',  # sample size notation
    r'sample of \d+',
    r'online survey',
    r'nationally representative',
]

# Cross-reference patterns (not resolvable citations)
CROSS_REFERENCE_PATTERNS = [
    r'^Ibid\.?$',
    r'^Id\.?$',
    r'^loc\. cit\.',
    r'^op\. cit\.',
    r'^supra note',
    r'^see note \d+',
    r'^note \d+ above',
]

# Minimum length for valid reference
MIN_REFERENCE_LENGTH = 20

# ============================================================================
# Default Values for Missing Data
# ============================================================================

DEFAULT_TITLE = "Untitled"
DEFAULT_AUTHOR = "Unknown"
DEFAULT_PUBLISHED_DATE = "Unknown"
DEFAULT_YEAR = "Unknown"
DEFAULT_DESCRIPTION = "No description available."
DEFAULT_CATEGORY = "Uncategorized"

# ============================================================================
# Markdown Output Defaults
# ============================================================================

DEFAULT_OUTPUT_FOLDER = "Misc"
MARKDOWN_TAG_BIBLIOGRAPHY = "Type/Bibliography"

# ============================================================================
# Markdown Frontmatter Template Constants
# ============================================================================

FRONTMATTER_VERSION = "1"
FRONTMATTER_PUBLISH_DEFAULT = False
