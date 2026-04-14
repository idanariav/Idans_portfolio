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
SEMANTIC_SCHOLAR_RATE_LIMIT_DELAY = 5  # Base delay for exponential backoff on 429 (seconds)
SEMANTIC_SCHOLAR_MAX_RETRIES = 4  # Max retry attempts on rate limit (backoff: 5s, 10s, 20s)

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

# Folder for rare source types (Post, Quote) when no dedicated vault folder exists
RARE_TYPE_FOLDER = "Inbox"

# Hybrid Mode Configuration
HYBRID_MODE_ENABLED = True  # Enable fast deterministic path for obvious references
CONFIDENCE_THRESHOLD = 0.80  # Minimum confidence to use fast path (0.0-1.0)
FAST_PATH_STATS = True  # Track and display fast vs agent path usage
BATCH_SIZE = 12  # References per batch LLM classification call

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
# Captures parenthesized DOIs (e.g. 10.1016/s0140-6736(14)61682-2) and
# stops before trailing punctuation (.,;:) or angle brackets (>)
PATTERN_DOI = r'\b(10\.\d{4,}/[^\s>]+\w)'
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
    # Hypothetical/conditional/explanatory statements (Phase 1 improvements)
    r'^If\s+',
    r'^Imagine\s+',
    r'^Let\'?s say\s+',
    r'^Suppose\s+',
    r'^Consider\s+',
    r'^Thus\s+',
    r'^Therefore\s+',
    r'^Backward induction',
    r'This (formula|approach|method|technique|algorithm|sporting contest)',
    r'This is (how|why|what|where|when)',
    # Comparative/descriptive statements
    r'^Applying the',
    r'^One last word',
    r'^Looking into the matter',
    r'^Subsequent engineers',
    r'^Computer science has',
    r'^Now you have',
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
    r'\bIbid\.?\b',  # Changed: Match Ibid anywhere in text, not just at start
    r'^Id\.?$',
    r'^loc\. cit\.',
    r'^op\. cit\.',
    r'^supra note',
    r'^see note \d+',
    r'^note \d+ above',
]

# Standalone quote patterns (Phase 1 improvements)
# These are excerpts from text, not citations
QUOTE_PATTERNS = [
    r'^"[^"]+"\s*:\s+',  # Quote followed by colon and text
    r'^"[^"]+":(?!\s+[A-Z][a-z]+,?\s+[A-Z]\.)',  # Quote without proper citation after
]

# Section/list header patterns
SECTION_HEADER_PATTERNS = [
    r'^\d+\\\.\s+[A-Z]',  # Escaped numbered list: "1\. The tourist's problem"
    r'^\d+\.\s+[A-Z][^:]+:\s+[A-Z]',  # Normal numbered list with description
]

# Biographical/historical narrative patterns
BIOGRAPHICAL_PATTERNS = [
    r'was born in \d{4}',
    r'grew up in',
    r'studied at',
    r'career was spent',
    r'courting a total of',
]

# Mathematical derivation patterns
DERIVATION_PATTERNS = [
    r'is derived by',
    r'simplifies when',
    r'can be shown that',
    r'follows from',
    r'settles to \d+%',
]

# Minimum length for valid reference
MIN_REFERENCE_LENGTH = 20

# ============================================================================
# File-Level Screening Constants
# ============================================================================

# Minimum substantive (non-header, non-blank, non-image) lines for a file to be processed
MIN_SUBSTANTIVE_LINES = 5

# Minimum fraction of entries with citation signals (year, DOI, URL, ISBN, arXiv)
# Files below this threshold are likely pure narrative, not reference lists
MIN_CITATION_RATIO = 0.10

# ============================================================================
# Content Pre-Processing Patterns (lines to remove before splitting)
# ============================================================================

# Standalone markdown image lines (never citations)
PATTERN_MARKDOWN_IMAGE_LINE = r'^\s*>?\s*!\[.*?\]\(.*?\)\s*$'

# Standalone e-book internal navigation links
PATTERN_EBOOK_NAV_LINK_LINE = r'^\s*\[.*?\]\((?:nav\.xhtml|index_split_\d+\.html|\d+\w+\.xhtml)[^)]*\)\s*$'

# Horizontal rule patterns (markdown separators)
PATTERN_HORIZONTAL_RULE = r'^\s*(?:(\*\s*){3,}|(-\s*){3,}|(_\s*){3,})\s*$'

# Standalone section header lines (chapter/section markers, not citation content)
SECTION_HEADER_KEYWORDS = (
    r'(?:CHAPTER|Chapter|PART|Part|NOTES|Notes|SOURCES|Sources|'
    r'BIBLIOGRAPHY|Bibliography|ENDNOTES|Endnotes|REFERENCES|References)'
)
PATTERN_SECTION_HEADER_LINE = rf'^\s*#{{1,6}}\s+{SECTION_HEADER_KEYWORDS}\b[^(]*$'

# ============================================================================
# Additional Non-Citation Detection Patterns (Phase 2)
# ============================================================================

# Markdown header patterns (entries that are chapter/section headers after splitting)
MARKDOWN_HEADER_PATTERNS = [
    r'^#{1,6}\s+(?:CHAPTER|Chapter)\s+\d+',       # "# CHAPTER 1. THE MINDSETS"
    r'^#{1,6}\s+(?:PART|Part)\s+\d+',             # "## Part 2"
    r'^#{1,6}\s+\w+(?:\s+\w+){0,3}\s*$',          # Short headers: "## Prologue"
]

# Bible/scripture reference patterns (not academic citations)
# Matches "BookName chapter:verse" format (e.g., "Proverbs 18:21", "Luke 6:27-28")
SCRIPTURE_PATTERNS = [
    r'(?:Genesis|Exodus|Leviticus|Numbers|Deuteronomy|Joshua|Judges|Ruth|'
    r'1\s*Samuel|2\s*Samuel|1\s*Kings|2\s*Kings|1\s*Chronicles|2\s*Chronicles|'
    r'Ezra|Nehemiah|Esther|Job|Psalms?|Proverbs|Ecclesiastes|'
    r'Song of (?:Solomon|Songs)|Isaiah|Jeremiah|Lamentations|Ezekiel|Daniel|'
    r'Hosea|Joel|Amos|Obadiah|Jonah|Micah|Nahum|Habakkuk|Zephaniah|'
    r'Haggai|Zechariah|Malachi|'
    r'Matthew|Mark|Luke|John|Acts|Romans|'
    r'1\s*Corinthians|2\s*Corinthians|Galatians|Ephesians|Philippians|'
    r'Colossians|1\s*Thessalonians|2\s*Thessalonians|'
    r'1\s*Timothy|2\s*Timothy|Titus|Philemon|Hebrews|James|'
    r'1\s*Peter|2\s*Peter|1\s*John|2\s*John|3\s*John|Jude|Revelation)'
    r'\s+\d+:\d+',
]

# Collaborator-only notes (no publication cited, just mentions who participated)
COLLABORATOR_PATTERNS = [
    r'^This (?:research|work|study|experiment) was (?:conducted|done|carried out) with\b',
    r'^This (?:research|work|study) (?:is being|was being) (?:conducted|done|carried out) with\b',
    r'^Thanks to .+ for (?:this|the) (?:quote|example|suggestion|idea)',
    r'^(?:Supported|Funded) by (?:a |the )?grant from\b',
    r'^(?:I am |We are )?grateful to\b',
]

# Marketing/promotional content patterns
MARKETING_PATTERNS = [
    r'(?:Call|Contact) (?:us )?at:?\s+\d',          # "Call us at 517-699-3570"
    r'\b\d{3}[-.)]\s*\d{3}[-.)]\s*\d{4}\b',         # Phone numbers: 800-555-1234
    r'^Visit:?\s+(?:www\.|http)',                     # "Visit www.example.com"
    r'^E-?mail (?:us )?at:',                          # "E-mail us at: info@..."
]

# Vague references without specific publications
VAGUE_REFERENCE_PATTERNS = [
    r'^See the (?:fine|excellent|important|notable) work (?:of|by)\b',
    r'^See the work (?:of|by)\b',
    r'^See also the (?:research|work|writing) (?:of|by)\b',
]

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
