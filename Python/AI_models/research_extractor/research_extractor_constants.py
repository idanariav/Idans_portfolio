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
}

# Source Type Categories
SOURCE_TYPES = [
    "Research Paper",
    "Article",
    "Book",
    "Lecture",
    "Post",
    "Quote"
]

# Validation Settings
MIN_IDENTIFIER_LENGTH = 5
MIN_WORD_CONTENT_LENGTH = 3

# Meaningless Reference Patterns (for validation)
MEANINGLESS_PATTERNS = [
    r'^Ibid',           # Ibidem reference
    r'^\d{3,}[a-z]\d+–\d+$',  # Classical reference format (e.g., 1144a4–5)
    r'^loc\.\s*cit',    # Loco citato
    r'^id\.',           # Idem
    r'^supra',          # Supra note
    r'^infra',          # Infra note
]

# NYT Domains
NYT_DOMAINS = [
    'nytimes.com',
    'www.nytimes.com',
    'query.nytimes.com',
    'archive.nytimes.com'
]
