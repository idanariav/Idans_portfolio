"""
Centralized constants for the PKM RAG system.

Contains all configuration for paths, models, chunking, retrieval,
and regex patterns used across the PKM RAG components.
"""

# Ollama Models
EMBED_MODEL = "nomic-embed-text"
CHAT_MODEL = "llama3.1:8b"
EMBED_DIMENSIONS = 768

# ChromaDB
COLLECTION_NAME = "pkm_notes"
DEFAULT_CHROMA_DB_PATH = "./chroma_db"

# Chunking
CHUNK_SIZE = 512
CHUNK_OVERLAP = 64
MIN_CHUNK_LENGTH = 50
CHUNK_SEPARATORS = ["\n## ", "\n### ", "\n\n", "\n", ". ", " "]

# Retrieval
TOP_K = 5
SIMILARITY_THRESHOLD = 0.3
SIMILAR_TOP_K = 10
GAP_ANALYSIS_TOP_K = 15
ENABLE_QUERY_REWRITE = True

# Parsing
NOTES_SECTION_HEADER = "## Notes"
# (Jump:: [[Responsibility|responsible]]) -> responsible
PROPERTY_WIKILINK_PATTERN = r'\([A-Za-z]+::\s*\[\[(?:[^\]|]*\|)?([^\]]+)\]\]\)'
# [[Target|display]] -> display, [[Target]] -> Target
WIKILINK_PATTERN = r'\[\[(?:[^\]|]*\|)?([^\]]+)\]\]'
# Dataview inline fields like "key:: value" at start of line
DATAVIEW_FIELD_PATTERN = r'^\s*\w+::\s*'

# Metadata keys to store in ChromaDB
FRONTMATTER_KEYS_TO_STORE = [
    "UUID", "Description", "Modified", "aliases"
]

# Streamlit
PAGE_TITLE = "PKM Knowledge Assistant"
