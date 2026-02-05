# PKM RAG - Personal Knowledge Management with Retrieval-Augmented Generation

A semantic search and AI assistant system for Obsidian vaults. This tool embeds your personal notes using vector embeddings and provides multiple interaction modes to explore, query, and analyze your knowledge base.

## Features

### Usage Modes

The PKM RAG provides five different modes for interacting with your knowledge:

#### 1. **Ask**
Chat-based Q&A grounded in note context. Ask questions about your notes and get AI-generated answers based on semantically relevant content.

- **Features:**
  - Natural language questions about your knowledge base
  - Optional query expansion for broader retrieval
  - Source attribution showing which notes informed the answer
  - Chat history preserved during session

- **Best for:** Finding answers to specific questions, exploring topics you've written about

#### 2. **Find Related**
Pure vector similarity search to discover semantically related notes without LLM inference.

- **Features:**
  - Select a note from your vault
  - Find the most similar notes based on semantic content
  - Option to exclude already linked notes (discover new connections)
  - Similarity scores for each result
  - Fast results without LLM calls

- **Best for:** Discovering connections between notes, finding potential links, exploring related topics

#### 3. **Connect**
Analyze relationships between multiple concepts by examining how they intersect in your knowledge base.

- **Features:**
  - Enter 2+ concepts (comma-separated)
  - Retrieves context for each concept
  - AI analyzes connections, overlaps, and relationships
  - Synthesizes insights across your notes

- **Best for:** Understanding how different ideas relate, finding synthesis opportunities, exploring conceptual intersections

#### 4. **Gap Analysis**
Identify what's missing or underrepresented in your knowledge base for a given topic.

- **Features:**
  - Enter a topic to analyze
  - Retrieves broad context from your notes
  - AI identifies coverage gaps, missing perspectives, and underexplored angles
  - Suggests areas for further research or note-taking

- **Best for:** Understanding blind spots, finding research opportunities, improving coverage of topics

#### 5. **Devil's Advocate**
Challenge a note's reasoning using evidence from your own knowledge base.

- **Features:**
  - Select a note to scrutinize
  - Finds related notes with potentially conflicting or complementary perspectives
  - AI generates counterarguments and challenges using your own notes as evidence
  - Helps stress-test ideas

- **Best for:** Critical thinking, finding contradictions, strengthening arguments, intellectual honesty

## Prerequisites

### Required Software

1. **Python 3.10+**
2. **Ollama** - Local LLM runtime
   - Download from: https://ollama.ai
   - Models required:
     - `nomic-embed-text` - For embeddings
     - `llama3.1:8b` - For chat (or customize in [pkm_rag_constants.py](pkm_rag_constants.py))

3. **Obsidian vault** with properly formatted notes (see [Note Format](#note-format))

### Installing Ollama Models

After installing Ollama, pull the required models:

```bash
ollama pull nomic-embed-text
ollama pull llama3.1:8b
```

## Installation

1. **Clone or navigate to the project directory:**

```bash
cd /path/to/pkm_rag
```

2. **Install Python dependencies:**

```bash
pip install -r requirements.txt
```

3. **Configure environment variables:**

Create a `.env` file in the project root:

```bash
VAULT_PATH=/path/to/your/obsidian/vault/notes
CHROMA_DB_PATH=./chroma_db
```

- `VAULT_PATH`: Absolute path to your Obsidian notes folder
- `CHROMA_DB_PATH`: Path where vector database will be stored (relative or absolute)

## Note Format

Your Obsidian notes must follow this structure to be embedded:

### Required Frontmatter Fields

```yaml
---
UUID: unique-identifier-here
Modified: 2025-01-31
Description: A brief summary of what this note is about
aliases:
  - Alternative Name 1
  - Alternative Name 2
---
```

- **UUID**: Unique identifier for the note (required for change detection)
- **Modified**: Date of last modification (used for incremental updates)
- **Description**: Brief summary (prepended to chunks for better semantic search)
- **aliases**: (Optional) Alternative names for the note

### Required Content Section

Notes must include a `## Notes` section:

```markdown
## Notes

Your actual note content goes here. This is what gets embedded and searched.

You can use [[wikilinks]] and other Obsidian syntax.
```

**Important:** Only content under the `## Notes` heading is embedded. Other sections are ignored.

### Example Note

```markdown
---
UUID: 20240131-agency
Modified: 2024-01-31
Description: Personal agency is the capacity to act independently and make free choices
aliases:
  - Personal Agency
  - Self-Determination
---

## Overview
Brief metadata about the concept

## Notes

Personal agency refers to the capacity of individuals to act independently and to make their own free choices. It's closely related to [[Autonomy]] and [[Free Will]].

Key aspects:
- Sense of control over one's actions
- Belief in ability to influence outcomes
- Connection to (Concept:: [[Responsibility|responsibility]])

Research shows that agency is fundamental to human motivation and well-being.
```

## Usage

### 1. Embedding Your Notes

Before using the RAG system, you need to embed your notes into the vector database.

#### Initial Embedding (First Time)

```bash
python pkm_rag_embed.py
```

This will:
- Scan your vault for all `.md` files
- Parse notes with proper frontmatter and `## Notes` section
- Chunk the content intelligently
- Generate embeddings using `nomic-embed-text`
- Store in ChromaDB

#### Incremental Updates

The system automatically detects:
- **New notes**: Embedded automatically
- **Modified notes**: Re-embedded (detected via `Modified` field)
- **Deleted notes**: Removed from database
- **Unchanged notes**: Skipped for efficiency

Simply run the same command after making changes:

```bash
python pkm_rag_embed.py
```

#### Force Re-embedding

If you change chunking parameters or want a fresh start:

```python
from pkm_rag_embed import run_embed

# Clears database and re-embeds everything
run_embed(force_embed=True)
```

#### Embedding Specific Files

To embed only specific files (useful for large vaults):

```python
from pkm_rag_embed import run_embed

run_embed(file_paths=[
    "/path/to/note1.md",
    "/path/to/note2.md"
])
```

### 2. Running the Streamlit Interface

After embedding your notes, launch the interactive UI:

```bash
streamlit run pkm_rag_app.py
```

This will:
- Start a local web server (default: http://localhost:8501)
- Open your browser automatically
- Load the embedded note collection
- Present the mode selector in the sidebar

### 3. Using the Interface

1. **Select a mode** from the sidebar:
   - Ask
   - Find Related
   - Connect
   - Gap Analysis
   - Devil's Advocate

2. **Follow the mode-specific interface:**
   - **Ask**: Type questions in the chat input
   - **Find Related**: Select a note and optionally filter linked notes
   - **Connect**: Enter comma-separated concepts
   - **Gap Analysis**: Enter a topic to analyze
   - **Devil's Advocate**: Select a note to challenge

3. **View results** with source attribution and similarity scores

## Customization

### Configuration Constants

Edit [pkm_rag_constants.py](pkm_rag_constants.py) to customize:

#### Models
```python
EMBED_MODEL = "nomic-embed-text"  # Ollama embedding model
CHAT_MODEL = "llama3.1:8b"         # Ollama chat model
```

#### Chunking Parameters
```python
CHUNK_SIZE = 800          # Characters per chunk
CHUNK_OVERLAP = 100       # Overlap between chunks
MIN_CHUNK_LENGTH = 50     # Minimum chunk size
```

#### Retrieval Settings
```python
TOP_K = 5                    # Results for Q&A mode
SIMILAR_TOP_K = 10           # Results for Find Related mode
GAP_ANALYSIS_TOP_K = 15      # Results for Gap Analysis
SIMILARITY_THRESHOLD = 0.5   # Minimum similarity score (0-1)
ENABLE_QUERY_REWRITE = False # Query expansion toggle
```

#### Vault Path
The vault path must be set via the `VAULT_PATH` environment variable in your `.env` file. There is no default fallback for security reasons.

### Advanced: Custom Prompts

Edit [pkm_rag_prompts.py](pkm_rag_prompts.py) to customize AI behavior for each mode.

## Troubleshooting

### Notes Not Appearing

**Check that your notes have:**
1. Valid YAML frontmatter with `UUID` field
2. A `## Notes` section with content
3. Content longer than `MIN_CHUNK_LENGTH` characters

**Run embedding with error output:**
```bash
python pkm_rag_embed.py
```

Look for skipped files and errors in the output.

### Ollama Connection Issues

**Ensure Ollama is running:**
```bash
ollama serve
```

**Verify models are pulled:**
```bash
ollama list
```

### Poor Search Results

**Try these adjustments in [pkm_rag_constants.py](pkm_rag_constants.py):**
- Increase `TOP_K` for more context
- Lower `SIMILARITY_THRESHOLD` for broader matching
- Enable `ENABLE_QUERY_REWRITE` for query expansion
- Adjust `CHUNK_SIZE` and `CHUNK_OVERLAP`

**After changing chunking parameters, re-embed:**
```python
from pkm_rag_embed import run_embed
run_embed(force_embed=True)
```

### Memory Issues

For very large vaults (1000+ notes):
- Increase Ollama memory limits
- Reduce `TOP_K`, `SIMILAR_TOP_K`, and `GAP_ANALYSIS_TOP_K`
- Process notes in batches using `file_paths` parameter

## Architecture

### Components

- **`pkm_rag_embed.py`**: Embedding pipeline (scanning, parsing, chunking, embedding)
- **`pkm_rag_parser.py`**: Obsidian markdown parser (frontmatter, wikilinks, content extraction)
- **`pkm_rag_app.py`**: Streamlit UI and interaction modes
- **`pkm_rag_prompts.py`**: LLM system and user prompts for each mode
- **`pkm_rag_constants.py`**: Centralized configuration

### Data Flow

1. **Embedding Pipeline**:
   ```
   Obsidian Notes → Parser → Chunker → Embeddings → ChromaDB
   ```

2. **Query Flow**:
   ```
   User Query → [Optional Rewrite] → Vector Search → Context Retrieval → LLM → Answer
   ```

3. **Find Related Flow**:
   ```
   Selected Note → Get Embedding → Vector Search → Ranked Results
   ```

### Vector Store

- **Database**: ChromaDB (persistent, local)
- **Embedding Model**: nomic-embed-text (768 dimensions)
- **Distance Metric**: Cosine similarity
- **Metadata Stored**: UUID, title, description, modified date, aliases, links, chunk info

## Performance

- **Embedding Speed**: ~100-200 notes/minute (depends on note size and hardware)
- **Query Latency**:
  - Vector search: <100ms
  - Full RAG with LLM: 2-5 seconds (depends on `CHAT_MODEL`)
- **Storage**: ~1-2MB per 100 embedded notes

## Privacy & Security

- **Fully Local**: All data stays on your machine
- **No Cloud Services**: Ollama runs locally, no API calls
- **No Telemetry**: No usage tracking or data collection
