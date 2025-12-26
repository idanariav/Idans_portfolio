# Research Extractor Agent

An autonomous LLM agent that processes academic and web references, extracts metadata, and generates structured markdown notes optimized for personal knowledge management systems like Obsidian.

## 🚀 New Agent Architecture

This project has been **converted from a sequential pipeline to an LLM agent** using LangChain's ReAct framework. The agent autonomously decides how to process each reference, handle errors, and skip invalid entries.

### Key Features

- **Autonomous Decision-Making**: Agent decides when to skip, retry, or proceed based on reference quality
- **Timeout Protection**: 30-second timeouts on API calls prevent hanging on slow sources
- **Context Reset**: Each reference processed with fresh context to prevent token overflow
- **Tool-Based Architecture**: Extensible design with 8 specialized tools
- **Comprehensive Testing**: Test suite validates processing of diverse reference types

## 📁 Project Structure

```
research_extractor/
├── research_extractor_agent.py      # Main agent implementation (NEW)
├── research_extractor_tools.py      # LangChain tool definitions (NEW)
├── research_extractor_prompts.py    # Prompts for LLM and agent
├── research_extractor_api_utils.py  # API utilities (Semantic Scholar, web scraping)
├── research_extractor_pipeline.py   # DEPRECATED - Old sequential pipeline
├── test_research_extractor.py       # Test suite (NEW)
├── requirements.txt                 # Updated with LangChain dependencies
└── README.md                        # This file (NEW)
```

## 🔧 Installation

### 1. Install Dependencies

```bash
pip install -r requirements.txt
```

This will install:
- `openai` - For OpenRouter API access
- `requests` - HTTP requests
- `python-dotenv` - Environment variable management
- `trafilatura` - Web content extraction
- `beautifulsoup4` - HTML parsing
- `langchain>=0.1.0` - Agent framework
- `langchain-openai>=0.0.5` - OpenAI integration
- `langchain-community` - Community tools

### 2. Configure Environment Variables

Create a `.env` file with:

```bash
OPENROUTER_API_KEY=your_openrouter_api_key
TAVILY_API_KEY=your_tavily_api_key  # Optional, for web search fallback
NYT_API_KEY=your_nyt_api_key        # Optional, for NYT articles
```

**Get API Keys:**
- OpenRouter: https://openrouter.ai/ (supports multiple LLM providers)
- Tavily: https://tavily.com/ (web search API)
- NYT: https://developer.nytimes.com/ (New York Times articles)

## 🎯 Usage

### Basic Usage

```python
from research_extractor_agent import run_agent

result = run_agent(
    input_file="path/to/references.txt",
    output_dir="path/to/output",
    origin="[[Source Book Name]]",
    verbose=False  # Set True for detailed agent reasoning
)

print(f"Success: {result['success']}/{result['total']}")
```

### Command Line

```bash
python research_extractor_agent.py
```

(Edit the `__main__` block to customize paths)

### Input Format

Create a text file with references separated by double newlines or numbered lists:

```
Attention Is All You Need
Vaswani et al., 2017
DOI: 10.48550/arXiv.1706.03762

https://www.nytimes.com/2023/05/15/technology/ai-language-models.html

The human brain is a prediction machine: Hermann von Helmholtz developed this idea.
```

### Output

Markdown files organized by type:
```
output/
├── Journals/          # Research papers
├── Articles/          # Web articles
├── Lectures/          # Lectures and talks
├── Socials/           # Social media posts
└── Quotes/            # Quotes and citations
```

Each file includes:
- YAML frontmatter (authors, year, topics, links)
- Structured sections (Hypothesis, Methodology, Main Findings, etc.)
- Obsidian-compatible links (`[[Author]]`, `[[Topic (MOC)]]`)

## 🧪 Testing

Run the test suite to validate the agent:

```bash
python test_research_extractor.py
```

Tests cover:
1. Basic processing with diverse references
2. Invalid reference handling
3. Research paper DOI lookup
4. Book skipping behavior

## 🛠️ Available Tools

The agent has access to 8 tools:

| Tool | Purpose |
|------|---------|
| `parse_references_file` | Load and parse references from text file |
| `classify_source_type` | Classify as Research Paper, Article, Lecture, Post, Quote, or Book |
| `extract_identifier` | Extract DOI, ArXiv ID, Title, or URL |
| `validate_identifier` | Check if identifier is meaningful/searchable |
| `fetch_paper_metadata` | Get metadata from Semantic Scholar API |
| `fetch_web_content` | Scrape web content and metadata |
| `generate_note` | Create structured notes with LLM |
| `save_markdown` | Export to formatted markdown file |

## 🔄 Agent Workflow

For each reference, the agent follows this workflow:

```
1. Parse input file → Get all references
2. For each reference:
   ├─ Classify source type
   ├─ Skip if "Book"
   ├─ Extract identifier (DOI/ArXiv/Title/URL)
   ├─ Validate identifier
   │  └─ If invalid (e.g., "Ibid") → SKIP
   ├─ Fetch metadata
   │  ├─ Research Paper → Semantic Scholar API
   │  └─ Other → Web scraping
   │     └─ If timeout/error → SKIP
   ├─ Generate structured notes
   └─ Save markdown file
3. Report summary (success/skipped/failed counts)
```

## ⚙️ Configuration

### Change LLM Model

Edit `research_extractor_tools.py` and `research_extractor_agent.py`:

```python
MODEL = "google/gemini-3-flash-preview"  # Current default
# Or try: "anthropic/claude-3-haiku", "openai/gpt-4", etc.
```

### Adjust Timeouts

Edit `research_extractor_tools.py`:

```python
FETCH_TIMEOUT = 30  # seconds (default)
```

### Customize Folder Mapping

Edit `research_extractor_tools.py`:

```python
FOLDER_MAP = {
    "Research Paper": "Journals",
    "Article": "Articles",
    "Lecture": "Lectures",
    "Post": "Socials",
    "Quote": "Quotes",
}
```

## 📊 Processing Results

The agent returns a summary dictionary:

```python
{
    "total": 10,           # Total references found
    "success": 7,          # Successfully processed
    "skipped": 2,          # Skipped (invalid/book)
    "failed": 1,           # Failed (error/timeout)
    "details": [...]       # List of results per reference
}
```

## 🔍 Verbose Mode

Enable verbose mode to see agent reasoning:

```python
result = run_agent(
    input_file="references.txt",
    output_dir="output/",
    origin="[[My Book]]",
    verbose=True  # Shows thought process and tool calls
)
```

Output example:
```
> Entering new AgentExecutor chain...
Thought: I need to first classify this reference to determine its type
Action: classify_source_type
Action Input: "Attention Is All You Need, Vaswani et al..."
Observation: {"source_type": "Research Paper", "status": "success"}
Thought: This is a research paper, I should extract the identifier...
```

## 🚫 Skipping Rules

The agent automatically skips:

1. **Books** - Different processing required
2. **Invalid Identifiers**:
   - "Ibid" (ibidem references)
   - "loc. cit." (loco citato)
   - Classical references (e.g., "1144a4-5")
   - Too short (< 5 characters)
   - Mostly punctuation/numbers
3. **Timeout/Errors**:
   - API timeouts (>30 seconds)
   - Network failures
   - Missing required metadata

## 🔮 Migration from Old Pipeline

If you were using `research_extractor_pipeline.py`:

**Old Code:**
```python
from research_extractor_pipeline import main
main(input_file, origin, output_dir)
```

**New Code:**
```python
from research_extractor_agent import run_agent
run_agent(input_file, output_dir, origin, verbose=False)
```

The old pipeline is now deprecated but kept for reference.

## 🐛 Troubleshooting

### Import Errors

```bash
# Ensure all dependencies installed
pip install -r requirements.txt --upgrade
```

### API Rate Limits

If hitting rate limits on Semantic Scholar:
- The agent automatically skips after timeout
- Consider adding delays between batches
- Use a smaller test file first

### No Markdown Files Created

Check:
1. All references valid? (Run verbose mode to see skip reasons)
2. API keys configured? (Check `.env` file)
3. Output directory writable? (Check permissions)

### Timeout Issues

Increase timeout in `research_extractor_tools.py`:
```python
FETCH_TIMEOUT = 60  # Increase to 60 seconds
```

## 📝 Example Session

```bash
$ python research_extractor_agent.py

################################################################################
# Research Extractor Agent
################################################################################
Input file: /path/to/references.txt
Output directory: /path/to/output
Origin: [[My Book]]
Verbose mode: False
################################################################################

📚 Found 5 references to process

================================================================================
Processing reference 1/5
================================================================================
Reference: Attention Is All You Need, Vaswani et al., 2017...

✅ Successfully saved: Journals/Attention Is All You Need.md

================================================================================
Processing reference 2/5
================================================================================
Reference: https://www.nytimes.com/2023/05/15/technology...

✅ Successfully saved: Articles/AI Language Models.md

... (continues for all references)

================================================================================
PROCESSING COMPLETE
================================================================================
Total references: 5
✅ Successfully saved: 4
⏭️  Skipped: 1
❌ Failed: 0

📋 Skipped References:
  - Ibid.... (Reason: invalid identifier)
================================================================================

✨ Final Summary:
   Success rate: 4/5 (80.0%)
```

## 🤝 Contributing

To extend the agent:

1. **Add New Tools**: Create tools in `research_extractor_tools.py` with `@tool` decorator
2. **Modify Prompts**: Update `research_extractor_prompts.py` for agent behavior
3. **Add Tests**: Extend `test_research_extractor.py` with new test cases

## 📄 License

This project is part of Idan's Portfolio.

## 🙏 Acknowledgments

- Built with [LangChain](https://www.langchain.com/)
- Uses [OpenRouter](https://openrouter.ai/) for LLM access
- Semantic Scholar API for academic papers
- Trafilatura for web content extraction
