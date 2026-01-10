"""
Prompts for the research extractor pipeline and agent.
"""

# Agent System Prompt - Optimized for Gemini with function calling
AGENT_SYSTEM_PROMPT = """You are a metadata extraction specialist converting academic/web references and books into Obsidian markdown notes.

KEY BEHAVIORS:
- parse_references_file auto-filters non-citations and splits compound references
- Skip invalid identifiers ("Ibid", too short, meaningless)
- Never retry failed fetches - skip immediately on timeout/error
- Provide brief status per reference

TOOLS:
- parse_references_file: Load and filter references
- analyze_reference: Classify source + extract identifier + validate (ONE call)
- fetch_paper_metadata: Semantic Scholar (includes content_for_note)
- fetch_web_content: Web scraping (includes content_for_note)
- generate_note: Create structured summary from content_for_note
- fetch_book_metadata: Google Books
- save_book_to_reading_list: Append book entry
- create_minimal_note: Extract from citation text when no API available
- save_markdown: Export to categorized folder

WORKFLOW:
1. Parse input → get valid references
2. For each:
   - Analyze (get source_type, identifier, validation)
   - Skip if invalid
   - Book: fetch_book_metadata → save_book_to_reading_list
   - Research Paper/Article with Title: fetch_paper_metadata → if fails → create_minimal_note
   - Unresolvable: create_minimal_note → save_markdown
   - Other (URL): fetch_web_content → generate_note → save_markdown
3. Output: total|success|skipped|failed

SKIP IF: Pre-filtered|Invalid identifier|Timeout|API error|No metadata

Begin when given: input_file, output_dir, origin.
"""


def get_analyze_reference_prompt(reference: str) -> str:
    """
    Get prompt for analyzing a reference - classifies source type AND extracts identifier in one LLM call.
    
    Args:
        reference: The reference string to analyze
    
    Returns:
        Prompt string for the LLM
    """
    return f"""Analyze this bibliographic reference. Classify type and extract the best identifier for lookup.

TYPES:
- Book: Has publisher/ISBN/"Press"/"edition", no journal info
- Research Paper: Has journal/DOI/arXiv/volume-issue/conference
- Article: News/magazine/blog with URL or publication name
- Other: Lectures, talks, interviews, archival docs
- Unresolvable: Valid ref but cannot be searched (personal comm, incomplete)
- Invalid: Cross-refs only (ibid, op.cit, supra note, page numbers only)

IDENTIFIER PRIORITY:
Papers: DOI > arXiv > CorpusID > Title
Articles: URL > Title  
Books: ISBN > "Title + Author"
Other: URL > Title
Invalid: null

RULES:
- Extract identifier exactly as written (don't invent/guess)
- If author+title+year present, try as Research Paper first
- Prefer classifying as searchable type over Unresolvable

REFERENCE:
{reference}

Return JSON:
{{"source_type": "...", "identifier_type": "DOI|arXiv|CorpusID|URL|ISBN|Title|CitationText|None", "identifier_value": "...", "is_valid": true/false, "validation_reason": "", "confidence": "high|medium|low", "rationale": "brief signal explanation"}}"""


def get_generate_note_prompt(source_type: str, content: str) -> str:
    """Get prompt for generating notes from content."""
    if source_type == "Research Paper":
        sections = "Hypothesis|Methodology|Main Findings"
    elif source_type == "Article":
        sections = "Main Story|Credibility|Main Findings"
    else:
        sections = "Main Story|Credibility|Main Findings"
    
    return f"""Generate structured notes from this {source_type}.

CONSTRAINTS:
- Summary: ~50 words, practical findings, clear language
- Sections: {sections} (max 200 words each)
- Topics: 1-3 broad single-noun topics
- Use ONLY the content provided

CONTENT:
{content}

Return JSON: {{"summary": "...", "topics": [...], "body_sections": {{...}}}}"""
