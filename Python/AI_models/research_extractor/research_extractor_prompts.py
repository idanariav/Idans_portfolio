"""
Prompts for the research extractor pipeline and agent.
"""

from typing import List, Optional

# Agent System Prompt - Optimized for Gemini with function calling
AGENT_SYSTEM_PROMPT = """You are a metadata extraction specialist converting academic/web references and books into Obsidian markdown notes.

KEY BEHAVIORS:
- References are pre-parsed and pre-filtered before you receive them
- Skip invalid identifiers ("Ibid", too short, meaningless)
- Never retry failed fetches - skip immediately on timeout/error
- Provide brief status per reference

TOOLS:
- analyze_reference: Classify source + extract identifier + validate (ONE call)
- fetch_paper_metadata: Semantic Scholar (includes content_for_note)
- fetch_web_content: Web scraping (includes content_for_note)
- generate_note: Create structured summary from content_for_note
- fetch_book_metadata: Google Books
- save_book_to_reading_list: Append book entry
- create_minimal_note: Extract from citation text when no API available
- save_markdown: Export to categorized folder

WORKFLOW:
For each reference:
1. Analyze (get source_type, identifier, validation)
2. Skip if invalid
3. Book: fetch_book_metadata → save_book_to_reading_list
4. Research Paper/Article with Title: fetch_paper_metadata → if fails → create_minimal_note
5. Unresolvable: create_minimal_note → save_markdown
6. Other (URL): fetch_web_content → generate_note → save_markdown
Output: total|success|skipped|failed

SKIP IF: Pre-filtered|Invalid identifier|Timeout|API error|No metadata

Begin when given: input_file, output_dir, origin.
"""


# ============================================================================
# Shared prompt fragments for classification
# ============================================================================

_CLASSIFICATION_INSTRUCTIONS = """TYPES:
- Book: Has publisher/ISBN/"Press"/"edition", no journal info
- Research Paper: Has journal/DOI/arXiv/volume-issue/conference
- Article: News/magazine/blog with URL or publication name
- Other: Lectures, talks, interviews, archival docs
- Unresolvable: Valid ref but cannot be searched (personal comm, incomplete)
- Invalid: Cross-refs only (ibid, op.cit, supra note, page numbers only)

IDENTIFIER PRIORITY (use these exact identifier_type values):
Papers: DOI > arXiv > CorpusID > Title
Articles: URL > Title
Books: ISBN > CitationText
Other: URL > Title
Unresolvable: CitationText
Invalid: None

RULES:
- Extract identifier exactly as written (don't invent/guess)
- If author+title+year present, try as Research Paper first
- Prefer classifying as searchable type over Unresolvable
- ALL string fields must be strings, never null/None. Use "" for empty values"""

_CLASSIFICATION_JSON_SCHEMA = '{{"source_type": "...", "identifier_type": "DOI|arXiv|CorpusID|URL|ISBN|Title|CitationText|None", "identifier_value": "the extracted identifier or empty string", "is_valid": true/false, "validation_reason": "reason if invalid, otherwise empty string", "confidence": "high|medium|low", "rationale": "brief signal explanation"}}'


def get_analyze_reference_prompt(reference: str) -> str:
    """
    Get prompt for analyzing a reference - classifies source type AND extracts identifier in one LLM call.

    Args:
        reference: The reference string to analyze

    Returns:
        Prompt string for the LLM
    """
    return f"""Analyze this bibliographic reference. Classify type and extract the best identifier for lookup.

{_CLASSIFICATION_INSTRUCTIONS}

REFERENCE:
{reference}

Return JSON:
{_CLASSIFICATION_JSON_SCHEMA}"""


def get_batch_analyze_references_prompt(references: List[str]) -> str:
    """
    Get prompt for batch analyzing multiple references in a single LLM call.

    Args:
        references: List of reference strings to analyze

    Returns:
        Prompt string for the LLM to classify all references at once
    """
    refs_text = "\n\n".join(
        f"[{i}] {ref}" for i, ref in enumerate(references)
    )

    return f"""Analyze these {len(references)} bibliographic references. For each one, classify type and extract the best identifier for lookup.

{_CLASSIFICATION_INSTRUCTIONS}
- Classify EACH reference independently

REFERENCES:
{refs_text}

Return JSON with a classification for each reference keyed by its index:
{{"classifications": {{"0": {_CLASSIFICATION_JSON_SCHEMA}, "1": {{...}}, ...}}}}"""


def get_batch_extract_minimal_metadata_prompt(citations: List[str], allowed_topics: Optional[List[str]] = None) -> str:
    """
    Get prompt for batch extracting minimal metadata from multiple unresolvable citations.

    Args:
        citations: List of citation text strings
        allowed_topics: Optional list of allowed topic names. When provided, topics must be selected from this list.

    Returns:
        Prompt string for the LLM
    """
    citations_text = "\n\n".join(
        f"[{i}] {cit}" for i, cit in enumerate(citations)
    )

    if allowed_topics:
        topic_instruction = f"1-3 topics selected ONLY from this list: {', '.join(allowed_topics)}. If none fit, use \"Uncategorized\". Do NOT invent new topics."
    else:
        topic_instruction = "1-3 broad topic nouns"

    return f"""Extract bibliographic data from each of these {len(citations)} citations.

For each citation, extract: author names, year (4-digit), title, venue/publisher, a one-sentence summary, and {topic_instruction}.

IMPORTANT: ALL values must be strings, never null. Use "Unknown" for missing year, "Untitled Citation" for missing title, "" for missing venue.

CITATIONS:
{citations_text}

Return JSON keyed by index:
{{"extractions": {{"0": {{"title": "...", "authors": ["..."], "year": "YYYY or Unknown", "publication_venue": "venue or empty string", "summary": "...", "topics": ["..."]}}, "1": {{...}}, ...}}}}"""


def get_generate_note_prompt(source_type: str, content: str, allowed_topics: Optional[List[str]] = None) -> str:
    """Get prompt for generating notes from content.

    Args:
        source_type: Type of source (Research Paper, Article, etc.)
        content: The content to generate notes from
        allowed_topics: Optional list of allowed topic names. When provided, topics must be selected from this list.
    """
    if source_type == "Research Paper":
        sections = "Hypothesis|Methodology|Main Findings"
    else:
        sections = "Main Story|Credibility|Main Findings"

    if allowed_topics:
        topic_instruction = f"Topics: Select 1-3 from ONLY this list: {', '.join(allowed_topics)}. If none fit, use \"Uncategorized\". Do NOT invent new topics."
    else:
        topic_instruction = "Topics: 1-3 broad single-noun topics"

    return f"""Generate structured notes from this {source_type}.

CONSTRAINTS:
- Summary: ~50 words, practical findings, clear language
- Sections: {sections} (max 200 words each)
- {topic_instruction}
- Use ONLY the content provided

CONTENT:
{content}

Return JSON: {{"summary": "...", "topics": [...], "body_sections": {{...}}}}"""
