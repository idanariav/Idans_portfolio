"""
Prompts for the research extractor pipeline and agent.
"""

# Agent System Prompt - Optimized for Gemini 3
AGENT_SYSTEM_PROMPT = """<role>Metadata extraction specialist processing academic/web references and books into Obsidian markdown notes.</role>

<constraints>
- Skip invalid identifiers ("Ibid", too short, meaningless patterns)
- Never retry failed fetches - skip immediately on timeout/error
- Process complete batch before finishing
- Provide brief status per reference
</constraints>

<tools>
parse_references_file: Load references from text file
classify_source_type: Identify as Research Paper|Article|Lecture|Post|Quote|Book
extract_identifier: Get DOI|ArXiv|Title|URL
validate_identifier: Check if meaningful/searchable
fetch_paper_metadata: Semantic Scholar API (Research Papers only)
fetch_web_content: Web scraping (Articles|Lectures|Posts|Quotes)
prepare_content_for_note: Extract content string from metadata dict
generate_note: Create structured summary with sections
fetch_book_metadata: Google Books API (Books only)
save_book_to_reading_list: Append book to reading list file (Books only)
save_markdown: Export to categorized folder (non-Books)
</tools>

<workflow>
1. Parse input file
2. For each reference:
   - Classify source type
   - Extract identifier
   - Validate → if invalid: SKIP with reason
   
   IF Book:
   - Fetch book metadata (use fetch_book_metadata)
   - If timeout/error: SKIP with reason
   - Save to reading list (use save_book_to_reading_list with sanitized origin)
   - Report status
   
   ELSE (Research Paper|Article|Lecture|Post|Quote):
   - Fetch: use fetch_paper_metadata for Research Papers, fetch_web_content for others
   - If timeout/error: SKIP with reason
   - Prepare content: extract text from metadata
   - Generate note
   - Save markdown
   - Report status
   
3. Output summary: total|success|skipped|failed with reasons
</workflow>

<skip_conditions>
Invalid identifier|Fetch timeout (>30s)|API error|Missing metadata
</skip_conditions>

Begin when given: input_file, output_dir, origin.
"""


def get_classify_source_prompt(reference: str) -> str:
    """
    Get prompt for classifying the source type of a reference.
    
    Args:
        reference: The reference string to classify
    
    Returns:
        Prompt string for the LLM
    """
    return f"""<task>Classify reference type</task>

<categories>Research Paper|Article|Book|Lecture|Post|Quote</categories>

<output_format>JSON with key "source_type"</output_format>

<reference>
{reference}
</reference>

Based on the reference above, return: {{"source_type": "..."}}
"""


def get_extract_identifier_prompt(source_type: str, reference: str) -> str:
    """
    Get prompt for extracting the main identifier from a reference.
    
    Args:
        source_type: Type of source (Research Paper, Article, etc.)
        reference: The full reference string
    
    Returns:
        Prompt string for the LLM
    """
    if source_type == "Research Paper":
        return f"""<task>Extract primary identifier from research paper</task>

<priority_order>
1. DOI (format: 10.xxxx/xxxxx)
2. CorpusID (numeric Semantic Scholar ID)
3. ArXiv ID (format: arXiv:1234.5678)
4. Paper TITLE (if no DOI/CorpusID/ArXiv)
</priority_order>

<output_format>
JSON with:
- identifier_type: DOI|CorpusID|ArXiv|Title
- identifier_value: extracted value
</output_format>

<reference>
{reference}
</reference>

Based on the reference above, extract and return: {{"identifier_type": "...", "identifier_value": "..."}}
"""
    else:
        return f"""<task>Extract title from reference</task>

<output_format>
JSON with:
- identifier_type: "Title"
- identifier_value: extracted title
</output_format>

<reference>
{reference}
</reference>

Based on the reference above, return: {{"identifier_type": "Title", "identifier_value": "..."}}
"""


def get_generate_note_prompt(source_type: str, content: str) -> str:
    """
    Get prompt for generating notes from content.
    
    Args:
        source_type: Type of source (Research Paper, Article, etc.)
        content: The content to analyze
    
    Returns:
        Prompt string for the LLM
    """
    # Customize sections based on source type
    if source_type == "Research Paper":
        sections = "Hypothesis|Methodology|Main Findings"
    elif source_type == "Article":
        sections = "Main Story|Credibility|Main Findings"
    else:  # Lecture / Quote / Talk
        sections = "Main Story|Credibility|Main Findings"
    
    return f"""<task>Generate structured notes from {source_type}</task>

<constraints>
- Summary: ~50 words, focus on practical findings, clear language
- Sections: {sections} (max 200 words each)
- Topics: 1-3 broad topics as single nouns
- Use ONLY the content provided below
</constraints>

<output_format>
JSON with keys: summary, topics (array), body_sections (object)
</output_format>

<content>
{content}
</content>

Based on the content above, return: {{"summary": "...", "topics": [...], "body_sections": {{...}}}}
"""
