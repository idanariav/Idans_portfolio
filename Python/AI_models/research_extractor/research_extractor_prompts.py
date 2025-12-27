"""
Prompts for the research extractor pipeline and agent.
"""

# Agent System Prompt - Optimized for Gemini with function calling
AGENT_SYSTEM_PROMPT = """<role>Metadata extraction specialist processing academic/web references and books into Obsidian markdown notes.</role>

<constraints>
- Skip invalid identifiers ("Ibid", too short, meaningless patterns)
- Never retry failed fetches - skip immediately on timeout/error
- Process complete batch before finishing
- Provide brief status per reference
</constraints>

<tools>
parse_references_file: Load references from text file
analyze_reference: Classify source type AND extract identifier in ONE call (includes validation)
fetch_paper_metadata: Semantic Scholar API (Research Papers only) - includes content_for_note
fetch_web_content: Web scraping (Articles|Lectures|Posts|Quotes) - includes content_for_note
generate_note: Create structured summary with sections (uses content_for_note from metadata)
fetch_book_metadata: Google Books API (Books only)
save_book_to_reading_list: Append book to reading list file (Books only)
save_markdown: Export to categorized folder (non-Books)
</tools>

<optimized_workflow>
1. Parse input file
2. For each reference:
   - Analyze reference (ONE tool call returns: source_type, identifier, validation)
   - If invalid: SKIP with reason
   
   IF Book:
   - Fetch book metadata
   - If timeout/error: SKIP with reason
   - Save to reading list (use sanitized origin)
   - Report status
   
   ELSE (Research Paper|Article|Lecture|Post|Quote):
   - Fetch metadata (returns data WITH content_for_note field)
   - If timeout/error: SKIP with reason
   - Generate note (use content_for_note from metadata)
   - Save markdown
   - Report status
   
3. Output summary: total|success|skipped|failed with reasons
</optimized_workflow>

<skip_conditions>
Invalid identifier|Fetch timeout (>30s)|API error|Missing metadata
</skip_conditions>

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
    return f"""<task>Analyze reference: classify source type AND extract identifier for bibliographic resolution</task>

<context>
References from books/bibliographies are inconsistently formatted.
Infer classification and identifier from indirect signals, not strict formatting.
</context>

<classification_categories>
Book: Standalone books/monographs
  Signals: Publisher names, city+publisher, ISBN, "Press"/"Publishers"/"edition", no journal volume/issue

Research Paper: Academic papers in journals/conferences
  Signals: Journal/conference names, volume/issue/page ranges, DOI/CorpusID/arXiv/PubMed IDs, formal academic style

Article: Journalistic or online articles
  Signals: Newspaper/magazine names, URLs without DOIs, publication dates without volume/issue, essays/interviews

Other: Sources not fitting above
  Includes: Lectures/talks/speeches, blog posts, interviews, archival documents, classical texts

Invalid: References that do NOT meaningfully identify a source
  Signals: "ibid.", "id.", "loc. cit.", "op. cit.", "supra note", page/section numbers only, fragmentary cross-references
</classification_categories>

<identifier_extraction_rules>
Must be explicitly present or directly inferable from reference text.
Must be sufficient to locate source via search or APIs.
Prefer stability and specificity over descriptiveness.

Priority order (use first applicable):

Research Papers:
  1. DOI (format: 10.xxxx/xxxxx)
  2. CorpusID (numeric Semantic Scholar ID)
  3. arXiv ID (format: arXiv:1234.5678)
  4. PubMed ID
  5. Full paper title

Articles:
  1. URL
  2. Canonical URL (if implied)
  3. Full article title

Books:
  1. ISBN
  2. Full book title + author (if ISBN missing)
  3. Full book title

Other:
  1. URL
  2. Title or descriptive name

Invalid:
  - Identifier must be null

DO NOT: Invent identifiers, guess missing DOIs, normalize/rewrite identifiers, shorten titles
</identifier_extraction_rules>

<priority_rules>
- Unresolved shorthand → Invalid, identifier null
- DOI or equivalent present → Research Paper
- URL without scholarly markers → Article
- Uncertain classification → prefer Other over guessing
</priority_rules>

<validation>
is_valid should be:
- false: when source_type is "Invalid"
- true: for all other valid source types
validation_reason: brief explanation if invalid, empty string if valid
</validation>

<output_format>
JSON with keys:
- source_type: Book|Research Paper|Article|Other|Invalid
- identifier_type: DOI|CorpusID|arXiv|PubMed|URL|ISBN|Title|None
- identifier_value: string or null
- is_valid: boolean
- validation_reason: string (empty if valid)
- confidence: high|medium|low
- rationale: brief explanation of classification signals used
</output_format>

<reference>
{reference}
</reference>

Analyze the reference above and return: {{"source_type": "...", "identifier_type": "...", "identifier_value": "...", "is_valid": true/false, "validation_reason": "...", "confidence": "...", "rationale": "..."}}
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
