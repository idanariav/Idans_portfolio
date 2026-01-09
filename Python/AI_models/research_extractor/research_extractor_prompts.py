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

Unresolvable: Valid bibliographic references that CANNOT be resolved even with title search
  Signals: Missing both identifiers AND insufficient title (too vague/incomplete)
  Examples: "Personal communication", "Unpublished manuscript", incomplete citations
  Action: Extract author/year/title from citation text only
  NOTE: If author+title+year are present, classify as Research Paper/Article and try title search first

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

Unresolvable:
  1. Citation text (full reference as-is for manual extraction)

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
- source_type: Book|Research Paper|Article|Other|Unresolvable|Invalid
- identifier_type: DOI|CorpusID|arXiv|PubMed|URL|ISBN|Title|CitationText|None
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
