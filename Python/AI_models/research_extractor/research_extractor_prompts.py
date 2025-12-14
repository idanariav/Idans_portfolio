"""
Prompts for the research extractor pipeline.
"""


def get_classify_source_prompt(reference: str) -> str:
    """
    Get prompt for classifying the source type of a reference.
    
    Args:
        reference: The reference string to classify
    
    Returns:
        Prompt string for the LLM
    """
    return f"""
Classify this reference as one of:
Research Paper, Article, Book, Lecture, Post, Quote.

Return JSON {{ "source_type": "..." }}

Reference:
{reference}
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
        return f"""
From this research paper reference, extract the MAIN identifier in this priority order:
1. DOI (Digital Object Identifier) - looks like 10.xxxx/xxxxx
2. CorpusID - a numeric ID from Semantic Scholar
3. ArXiv ID - looks like arXiv:1234.5678 or similar
4. If none of the above exist, extract the paper TITLE

Return JSON with:
- "identifier_type": one of "DOI", "CorpusID", "ArXiv", or "Title"
- "identifier_value": the actual value

Reference:
{reference}
"""
    else:
        return f"""
From this reference, extract the main TITLE of the content.

Return JSON with:
- "identifier_type": "Title"
- "identifier_value": the title of the content

Reference:
{reference}
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
        sections_instruction = """
Sections to create:
- Hypothesis
- Methodology
- Main Findings
"""
        content_focus = "This is a research paper."
    elif source_type == "Article":
        sections_instruction = """
Sections to create:
- Main Story
- Credibility
- Main Findings
"""
        content_focus = "This is a website article."
    else:  # Lecture / Quote / Talk
        sections_instruction = """
Sections to create:
- Main Story
- Credibility
- Main Findings
"""
        content_focus = "This is a lecture, quote, or talk."
    
    return f"""
You are analyzing a {source_type}. {content_focus}

Using ONLY the content below:

1. Write a ~50 word summary focused on PRACTICAL FINDINGS. Use simple, clear language without jargon.
2. Create structured sections.

{sections_instruction}

Each section <= 200 words.

3. Suggest 1–3 broad topics as single nouns.

Return JSON {{summary, topics, body_sections}}

CONTENT:
{content}
"""
