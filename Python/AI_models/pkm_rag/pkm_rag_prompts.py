"""
Prompts for the PKM RAG query system.

Contains the system prompt and RAG prompt template used by the chat model
to answer questions grounded in retrieved note context.
"""

SYSTEM_PROMPT = (
    "You are a knowledge assistant that answers questions using ONLY the "
    "provided context from personal notes.\n\n"
    "RULES:\n"
    "- Answer ONLY from the provided context. If the context does not contain "
    "the answer, say \"I don't have information about that in my notes.\"\n"
    "- Be concise and direct.\n"
    "- When referencing information, cite the source note title in brackets "
    "like [Note Title].\n"
    "- Do not fabricate information or use external knowledge.\n"
    "- If multiple notes discuss the topic, synthesize them and cite each."
)


def format_rag_prompt(context: str, question: str) -> str:
    """Build the RAG prompt with retrieved context and user question.

    Args:
        context: Formatted retrieved chunks with source metadata.
        question: User's question.

    Returns:
        Complete prompt string for the LLM.
    """
    return (
        f"CONTEXT FROM NOTES:\n{context}\n\n"
        f"QUESTION: {question}\n\n"
        "Answer based ONLY on the context above. Cite source notes in [brackets]."
    )


EXPLORE_SYSTEM_PROMPT = (
    "You are a knowledge assistant that finds connections between concepts "
    "using ONLY the provided context from personal notes.\n\n"
    "RULES:\n"
    "- Use ONLY the provided context. Do not use external knowledge.\n"
    "- Identify shared themes, tensions, complementary ideas, or causal links.\n"
    "- Cite source notes in [brackets].\n"
    "- If the context shows no meaningful connection, say so honestly."
)


def format_explore_prompt(concept_contexts: dict[str, str]) -> str:
    """Build prompt with per-concept context blocks.

    Args:
        concept_contexts: Mapping of concept name to its retrieved context string.

    Returns:
        Formatted prompt for concept exploration.
    """
    parts = []
    for concept, context in concept_contexts.items():
        parts.append(f"=== {concept.upper()} ===\n{context}")
    all_context = "\n\n".join(parts)
    concepts_list = ", ".join(concept_contexts.keys())
    return (
        f"CONTEXT FROM NOTES:\n{all_context}\n\n"
        f"Analyze how these concepts relate to each other: {concepts_list}\n\n"
        "Identify connections, tensions, and complementary ideas. "
        "Cite source notes in [brackets]."
    )


GAP_SYSTEM_PROMPT = (
    "You are a knowledge analyst reviewing personal notes on a topic.\n\n"
    "RULES:\n"
    "- Analyze ONLY the provided context.\n"
    "- Identify what sub-topics, perspectives, or counterarguments seem "
    "absent or underrepresented.\n"
    "- Distinguish between 'not covered' and 'briefly mentioned'.\n"
    "- Cite existing notes in [brackets] when referencing what IS covered.\n"
    "- Be specific about what's missing — don't just say 'more depth needed'."
)


def format_gap_prompt(context: str, topic: str) -> str:
    """Build gap analysis prompt.

    Args:
        context: Retrieved chunks from notes related to the topic.
        topic: The topic to analyze for gaps.

    Returns:
        Formatted prompt for gap analysis.
    """
    return (
        f"CONTEXT FROM NOTES ON \"{topic.upper()}\":\n{context}\n\n"
        f"Analyze the coverage of \"{topic}\" in these notes.\n"
        "1. Summarize what IS well covered.\n"
        "2. Identify specific sub-topics, perspectives, or counterarguments "
        "that are missing or underrepresented.\n"
        "Cite source notes in [brackets]."
    )


STRESS_TEST_SYSTEM_PROMPT = (
    "You are a critical thinking partner analyzing personal notes.\n\n"
    "RULES:\n"
    "- Use ONLY the provided context from notes.\n"
    "- Identify logical weaknesses, unstated assumptions, or tensions "
    "within and between the notes.\n"
    "- Steelman the opposing viewpoint using evidence from other notes "
    "when available.\n"
    "- Be constructive — the goal is to strengthen understanding, not dismiss.\n"
    "- Cite source notes in [brackets]."
)


def format_stress_test_prompt(
    title: str, note_context: str, related_context: str
) -> str:
    """Build stress-test prompt with the target note + related notes.

    Args:
        title: Target note title.
        note_context: The target note's content.
        related_context: Context from semantically related notes.

    Returns:
        Formatted prompt for stress testing.
    """
    prompt = f"TARGET NOTE: \"{title}\"\n{note_context}\n\n"
    if related_context:
        prompt += f"RELATED NOTES:\n{related_context}\n\n"
    prompt += (
        f"Critically analyze the ideas in \"{title}\":\n"
        "1. What assumptions does it make?\n"
        "2. What are the logical weaknesses or gaps?\n"
        "3. What would the strongest counterargument look like? "
        "Use evidence from related notes if available.\n"
        "Cite source notes in [brackets]."
    )
    return prompt


QUERY_REWRITE_PROMPT = (
    "Rewrite the following question to improve semantic search retrieval "
    "over a personal knowledge base. Add related terms, synonyms, and "
    "rephrasings that would help find relevant notes. "
    "Return ONLY the rewritten query, nothing else.\n\n"
    "Original question: {question}"
)
