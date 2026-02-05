"""
Streamlit interface for PKM RAG.

Provides multiple modes:
- Ask: Chat-based Q&A grounded in note context
- Find Similar: Pure vector similarity to discover related notes
"""

import streamlit as st
import ollama

from pkm_rag_constants import (
    CHAT_MODEL,
    ENABLE_QUERY_REWRITE,
    GAP_ANALYSIS_TOP_K,
    PAGE_TITLE,
    SIMILAR_TOP_K,
    SIMILARITY_THRESHOLD,
    TOP_K,
)
from pkm_rag_embed import get_collection
from pkm_rag_prompts import (
    EXPLORE_SYSTEM_PROMPT,
    QUERY_REWRITE_PROMPT,
    SYSTEM_PROMPT,
    GAP_SYSTEM_PROMPT,
    STRESS_TEST_SYSTEM_PROMPT,
    format_explore_prompt,
    format_gap_prompt,
    format_rag_prompt,
    format_stress_test_prompt,
)


def retrieve_context(
    query: str, collection, n_results: int = TOP_K
) -> tuple[str, list[dict]]:
    """Retrieve relevant chunks from ChromaDB via semantic search.

    Args:
        query: User's question.
        collection: ChromaDB collection with embedded notes.

    Returns:
        Tuple of (formatted context string, list of unique source metadata dicts).
    """
    results = collection.query(
        query_texts=[query],
        n_results=n_results,
        include=["documents", "metadatas", "distances"],
    )

    context_parts: list[str] = []
    sources: list[dict] = []
    seen_titles: set[str] = set()

    for doc, meta, distance in zip(
        results["documents"][0],
        results["metadatas"][0],
        results["distances"][0],
    ):
        if distance > SIMILARITY_THRESHOLD:
            continue

        title = meta.get("title", "Unknown")
        description = meta.get("description", "")

        header = f"[Source: {title}]"
        if description:
            header += f" | {description}"
        context_parts.append(f"{header}\n{doc}")

        if title not in seen_titles:
            seen_titles.add(title)
            sources.append({
                "title": title,
                "description": description,
            })

    formatted_context = "\n\n---\n\n".join(context_parts)
    return formatted_context, sources


def find_similar_notes(title: str, collection) -> list[dict]:
    """Find notes semantically similar to the given note title.

    Retrieves the target note's chunks by metadata filter, uses the first
    chunk as a query, and returns similar notes excluding the source note.

    Args:
        title: Exact note title to find similarities for.
        collection: ChromaDB collection.

    Returns:
        List of dicts with title, description, similarity score.
        Empty list if the note is not found.
    """
    target = collection.get(
        where={"title": title},
        include=["documents", "metadatas"],
    )

    if not target["documents"]:
        return []

    target_uuid = target["metadatas"][0].get("uuid", "")
    query_text = target["documents"][0]

    results = collection.query(
        query_texts=[query_text],
        n_results=SIMILAR_TOP_K + 10,  # over-fetch to account for self-note filtering
        include=["metadatas", "distances"],
    )

    similar: list[dict] = []
    seen_titles: set[str] = set()

    for meta, distance in zip(
        results["metadatas"][0],
        results["distances"][0],
    ):
        note_uuid = meta.get("uuid", "")
        note_title = meta.get("title", "Unknown")

        if note_uuid == target_uuid:
            continue
        if note_title in seen_titles:
            continue

        seen_titles.add(note_title)
        similar.append({
            "title": note_title,
            "description": meta.get("description", ""),
            "similarity": round(1 - distance, 3),
        })

        if len(similar) >= SIMILAR_TOP_K:
            break

    return similar


def query_llm(prompt: str, system_prompt: str = SYSTEM_PROMPT) -> str:
    """Query llama3.1:8b via Ollama with the given prompt.

    Args:
        prompt: Complete prompt with RAG context.
        system_prompt: System instruction for the LLM.

    Returns:
        Model response text.
    """
    response = ollama.chat(
        model=CHAT_MODEL,
        messages=[
            {"role": "system", "content": system_prompt},
            {"role": "user", "content": prompt},
        ],
    )
    return response["message"]["content"]


def render_sources(sources: list[dict]) -> None:
    """Render source notes in a collapsible expander."""
    if not sources:
        return
    with st.expander(f"Sources ({len(sources)} notes)", expanded=False):
        for src in sources:
            st.markdown(f"**{src['title']}**")
            if src["description"]:
                st.caption(src["description"])


def rewrite_query(question: str) -> str:
    """Use the LLM to expand a query with related terms for better retrieval.

    Args:
        question: Original user question.

    Returns:
        Rewritten query string, or original question if rewrite fails.
    """
    try:
        response = ollama.chat(
            model=CHAT_MODEL,
            messages=[{
                "role": "user",
                "content": QUERY_REWRITE_PROMPT.format(question=question),
            }],
        )
        rewritten = response["message"]["content"].strip()
        return rewritten if rewritten else question
    except Exception:
        return question


def run_ask_mode(collection) -> None:
    """Run the Ask (Q&A) mode with chat interface."""
    st.caption("💡 **Tip:** Phrase questions directly (e.g., \"What is agency?\") rather than meta-questions (e.g., \"Do I have a note about agency?\") for better retrieval.")

    # Query rewriting toggle
    enable_rewrite = st.checkbox(
        "Enable query expansion",
        value=ENABLE_QUERY_REWRITE,
        help="Expand queries with related terms for broader retrieval (may reduce precision)"
    )

    if "messages" not in st.session_state:
        st.session_state.messages = []

    for msg in st.session_state.messages:
        with st.chat_message(msg["role"]):
            st.markdown(msg["content"])
            if msg.get("sources"):
                render_sources(msg["sources"])

    if question := st.chat_input("Ask about your notes..."):
        st.session_state.messages.append({"role": "user", "content": question})
        with st.chat_message("user"):
            st.markdown(question)

        with st.chat_message("assistant"):
            # Optional query rewriting for better retrieval
            search_query = question
            if enable_rewrite:
                with st.spinner("Rewriting query..."):
                    search_query = rewrite_query(question)
                if search_query != question:
                    st.caption(f"🔍 Searched for: {search_query}")

            with st.spinner("Searching notes..."):
                context, sources = retrieve_context(search_query, collection)

            if not context:
                answer = "I don't have information about that in my notes."
            else:
                # Use original question in RAG prompt, not the rewritten one
                prompt = format_rag_prompt(context, question)
                answer = query_llm(prompt)

            st.markdown(answer)
            render_sources(sources)

            st.session_state.messages.append({
                "role": "assistant",
                "content": answer,
                "sources": sources,
            })


def run_similar_mode(collection) -> None:
    """Run the Find Similar mode."""
    st.subheader("Find Similar Notes")
    st.caption("Enter a note title to find semantically similar notes. No LLM call — pure vector similarity.")

    title = st.text_input("Note title", placeholder="e.g. Agency")

    if st.button("Find Similar", disabled=not title):
        with st.spinner("Searching..."):
            similar = find_similar_notes(title.strip(), collection)

        if not similar:
            st.warning(f"No note found with title \"{title}\". Make sure the title matches exactly.")
        else:
            st.success(f"Found {len(similar)} similar notes to **{title}**")
            for i, note in enumerate(similar, 1):
                col1, col2 = st.columns([4, 1])
                with col1:
                    st.markdown(f"**{i}. {note['title']}**")
                    if note["description"]:
                        st.caption(note["description"])
                with col2:
                    st.metric("Similarity", f"{note['similarity']:.1%}")


def run_connect_mode(collection) -> None:
    """Run the Concept Connection mode."""
    st.subheader("Connect Concepts")
    st.caption("Enter 2+ concept names (comma-separated) to discover how they relate.")

    concepts_input = st.text_input("Concepts", placeholder="e.g. Agency, Second order thinking")

    if st.button("Explore", disabled=not concepts_input):
        concepts = [c.strip() for c in concepts_input.split(",") if c.strip()]
        if len(concepts) < 2:
            st.warning("Enter at least 2 concepts separated by commas.")
            return

        concept_contexts: dict[str, str] = {}
        all_sources: list[dict] = []

        with st.spinner("Retrieving context for each concept..."):
            for concept in concepts:
                context, sources = retrieve_context(concept, collection)
                concept_contexts[concept] = context if context else "No notes found."
                all_sources.extend(sources)

        prompt = format_explore_prompt(concept_contexts)
        with st.spinner("Analyzing connections..."):
            answer = query_llm(prompt, system_prompt=EXPLORE_SYSTEM_PROMPT)

        st.markdown(answer)

        # Deduplicate sources across all concepts
        seen: set[str] = set()
        unique_sources: list[dict] = []
        for src in all_sources:
            if src["title"] not in seen:
                seen.add(src["title"])
                unique_sources.append(src)
        render_sources(unique_sources)


def run_gap_mode(collection) -> None:
    """Run the Gap Analysis mode."""
    st.subheader("Gap Analysis")
    st.caption("Enter a topic to identify what's missing or underrepresented in your notes.")

    topic = st.text_input("Topic", placeholder="e.g. Free Will")

    if st.button("Analyze Gaps", disabled=not topic):
        with st.spinner("Retrieving broad context..."):
            context, sources = retrieve_context(
                topic.strip(), collection, n_results=GAP_ANALYSIS_TOP_K
            )

        if not context:
            st.warning(f"No notes found related to \"{topic}\".")
            return

        prompt = format_gap_prompt(context, topic.strip())
        with st.spinner("Analyzing coverage gaps..."):
            answer = query_llm(prompt, system_prompt=GAP_SYSTEM_PROMPT)

        st.markdown(answer)
        render_sources(sources)


def run_devils_advocate_mode(collection) -> None:
    """Run the Devil's Advocate mode."""
    st.subheader("Devil's Advocate")
    st.caption("Enter a note title to challenge its reasoning using your own notes as evidence.")

    title = st.text_input("Note title", placeholder="e.g. Agency", key="devils_advocate_title")

    if st.button("Challenge", disabled=not title):
        title = title.strip()

        # Fetch the target note's chunks
        with st.spinner("Fetching note..."):
            target = collection.get(
                where={"title": title},
                include=["documents", "metadatas"],
            )

        if not target["documents"]:
            st.warning(f"No note found with title \"{title}\". Make sure the title matches exactly.")
            return

        target_uuid = target["metadatas"][0].get("uuid", "")
        note_context = "\n\n".join(target["documents"])

        # Semantic search for related notes using the note's content
        with st.spinner("Finding related perspectives..."):
            results = collection.query(
                query_texts=[target["documents"][0]],
                n_results=TOP_K + 5,
                include=["documents", "metadatas", "distances"],
            )

        related_parts: list[str] = []
        sources: list[dict] = [{"title": title, "description": target["metadatas"][0].get("description", "")}]
        seen_titles: set[str] = {title}

        for doc, meta, distance in zip(
            results["documents"][0],
            results["metadatas"][0],
            results["distances"][0],
        ):
            if meta.get("uuid") == target_uuid:
                continue
            if distance > SIMILARITY_THRESHOLD:
                continue

            rel_title = meta.get("title", "Unknown")
            description = meta.get("description", "")

            header = f"[Source: {rel_title}]"
            if description:
                header += f"\nDescription: {description}"
            related_parts.append(f"{header}\n{doc}")

            if rel_title not in seen_titles:
                seen_titles.add(rel_title)
                sources.append({"title": rel_title, "description": description})

        related_context = "\n\n---\n\n".join(related_parts)

        prompt = format_stress_test_prompt(title, note_context, related_context)
        with st.spinner("Analyzing..."):
            answer = query_llm(prompt, system_prompt=STRESS_TEST_SYSTEM_PROMPT)

        st.markdown(answer)
        render_sources(sources)


def main():
    """Main Streamlit app entry point."""
    st.set_page_config(page_title=PAGE_TITLE, layout="centered")
    st.title(PAGE_TITLE)

    if "collection" not in st.session_state:
        st.session_state.collection = get_collection()

    mode = st.sidebar.radio("Mode", ["Ask", "Find Similar", "Connect", "Gap Analysis", "Devil's Advocate"])

    if mode == "Ask":
        run_ask_mode(st.session_state.collection)
    elif mode == "Find Similar":
        run_similar_mode(st.session_state.collection)
    elif mode == "Connect":
        run_connect_mode(st.session_state.collection)
    elif mode == "Gap Analysis":
        run_gap_mode(st.session_state.collection)
    elif mode == "Devil's Advocate":
        run_devils_advocate_mode(st.session_state.collection)


if __name__ == "__main__":
    main()
