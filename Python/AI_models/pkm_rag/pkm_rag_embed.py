"""
Embedding pipeline for PKM RAG.

Scans an Obsidian vault, detects new/modified files via UUID + Modified date,
chunks content, embeds with nomic-embed-text via Ollama, and upserts to ChromaDB.
Supports incremental updates and deletion detection.
"""

import os
import shutil
from pathlib import Path

import chromadb
from chromadb.utils.embedding_functions import OllamaEmbeddingFunction
from dotenv import load_dotenv
from langchain_text_splitters import RecursiveCharacterTextSplitter

from pkm_rag_constants import (
    CHUNK_OVERLAP,
    CHUNK_SEPARATORS,
    CHUNK_SIZE,
    COLLECTION_NAME,
    DEFAULT_CHROMA_DB_PATH,
    EMBED_MODEL,
    MIN_CHUNK_LENGTH,
)
from pkm_rag_parser import ParsedNote, parse_note


def get_collection(db_path: str | None = None) -> chromadb.Collection:
    """Get or create the ChromaDB collection with Ollama embedding function.

    Args:
        db_path: Path for persistent ChromaDB storage. Falls back to env/default.

    Returns:
        ChromaDB Collection configured with nomic-embed-text embeddings.
    """
    db_path = db_path or os.getenv("CHROMA_DB_PATH", DEFAULT_CHROMA_DB_PATH)
    client = chromadb.PersistentClient(path=db_path)
    embed_fn = OllamaEmbeddingFunction(
        model_name=EMBED_MODEL,
        url="http://localhost:11434/api/embeddings",
    )
    return client.get_or_create_collection(
        name=COLLECTION_NAME,
        embedding_function=embed_fn,
        metadata={"hnsw:space": "cosine"},
    )


def get_embedded_state(collection: chromadb.Collection) -> dict[str, str]:
    """Build a UUID -> Modified lookup from all currently embedded documents.

    Args:
        collection: ChromaDB collection.

    Returns:
        Dict mapping note UUID to its Modified date string.
    """
    results = collection.get(include=["metadatas"])
    state: dict[str, str] = {}
    for meta in results["metadatas"]:
        uuid = meta.get("uuid", "")
        if uuid:
            state[uuid] = meta.get("modified", "")
    return state


def chunk_note(note: ParsedNote) -> list[dict]:
    """Split a parsed note into chunks with attached metadata.

    The note's Description is prepended to content so every chunk
    (especially the first) carries the semantic summary.

    Args:
        note: Parsed note with content and metadata.

    Returns:
        List of dicts with keys: id, text, metadata.
    """
    full_text = note.content
    if note.description:
        full_text = f"{note.description}\n\n{note.content}"

    if len(full_text.strip()) < MIN_CHUNK_LENGTH:
        return []

    splitter = RecursiveCharacterTextSplitter(
        chunk_size=CHUNK_SIZE,
        chunk_overlap=CHUNK_OVERLAP,
        length_function=len,
        separators=CHUNK_SEPARATORS,
    )
    texts = splitter.split_text(full_text)

    chunks = []
    for i, text in enumerate(texts):
        metadata = {
            "uuid": note.uuid,
            "modified": note.modified,
            "title": note.title,
            "description": note.description[:500] if note.description else "",
            "aliases": ", ".join(note.aliases),
            "outgoing_links": ", ".join(note.outgoing_links),
            "chunk_index": i,
            "total_chunks": len(texts),
            "file_path": note.file_path,
        }
        chunks.append({
            "id": f"{note.uuid}_chunk_{i}",
            "text": text,
            "metadata": metadata,
        })
    return chunks


def scan_vault(vault_path: str | None = None) -> list[str]:
    """Recursively find all .md files in the vault.

    Args:
        vault_path: Root path of the Obsidian vault. Falls back to VAULT_PATH env var.

    Returns:
        List of absolute file paths.

    Raises:
        ValueError: If vault_path is not provided and VAULT_PATH env var is not set.
    """
    vault_path = vault_path or os.getenv("VAULT_PATH")
    if not vault_path:
        raise ValueError(
            "Vault path not provided. Set VAULT_PATH environment variable in .env file."
        )
    return [str(p) for p in Path(vault_path).rglob("*.md")]


def _delete_note_chunks(collection: chromadb.Collection, uuid: str) -> None:
    """Delete all chunks for a given note UUID from the collection."""
    collection.delete(where={"uuid": uuid})


def run_embed(
    vault_path: str | None = None,
    db_path: str | None = None,
    file_paths: list[str] | None = None,
    force_embed: bool = False,
) -> dict:
    """Run the full embedding pipeline with incremental updates.

    Flow:
        1. Optionally clear existing database if force_embed=True
        2. Scan vault for .md files (or use provided file_paths)
        3. Load existing embedded state from ChromaDB
        4. For each file: skip unchanged, delete+re-embed modified, embed new
        5. Remove chunks for notes deleted from vault (only on full scan)

    Args:
        vault_path: Obsidian vault root path.
        db_path: ChromaDB storage path.
        file_paths: Specific file paths to embed. Skips vault scan and
            deletion detection when provided.
        force_embed: If True, clears ChromaDB and re-embeds all files.
            Useful after changing chunking configuration.

    Returns:
        Stats dict with counts: new, updated, unchanged, skipped, deleted, errors.
    """
    db_path = db_path or os.getenv("CHROMA_DB_PATH", DEFAULT_CHROMA_DB_PATH)

    # Clear database if force_embed is True
    if force_embed and Path(db_path).exists():
        print(f"Force embed enabled - clearing {db_path}...")
        shutil.rmtree(db_path)

    collection = get_collection(db_path)
    embedded_state = get_embedded_state(collection)
    files = file_paths if file_paths is not None else scan_vault(vault_path)

    stats = {
        "new": 0, "updated": 0, "unchanged": 0,
        "skipped": 0, "deleted": 0, "errors": 0,
    }
    seen_uuids: set[str] = set()

    for file_path in files:
        try:
            note = parse_note(file_path)
            if note is None:
                stats["skipped"] += 1
                continue

            seen_uuids.add(note.uuid)

            existing_modified = embedded_state.get(note.uuid)
            if existing_modified == note.modified:
                stats["unchanged"] += 1
                continue

            if existing_modified is not None:
                _delete_note_chunks(collection, note.uuid)
                stats["updated"] += 1
            else:
                stats["new"] += 1

            chunks = chunk_note(note)
            if not chunks:
                stats["skipped"] += 1
                continue

            collection.upsert(
                ids=[c["id"] for c in chunks],
                documents=[c["text"] for c in chunks],
                metadatas=[c["metadata"] for c in chunks],
            )
        except Exception as e:
            print(f"Error processing {file_path}: {e}")
            stats["errors"] += 1

    # Remove chunks for notes deleted from vault (only on full scan)
    if file_paths is None:
        deleted_uuids = set(embedded_state.keys()) - seen_uuids
        for uuid in deleted_uuids:
            _delete_note_chunks(collection, uuid)
            stats["deleted"] += 1

    _print_stats(stats)
    return stats


def _print_stats(stats: dict) -> None:
    """Print embedding pipeline summary."""
    print("\nEmbedding complete:")
    for key, value in stats.items():
        print(f"  {key.capitalize():>12}: {value}")


if __name__ == "__main__":
    load_dotenv()
    run_embed()
