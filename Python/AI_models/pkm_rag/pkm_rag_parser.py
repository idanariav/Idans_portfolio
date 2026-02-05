"""
Obsidian markdown parser for PKM RAG.

Extracts YAML frontmatter metadata and Notes section content from Obsidian
markdown files, cleaning wiki-links and dataview inline fields.
"""

import re
from dataclasses import dataclass
from pathlib import Path
from typing import Optional

import yaml

from pkm_rag_constants import (
    DATAVIEW_FIELD_PATTERN,
    PROPERTY_WIKILINK_PATTERN,
    WIKILINK_PATTERN,
)


@dataclass
class ParsedNote:
    """Parsed Obsidian note with metadata and cleaned content."""

    uuid: str
    modified: str
    title: str
    description: str
    aliases: list[str]
    content: str
    file_path: str
    outgoing_links: list[str]


def parse_frontmatter(text: str) -> dict:
    """Extract YAML frontmatter from markdown text.

    Args:
        text: Full markdown file content.

    Returns:
        Dict of frontmatter fields, empty dict if missing or invalid.
    """
    if not text.startswith("---"):
        return {}
    end_idx = text.find("---", 3)
    if end_idx == -1:
        return {}
    try:
        return yaml.safe_load(text[3:end_idx]) or {}
    except yaml.YAMLError:
        return {}


def extract_notes_section(text: str) -> Optional[str]:
    """Extract content under the '## Notes' heading.

    Captures everything from '## Notes' until the next same-level heading
    or end of file.

    Args:
        text: Full markdown file content.

    Returns:
        Notes section content, or None if not found or empty.
    """
    match = re.search(r'(?:^|\n)## Notes\s*\n(.*?)(?=\n## |\Z)', text, re.DOTALL)
    if not match:
        return None
    content = match.group(1).strip()
    return content if content else None


def extract_wikilinks(text: str) -> list[str]:
    """Extract target note titles from Obsidian wikilinks.

    Extracts both standard wikilinks and property wikilinks, returning
    the linked note titles (not display text).

    Args:
        text: Text containing Obsidian wikilink syntax.

    Returns:
        List of unique note titles that are linked to.
    """
    links: set[str] = set()

    # Extract from property wiki-links: (Jump:: [[Target|display]]) or (Jump:: [[Target]])
    for match in re.finditer(r'\([A-Za-z]+::\s*\[\[([^\]|]+)(?:\|[^\]]*)?\]\]', text):
        links.add(match.group(1).strip())

    # Extract from standard wiki-links: [[Target|display]] or [[Target]]
    for match in re.finditer(r'\[\[([^\]|]+)(?:\|[^\]]*)?\]\]', text):
        links.add(match.group(1).strip())

    return sorted(links)


def clean_wikilinks(text: str) -> str:
    """Strip Obsidian wiki-links and dataview fields to plain display text.

    Transformations:
        (Jump:: [[Responsibility|responsible]]) -> responsible
        [[Target|display]] -> display
        [[Target]] -> Target
        Dataview inline fields (key:: value) at line start are stripped.

    Args:
        text: Text containing Obsidian syntax.

    Returns:
        Cleaned plain text.
    """
    # Property wiki-links first: (Jump:: [[X|y]]) -> y
    text = re.sub(PROPERTY_WIKILINK_PATTERN, r'\1', text)
    # Standard wiki-links: [[X|y]] -> y, [[X]] -> X
    text = re.sub(WIKILINK_PATTERN, r'\1', text)
    # Dataview inline fields at line start
    text = re.sub(DATAVIEW_FIELD_PATTERN, '', text, flags=re.MULTILINE)
    return text


def _normalize_list(value) -> list[str]:
    """Normalize a frontmatter value to a list of strings."""
    if isinstance(value, str):
        return [value]
    if isinstance(value, list):
        return [str(v).strip() for v in value if v]
    return []


def parse_note(file_path: str) -> Optional[ParsedNote]:
    """Parse a single Obsidian markdown file.

    Extracts frontmatter metadata and the cleaned Notes section content.
    Skips files without a UUID or without a Notes section.

    Args:
        file_path: Absolute path to the .md file.

    Returns:
        ParsedNote if valid, None otherwise.
    """
    text = Path(file_path).read_text(encoding="utf-8")

    frontmatter = parse_frontmatter(text)
    uuid = str(frontmatter.get("UUID", ""))
    if not uuid:
        return None

    notes_content = extract_notes_section(text)
    if not notes_content:
        return None

    # Extract links before cleaning
    outgoing_links = extract_wikilinks(notes_content)

    return ParsedNote(
        uuid=uuid,
        modified=str(frontmatter.get("Modified", "")),
        title=Path(file_path).stem,
        description=str(frontmatter.get("Description", "") or ""),
        aliases=_normalize_list(frontmatter.get("aliases")),
        content=clean_wikilinks(notes_content),
        file_path=file_path,
        outgoing_links=outgoing_links,
    )
