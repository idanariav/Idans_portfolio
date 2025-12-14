import os
import re
import json
import requests
from dotenv import load_dotenv
from openai import OpenAI
from research_extractor_prompts import (
    get_classify_source_prompt,
    get_extract_identifier_prompt,
    get_generate_note_prompt,
)

load_dotenv()

client = OpenAI(
    api_key=os.getenv("OPENROUTER_API_KEY"),
    base_url="https://openrouter.ai/api/v1",
)

SEMANTIC_URL = "https://api.semanticscholar.org/graph/v1/paper/search"
TAVILY_URL = "https://api.tavily.com/search"
MODEL = "google/gemini-2.5-flash"
FIELDS = "title,authors,year,abstract,url,tldr"
FOLDER_MAP = {
    "Research Paper": "Journals",
    "Article": "Articles",
    "Lecture": "Lectures",
    "Post": "Socials",
    "Quote": "Quotes",
}


def call_llm(prompt, json_format=False):
    """Call LLM with standardized parameters."""
    kwargs = {
        "model": MODEL,
        "messages": [{"role": "user", "content": prompt}],
    }
    if json_format:
        kwargs["response_format"] = {"type": "json_object"}
    return client.chat.completions.create(**kwargs)


def parse_references(text):
    references = [r.strip() for r in re.split(r"\s*\n", text) if r.strip()]
    return references


def classify_source(reference):
    prompt = get_classify_source_prompt(reference)
    resp = call_llm(prompt, json_format=True)
    return json.loads(resp.choices[0].message.content)["source_type"]


def extract_identifier(source_type, reference):
    """Extract main identifier (DOI, CorpusID, ArXiv, or Title)."""
    prompt = get_extract_identifier_prompt(source_type, reference)
    resp = call_llm(prompt, json_format=True)
    return json.loads(resp.choices[0].message.content)


def fetch_research_paper(identifier_info):
    """Fetch research paper from Semantic Scholar API."""
    id_type = identifier_info.get("identifier_type")
    id_value = identifier_info.get("identifier_value")
    
    # Build request based on identifier type
    if id_type in ["DOI", "CorpusID", "ArXiv"]:
        # Direct lookup endpoint
        paper_id = f"{id_type if id_type != 'ArXiv' else 'ARXIV'}:{id_value}"
        url = f"{SEMANTIC_URL.replace('/search', '')}/{paper_id}"
        params = {"fields": FIELDS}
    else:
        # Search endpoint
        url = SEMANTIC_URL
        params = {"query": id_value, "limit": 1, "fields": FIELDS}
    
    try:
        r = requests.get(url, params=params, timeout=20)
        r.raise_for_status()
        data = r.json()
        
        if data.get("error"):
            print(f"API error: {data['error']}")
            return None
        
        # Handle both direct lookup (single object) and search (data array)
        paper = data if id_type in ["DOI", "CorpusID", "ArXiv"] else data.get("data", [None])[0]
        
        if not paper:
            print(f"No results found for {id_type}: {id_value}")
            return None
        
        return {
            "title": paper.get("title"),
            "authors": [a["name"] for a in paper.get("authors", [])],
            "year": paper.get("year"),
            "abstract": paper.get("abstract"),
            "tldr": paper.get("tldr"),
            "url": paper.get("url"),
        }
    except requests.RequestException as e:
        print(f"API request failed: {e}")
        return None


def fetch_web_content(identifier_info):
    """Fetch web content from Tavily API."""
    query = identifier_info.get("identifier_value")
    
    try:
        r = requests.post(
            TAVILY_URL,
            headers={"Authorization": f"Bearer {os.getenv('TAVILY_API_KEY')}"},
            json={"query": query, "max_results": 1},
            timeout=20,
        )
        r.raise_for_status()
        results = r.json().get("results", [])
        
        if not results:
            print(f"No results found for: {query}")
            return None
        
        res = results[0]
        return {
            "title": res.get("title", "Untitled"),
            "content": res.get("content") or res.get("snippet"),
            "authors": [res.get("author")] if res.get("author") else ["Unknown"],
            "url": res.get("url"),
        }
    except requests.RequestException as e:
        print(f"API request failed: {e}")
        return None


def generate_note(source_type, content):
    """Generate structured notes from content."""
    prompt = get_generate_note_prompt(source_type, content)
    resp = call_llm(prompt, json_format=True)
    return json.loads(resp.choices[0].message.content)


def render_markdown(meta, note, origin):
    authors = "\n".join(f"  - [[{a}]]" for a in meta["authors"])
    topics = "\n".join(f"  - [[{t} (MOC)]]" for t in note["topics"][:3])

    body = "\n\n".join(
        f"## {k}\n\n{v}" for k, v in note["body_sections"].items()
    )

    return f"""---
Authors:
{authors}
Summary: {note['summary']}
Year: {meta.get('year','')}
Topic:
{topics}
Link: {meta.get('url','')}
Title: {meta['title']}
Source-Type: {meta['source_type']}
Origin: {origin}
---

{body}
"""


def export_markdown(md, source_type, title, output_dir=None):
    """Export markdown file to local folder."""
    if not output_dir:
        return
    
    folder = FOLDER_MAP.get(source_type, "Misc")
    safe_title = re.sub(r"[\\/:*?\"<>|]", "", title)
    file_path = os.path.join(output_dir, folder, f"{safe_title}.md")
    
    os.makedirs(os.path.dirname(file_path), exist_ok=True)
    with open(file_path, "w", encoding="utf-8") as f:
        f.write(md)
    
    print(f"Exported: {file_path}")


def get_content(meta, source_type):
    """Combine relevant content for note generation."""
    if source_type == "Research Paper":
        abstract = meta.get("abstract", "")
        tldr = meta.get("tldr", "")
        return f"{tldr}\n\n{abstract}".strip() if tldr and abstract else (tldr or abstract or "")
    return meta.get("content") or meta.get("abstract", "")


def main(input_file_path, origin="", output_dir=None):
    """Process references from a file and export markdown files."""
    try:
        with open(input_file_path, "r", encoding="utf-8") as f:
            input_text = f.read()
    except FileNotFoundError:
        print(f"Error: File not found: {input_file_path}")
        return
    except Exception as e:
        print(f"Error reading file: {e}")
        return
    
    for ref in parse_references(input_text):
        source_type = classify_source(ref)
        if source_type == "Book":
            continue
        
        identifier_info = extract_identifier(source_type, ref)
        print(f"Extracted {identifier_info['identifier_type']}: {identifier_info['identifier_value']}")
        
        # Fetch metadata
        meta = (fetch_research_paper(identifier_info) 
                if source_type == "Research Paper" 
                else fetch_web_content(identifier_info))
        
        if not meta:
            print(f"Could not fetch data for: {ref}")
            continue
        
        meta["source_type"] = source_type
        meta["authors"] = meta["authors"] or ["Unknown"]
        
        # Generate and export
        note = generate_note(source_type, get_content(meta, source_type))
        md = render_markdown(meta, note, origin)
        export_markdown(md, source_type, meta["title"], output_dir)


if __name__ == "__main__":
    # Example usage with a file path
    # Create a sample file for demonstration
    sample_file = "/tmp/sample_references.txt"
    with open(sample_file, "w", encoding="utf-8") as f:
        f.write("""The human brain is a prediction machine: The German physician Hermann von Helmholtz developed the idea of the brain being a “prediction machine."
""")
    
    main(
        input_file_path="/Users/idanariav/Downloads/test_imports.txt",
        origin="[[The art and science of connections (book)]]",
        output_dir="/Users/idanariav/Downloads"
    )

