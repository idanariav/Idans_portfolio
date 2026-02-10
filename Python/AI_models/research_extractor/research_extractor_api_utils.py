"""
API utilities for research extractor.

Handles fetching metadata from various sources:
- Web content (articles, lectures, posts) with NYT API support
- Academic papers (Semantic Scholar, OpenAlex)
- Books (Google Books)
"""

import os
import json
import time
import logging
import trafilatura
import requests
from concurrent.futures import ThreadPoolExecutor, as_completed

logger = logging.getLogger(__name__)
from urllib.parse import urlparse, parse_qs

from research_extractor_constants import (
    TAVILY_API_URL,
    NYT_API_URL,
    NYT_DOMAINS,
    NYT_API_TIMEOUT,
    SEMANTIC_SCHOLAR_API_URL,
    SEMANTIC_SCHOLAR_FIELDS,
    SEMANTIC_SCHOLAR_RATE_LIMIT_DELAY,
    SEMANTIC_SCHOLAR_MAX_RETRIES,
    FETCH_TIMEOUT,
    OPENALEX_API_URL,
    API_REQUEST_TIMEOUT,
    DEFAULT_TITLE,
    DEFAULT_AUTHOR,
)


def fetch_web_api(identifier_info):
    """Fetch article/lecture content via parallel strategy.
    
    Tries multiple methods concurrently and returns first successful result.
    """
    url = identifier_info.get("url")
    query = identifier_info.get("identifier_value")
    
    def _try_metadata_extraction():
        """Method 1: Direct metadata extraction from URL."""
        if not url:
            return None
        meta = extract_article_metadata(url)
        if meta and (meta.get("title") or meta.get("description")):
            authors = (
                [a.strip() for a in meta["author"].split(",") if a.strip()]
                if meta.get("author")
                else [DEFAULT_AUTHOR]
            )
            return {
                "title": meta.get("title") or DEFAULT_TITLE,
                "content": meta.get("description") or "",
                "authors": authors,
                "url": url,
                "_method": "metadata"
            }
        return None
    
    def _try_trafilatura():
        """Method 2: Full content extraction with trafilatura."""
        if not url:
            return None
        try:
            downloaded = trafilatura.fetch_url(url)
            if downloaded:
                content = trafilatura.extract(downloaded)
                metadata = trafilatura.extract_metadata(downloaded)
                if content:
                    authors = (
                        [metadata.author] if metadata and metadata.author else [DEFAULT_AUTHOR]
                    )
                    return {
                        "title": metadata.title if metadata and metadata.title else DEFAULT_TITLE,
                        "content": content,
                        "authors": authors,
                        "url": url,
                        "_method": "trafilatura"
                    }
        except Exception as e:
            logger.warning("Trafilatura extraction failed for %s: %s", url, e)
        return None

    def _try_tavily():
        """Method 3: Tavily search fallback."""
        if not query:
            return None
        try:
            r = requests.post(
                TAVILY_API_URL,
                headers={"Authorization": f"Bearer {os.getenv('TAVILY_API_KEY')}"},
                json={"query": query, "max_results": 1},
                timeout=API_REQUEST_TIMEOUT,
            )
            r.raise_for_status()
            res = r.json()["results"][0]
            return {
                "title": res.get("title", DEFAULT_TITLE),
                "content": res.get("content") or res.get("snippet"),
                "authors": [res.get("author")] if res.get("author") else [DEFAULT_AUTHOR],
                "url": res.get("url"),
                "_method": "tavily"
            }
        except Exception as e:
            logger.warning("Tavily search failed for '%s': %s", query, e)
        return None

    # Run methods in parallel, return first success
    methods = []
    if url:
        methods.extend([_try_metadata_extraction, _try_trafilatura])
    if query:
        methods.append(_try_tavily)
    
    if not methods:
        return None
    
    # Execute with parallel threads
    with ThreadPoolExecutor(max_workers=3) as executor:
        futures = {executor.submit(m): m.__name__ for m in methods}
        
        for future in as_completed(futures, timeout=API_REQUEST_TIMEOUT + 5):
            try:
                result = future.result()
                if result:
                    # Remove internal method marker before returning
                    result.pop("_method", None)
                    return result
            except Exception as e:
                logger.warning("Web fetch method %s failed: %s", futures[future], e)
                continue

    return None


def extract_article_metadata(url):
    """Extract article metadata using NYT API or HTML scraping."""
    nyt_api_key = os.getenv('NYT_API_KEY')
    if nyt_api_key and is_nyt_url(url):
        api_result = extract_from_nyt_api(url, nyt_api_key)
        if api_result:
            return api_result
    
    # Single fetch, reuse HTML for both trafilatura and manual parsing
    try:
        downloaded = trafilatura.fetch_url(url)
        if downloaded:
            metadata = trafilatura.extract_metadata(downloaded)
            if metadata and (metadata.title or metadata.description):
                return {
                    "title": metadata.title,
                    "description": metadata.description,
                    "author": metadata.author,
                }
            # Fallback to manual HTML parsing using same downloaded content
            return _extract_html_metadata(downloaded)

        return None

    except Exception as e:
        logger.warning("Article metadata extraction failed for %s: %s", url, e)
        return None


def is_nyt_url(url):
    """Check if URL is from NYT."""
    return any(domain in urlparse(url).netloc for domain in NYT_DOMAINS)


def extract_from_nyt_api(url, api_key):
    """Extract metadata using NYT Article Search API."""
    try:
        # Try exact URL match
        params = {"fq": f'web_url:("{url}")', "api-key": api_key}
        result = _nyt_api_request(params)
        if result:
            return result
        
        # Try legacy article ID for old URLs
        if 'query.nytimes.com' in url:
            article_id = extract_legacy_article_id(url)
            if article_id:
                params = {"fq": f'web_url:(*{article_id}*)', "api-key": api_key}
                result = _nyt_api_request(params)
                if result:
                    return result
        
        # Try keyword search from URL
        keywords = extract_keywords_from_url(url)
        if keywords:
            params = {"q": keywords, "api-key": api_key, "sort": "relevance"}
            return _nyt_api_request(params)
        
        return None
    except Exception as e:
        logger.warning("NYT API extraction failed for %s: %s", url, e)
        return None


def _nyt_api_request(params):
    """Make NYT API request and format response."""
    try:
        response = requests.get(NYT_API_URL, params=params, timeout=NYT_API_TIMEOUT)
        response.raise_for_status()
        data = response.json()
        
        if data.get("response", {}).get("docs"):
            return format_nyt_api_response(data["response"]["docs"][0])
    except Exception as e:
        logger.warning("NYT API request failed: %s", e)
    return None


def extract_legacy_article_id(url):
    """Extract article ID from legacy NYT URL."""
    parsed = urlparse(url)
    params = parse_qs(parsed.query)
    return params.get('res', [None])[0]


def extract_keywords_from_url(url):
    """Extract search keywords from URL."""
    parsed = urlparse(url)
    params = parse_qs(parsed.query)
    sq = params.get('sq', [None])[0]
    return sq.replace('"', '').replace('%20', ' ') if sq else None


def format_nyt_api_response(article):
    """Format NYT API response."""
    title = article.get("headline", {}).get("main") if article.get("headline") else None
    description = article.get("abstract") or article.get("snippet") or article.get("lead_paragraph")
    
    author = None
    byline = article.get("byline")
    if byline:
        author = byline.get("original") if isinstance(byline, dict) else byline
    
    return {"title": title, "description": description, "author": author}


def _extract_html_metadata(html_content):
    """Extract metadata from HTML content using BeautifulSoup.
    
    Args:
        html_content: HTML page content as string
    
    Returns:
        Dict with title, description, author or None
    """
    try:
        from bs4 import BeautifulSoup
        
        soup = BeautifulSoup(html_content, "html.parser")
        
        meta = {
            "title": None,
            "description": None,
            "author": None,
        }
        
        # Open Graph + standard meta tags
        for tag in soup.find_all("meta"):
            prop = tag.get("property") or tag.get("name")
            content = tag.get("content")
            if not prop or not content:
                continue
            if prop == "og:title":
                meta["title"] = content
            elif prop in ("og:description", "description"):
                meta["description"] = content
            elif prop in ("article:author", "author"):
                meta["author"] = content
        
        # JSON-LD structured data
        for script in soup.find_all("script", type="application/ld+json"):
            try:
                data = json.loads(script.string)
                if isinstance(data, dict) and data.get("@type") in ("Article", "NewsArticle"):
                    meta["title"] = meta["title"] or data.get("headline")
                    author = data.get("author")
                    if isinstance(author, dict):
                        meta["author"] = meta["author"] or author.get("name")
            except Exception:
                continue
        
        # Fallback: title tag
        if not meta["title"]:
            title_tag = soup.find("title")
            if title_tag:
                meta["title"] = title_tag.get_text().strip()
        
        return meta
    
    except Exception as e:
        logger.warning("HTML metadata extraction failed: %s", e)
        return None


def make_api_request_with_retry(url, params=None, headers=None, timeout=30, max_retries=3, base_delay=1.5):
    """Generic API request with exponential backoff retry logic.
    
    Args:
        url: API endpoint URL
        params: Query parameters dict
        headers: HTTP headers dict
        timeout: Request timeout in seconds
        max_retries: Maximum retry attempts
        base_delay: Base delay for exponential backoff
    
    Returns:
        Response JSON dict or None on failure
    """
    for attempt in range(max_retries):
        try:
            if attempt > 0 and base_delay > 0:
                wait_time = base_delay * (2 ** (attempt - 1))
                time.sleep(wait_time)

            r = requests.get(url, params=params, headers=headers, timeout=timeout)

            # Handle rate limit
            if r.status_code == 429:
                if attempt < max_retries - 1:
                    continue
                return None

            r.raise_for_status()
            return r.json()
        except Exception as e:
            logger.warning("API request to %s failed (attempt %d/%d): %s", url, attempt + 1, max_retries, e)
            if attempt == max_retries - 1:
                return None

    return None


def normalize_paper_metadata(data, source, id_type=None):
    """Normalize paper metadata from Semantic Scholar or OpenAlex."""
    try:
        if source == "semantic_scholar":
            if data.get("error"):
                return None
            
            paper = data if id_type in ["DOI", "CorpusID", "ArXiv"] else data.get("data", [None])[0]
            if not paper:
                return None
            
            return {
                "title": paper.get("title"),
                "authors": [a["name"] for a in paper.get("authors", [])],
                "year": paper.get("year"),
                "abstract": paper.get("abstract"),
                "tldr": paper.get("tldr", {}).get("text") if paper.get("tldr") else None,
                "url": paper.get("url"),
                "source": "Semantic Scholar"
            }
        
        elif source == "openalex":
            work = data.get("results", [data])[0] if "results" in data else data
            if "results" in data and not data["results"]:
                return None
            
            authors = [a["author"]["display_name"] for a in work.get("authorships", [])]
            
            # Reconstruct abstract from inverted index
            abstract = None
            inv_index = work.get("abstract_inverted_index")
            if inv_index:
                try:
                    max_pos = max(max(positions) for positions in inv_index.values())
                    words = [""] * (max_pos + 1)
                    for word, positions in inv_index.items():
                        for pos in positions:
                            words[pos] = word
                    abstract = " ".join(words)
                except Exception as e:
                    logger.debug("Failed to reconstruct OpenAlex abstract: %s", e)
            
            return {
                "title": work.get("title"),
                "authors": authors,
                "year": work.get("publication_year"),
                "abstract": abstract,
                "tldr": None,
                "url": work.get("doi") or work.get("id"),
                "source": "OpenAlex"
            }
    except Exception as e:
        logger.warning("Paper metadata normalization failed (%s): %s", source, e)
        return None


def fetch_semantic_scholar_metadata(id_type, id_value):
    """Fetch paper metadata from Semantic Scholar."""
    if id_type in ["DOI", "CorpusID", "ArXiv"]:
        paper_id = f"{id_type if id_type != 'ArXiv' else 'ARXIV'}:{id_value}"
        url = f"{SEMANTIC_SCHOLAR_API_URL.replace('/search', '')}/{paper_id}"
        params = {"fields": SEMANTIC_SCHOLAR_FIELDS}
    else:
        url = SEMANTIC_SCHOLAR_API_URL
        params = {"query": id_value, "limit": 1, "fields": SEMANTIC_SCHOLAR_FIELDS}
    
    # Add API key if available
    api_key = os.getenv("SEMANTIC_SCHOLAR_API_KEY")
    headers = {"x-api-key": api_key} if api_key else {}
    
    # Make request with retry
    data = make_api_request_with_retry(
        url, params, headers,
        timeout=FETCH_TIMEOUT,
        max_retries=SEMANTIC_SCHOLAR_MAX_RETRIES,
        base_delay=SEMANTIC_SCHOLAR_RATE_LIMIT_DELAY
    )
    
    if not data:
        return None
    
    return normalize_paper_metadata(data, "semantic_scholar", id_type)


def fetch_openalex_metadata(id_type, id_value):
    """Fetch paper metadata from OpenAlex."""
    if id_type == "DOI":
        url = f"{OPENALEX_API_URL}/doi:{id_value}"
    else:
        url = f"{OPENALEX_API_URL}?filter=title.search:{id_value}"
    
    params = {"mailto": os.getenv("USER_EMAIL", "user@example.com")}
    data = make_api_request_with_retry(url, params, timeout=FETCH_TIMEOUT, max_retries=2, base_delay=0.5)
    
    if not data:
        return {"error": "OpenAlex request failed"}
    
    result = normalize_paper_metadata(data, "openalex")
    return result if result else {"error": "No results from OpenAlex"}


def fetch_google_books_metadata(book_title):
    """Fetch book metadata from Google Books."""
    url = "https://www.googleapis.com/books/v1/volumes"
    params = {"q": book_title, "maxResults": 1}
    
    # Add API key if available
    api_key = os.getenv("GOOGLE_BOOKS_API_KEY")
    if api_key:
        params["key"] = api_key.strip()
    
    # Make request
    data = make_api_request_with_retry(
        url, params, 
        timeout=FETCH_TIMEOUT, 
        max_retries=1, 
        base_delay=0
    )
    
    if not data or "items" not in data or not data["items"]:
        return {"error": "No books found"}
    
    book = data["items"][0]["volumeInfo"]
    
    # Extract ISBN
    isbn = None
    for identifier in book.get("industryIdentifiers", []):
        if identifier["type"] in ["ISBN_13", "ISBN_10"]:
            isbn = identifier["identifier"]
            break
    
    return {
        "title": book.get("title", ""),
        "authors": book.get("authors", []),
        "published_date": book.get("publishedDate", ""),
        "description": book.get("description", ""),
        "isbn": isbn,
        "page_count": book.get("pageCount"),
        "categories": book.get("categories", []),
        "link": book.get("infoLink", ""),
    }

