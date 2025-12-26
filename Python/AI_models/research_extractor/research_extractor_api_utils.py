import os
import re
import json
import trafilatura
import requests
from bs4 import BeautifulSoup
from dotenv import load_dotenv
from urllib.parse import urlparse, parse_qs
from research_extractor_constants import (
    TAVILY_API_URL,
    NYT_API_URL,
    NYT_DOMAINS,
    API_REQUEST_TIMEOUT,
    NYT_API_TIMEOUT,
)

load_dotenv()

def fetch_web_content(identifier_info):
    """Fetch article or lecture content via metadata-first strategy."""
    url = identifier_info.get("url")
    query = identifier_info.get("identifier_value")

    # 1. Metadata-first extraction
    if url:
        meta = extract_article_metadata(url)
        if meta and (meta.get("title") or meta.get("description")):
            authors = (
                [a.strip() for a in re.split(r",| and ", meta["author"])]
                if meta.get("author")
                else ["Unknown"]
            )
            return {
                "title": meta.get("title") or "Untitled",
                "content": meta.get("description") or "",
                "authors": authors,
                "url": url,
            }

        # 2. Readability fallback
        downloaded = trafilatura.fetch_url(url)
        if downloaded:
            content = trafilatura.extract(downloaded)
            metadata = trafilatura.extract_metadata(downloaded)
            if content:
                authors = (
                    re.split(r",| and ", metadata.author)
                    if metadata and metadata.author
                    else ["Unknown"]
                )
                return {
                    "title": metadata.title if metadata and metadata.title else "Untitled",
                    "content": content,
                    "authors": authors,
                    "url": url,
                }

    # 3. Tavily fallback
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
            "title": res.get("title", "Untitled"),
            "content": res.get("content") or res.get("snippet"),
            "authors": [res.get("author")] if res.get("author") else ["Unknown"],
            "url": res.get("url"),
        }
    except Exception:
        return None
    
def extract_article_metadata(url, nyt_api_key=os.getenv('NYT_API_KEY')):
    """
    Extract article metadata using NYT API (if available) or HTML scraping as fallback.
    
    Args:
        url: Article URL to extract metadata from
        nyt_api_key: Optional NYT API key. If provided, will try API first for NYT articles.
    
    Returns:
        dict with keys: title, description, author, or None on failure
    """
    
    # Check if this is a NYT URL and we have an API key
    if nyt_api_key and is_nyt_url(url):
        print(f"Detected NYT URL, attempting API extraction...")
        api_result = extract_from_nyt_api(url, nyt_api_key)
        if api_result:
            print(f"Successfully extracted metadata via NYT API")
            return api_result
        print(f"API extraction failed, falling back to HTML scraping...")
    
    # Fallback to HTML scraping
    return extract_from_html(url)


def is_nyt_url(url):
    """Check if URL is from New York Times."""
    parsed = urlparse(url)
    return any(domain in parsed.netloc for domain in NYT_DOMAINS)


def extract_from_nyt_api(url, api_key):
    """
    Extract metadata using NYT Article Search API.
    
    The API can search by:
    1. Direct web_url match (best for modern URLs)
    2. Legacy article ID (for query.nytimes.com URLs)
    3. Keywords from the URL
    """
    try:
        # Strategy 1: Search by exact URL match
        result = search_nyt_by_url(url, api_key)
        if result:
            return result
        
        # Strategy 2: For legacy URLs, extract and search by article ID
        if 'query.nytimes.com' in url:
            article_id = extract_legacy_article_id(url)
            if article_id:
                result = search_nyt_by_id(article_id, api_key)
                if result:
                    return result
        
        # Strategy 3: Extract keywords from URL and search
        keywords = extract_keywords_from_url(url)
        if keywords:
            result = search_nyt_by_keywords(keywords, api_key)
            if result:
                return result
        
        return None
        
    except Exception as e:
        print(f"NYT API error: {e}")
        return None


def search_nyt_by_url(url, api_key):
    """Search NYT API by exact URL match."""
    
    
    params = {
        "fq": f'web_url:("{url}")',
        "api-key": api_key
    }
    
    try:
        response = requests.get(NYT_API_URL, params=params, timeout=NYT_API_TIMEOUT)
        response.raise_for_status()
        data = response.json()
        
        if data.get("response", {}).get("docs"):
            return format_nyt_api_response(data["response"]["docs"][0])
        
    except Exception as e:
        print(f"URL search failed: {e}")
    
    return None


def search_nyt_by_id(article_id, api_key):
    """Search NYT API by legacy article ID."""
    
    # Try searching for the article ID in the URL field
    params = {
        "fq": f'web_url:(*{article_id}*)',
        "api-key": api_key
    }
    
    try:
        response = requests.get(NYT_API_URL, params=params, timeout=NYT_API_TIMEOUT)
        response.raise_for_status()
        data = response.json()
        
        if data.get("response", {}).get("docs"):
            return format_nyt_api_response(data["response"]["docs"][0])
        
    except Exception as e:
        print(f"ID search failed: {e}")
    
    return None


def search_nyt_by_keywords(keywords, api_key):
    """Search NYT API by keywords extracted from URL."""
    
    params = {
        "q": keywords,
        "api-key": api_key,
        "sort": "relevance"
    }
    
    try:
        response = requests.get(NYT_API_URL, params=params, timeout=NYT_API_TIMEOUT)
        response.raise_for_status()
        data = response.json()
        
        if data.get("response", {}).get("docs"):
            # Return the most relevant result
            return format_nyt_api_response(data["response"]["docs"][0])
        
    except Exception as e:
        print(f"Keyword search failed: {e}")
    
    return None


def extract_legacy_article_id(url):
    """Extract article ID from legacy NYT URL."""
    # Legacy URLs have format: ?res=ARTICLEID&...
    parsed = urlparse(url)
    params = parse_qs(parsed.query)
    return params.get('res', [None])[0]


def extract_keywords_from_url(url):
    """Extract search keywords from URL parameters."""
    parsed = urlparse(url)
    params = parse_qs(parsed.query)
    
    # Get 'sq' parameter which contains search query
    sq = params.get('sq', [None])[0]
    if sq:
        # Clean up the keywords
        keywords = sq.replace('"', '').replace('%20', ' ')
        return keywords
    
    return None


def format_nyt_api_response(article):
    """Format NYT API response into standard metadata structure."""
    # Extract title
    title = None
    if article.get("headline"):
        title = article["headline"].get("main")
    
    # Extract description
    description = article.get("abstract") or article.get("snippet") or article.get("lead_paragraph")
    
    # Extract author
    author = None
    if article.get("byline"):
        if isinstance(article["byline"], dict):
            author = article["byline"].get("original")
        elif isinstance(article["byline"], str):
            author = article["byline"]
    
    return {
        "title": title,
        "description": description,
        "author": author
    }


def extract_from_html(url):
    """Extract article metadata via HTML scraping (fallback method)."""
    try:
        headers = {
            "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
            "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
            "Accept-Language": "en-US,en;q=0.5",
            "Accept-Encoding": "gzip, deflate",
            "Connection": "keep-alive",
            "Upgrade-Insecure-Requests": "1"
        }
        r = requests.get(url, headers=headers, timeout=20, allow_redirects=True)
        r.raise_for_status()
        soup = BeautifulSoup(r.text, "html.parser")
        
        meta = {
            "title": None,
            "description": None,
            "author": None,
        }
        
        # Open Graph + standard meta
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
        
        # JSON-LD
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
        
        # FALLBACK for legacy pages
        if not meta["title"]:
            # Try <title> tag
            title_tag = soup.find("title")
            if title_tag:
                meta["title"] = title_tag.get_text().strip()
                # Remove " - New York Times" suffix if present
                meta["title"] = meta["title"].replace(" - The New York Times", "").replace(" - New York Times", "").strip()
        
        if not meta["author"]:
            # Try byline class or common patterns
            byline = soup.find(class_="byline")
            if byline:
                meta["author"] = byline.get_text().strip()
            else:
                # Look for "By AUTHOR NAME" pattern in text
                for tag in soup.find_all(['p', 'div']):
                    text = tag.get_text().strip()
                    if text.startswith("By "):
                        meta["author"] = text.split('\n')[0].replace("By ", "").strip()
                        break
        
        if not meta["description"]:
            # Try to get first paragraph as description
            # Look for article body paragraphs
            article_body = soup.find(class_=["story", "articleBody", "article-body"])
            if article_body:
                first_p = article_body.find("p")
                if first_p:
                    meta["description"] = first_p.get_text().strip()[:300]  # Limit to 300 chars
        
        return meta
    except Exception as e:
        print(f"HTML extraction error: {e}")
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
    import time
    
    for attempt in range(max_retries):
        try:
            if attempt == 0 and base_delay > 0:
                time.sleep(base_delay)
            
            r = requests.get(url, params=params, headers=headers, timeout=timeout)
            
            # Handle rate limit
            if r.status_code == 429:
                if attempt < max_retries - 1:
                    time.sleep(base_delay * (2 ** attempt))
                    continue
                return None
            
            r.raise_for_status()
            return r.json()
        except Exception:
            if attempt == max_retries - 1:
                return None
    return None


def normalize_paper_metadata(data, source, id_type=None):
    """Normalize paper metadata from different API sources to common format.
    
    Args:
        data: Raw API response
        source: Source name ('semantic_scholar' or 'openalex')
        id_type: Identifier type for Semantic Scholar (DOI, CorpusID, ArXiv, Title)
    
    Returns:
        Normalized metadata dict or None
    """
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
            work = data["results"][0] if "results" in data else data
            if "results" in data and not data["results"]:
                return None
            
            authors = [a["author"]["display_name"] for a in work.get("authorships", [])]
            
            # Reconstruct abstract from inverted index
            abstract = None
            inv_index = work.get("abstract_inverted_index")
            if inv_index:
                try:
                    max_pos = max([max(positions) for positions in inv_index.values()])
                    words = [""] * (max_pos + 1)
                    for word, positions in inv_index.items():
                        for pos in positions:
                            words[pos] = word
                    abstract = " ".join(words)
                except Exception:
                    abstract = None
            
            return {
                "title": work.get("title"),
                "authors": authors,
                "year": work.get("publication_year"),
                "abstract": abstract,
                "tldr": None,
                "url": work.get("doi") or work.get("id"),
                "source": "OpenAlex"
            }
    except Exception:
        return None


def fetch_semantic_scholar_metadata(id_type, id_value):
    """Fetch paper metadata from Semantic Scholar API."""
    from research_extractor_constants import (
        SEMANTIC_SCHOLAR_API_URL, SEMANTIC_SCHOLAR_FIELDS,
        SEMANTIC_SCHOLAR_RATE_LIMIT_DELAY, SEMANTIC_SCHOLAR_MAX_RETRIES, FETCH_TIMEOUT
    )
    
    # Build URL and params
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
    
    # Normalize response
    return normalize_paper_metadata(data, "semantic_scholar", id_type)


def fetch_openalex_metadata(id_type, id_value):
    """Fetch paper metadata from OpenAlex API."""
    from research_extractor_constants import OPENALEX_API_URL, FETCH_TIMEOUT
    
    # Build URL
    if id_type == "DOI":
        url = f"{OPENALEX_API_URL}/doi:{id_value}"
    else:
        url = f"{OPENALEX_API_URL}?filter=title.search:{id_value}"
    
    # Add polite pool email
    params = {"mailto": os.getenv("USER_EMAIL", "user@example.com")}
    
    # Make request (OpenAlex is more reliable, fewer retries needed)
    data = make_api_request_with_retry(url, params, timeout=FETCH_TIMEOUT, max_retries=2, base_delay=0.5)
    
    if not data:
        return {"error": "OpenAlex request failed"}
    
    # Normalize response
    result = normalize_paper_metadata(data, "openalex")
    return result if result else {"error": "No results from OpenAlex"}


def fetch_google_books_metadata(book_title):
    """Fetch book metadata from Google Books API."""
    from research_extractor_constants import FETCH_TIMEOUT
    
    url = "https://www.googleapis.com/books/v1/volumes"
    params = {"q": book_title, "maxResults": 1}
    
    # Add API key if available
    api_key = os.getenv("GOOGLE_BOOKS_API_KEY")
    if api_key:
        params["key"] = api_key.strip()
    
    # Make request (no retry needed for books)
    data = make_api_request_with_retry(url, params, timeout=FETCH_TIMEOUT, max_retries=1, base_delay=0)
    
    if not data or "items" not in data or not data["items"]:
        return {"error": "No books found"}
    
    book = data["items"][0]["volumeInfo"]
    
    # Extract ISBN (DRY: single loop for identifiers)
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
