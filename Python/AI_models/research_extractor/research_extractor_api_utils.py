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
    


