import scrapy
import re

ID_DETECTION = re.compile(r'\d+')


class BggSpiderSpider(scrapy.Spider):
    name = "bgg_scrapper"
    allowed_domains = ["boardgamegeek.com"]
    start_urls = ["https://boardgamegeek.com/browse/boardgame/page/1"]

    def parse(self, response):
        for row in response.css("tr[id = row_]"):
            rank = row.css('td[class = collection_rank]::text').getall()[1].strip()
            title = row.css("div[id*=results_objectname] a::text").get()
            link = row.css("div[id*=results_objectname] a::attr(href)").get()
            game_id = ID_DETECTION.search(link).group()
            game_page = f"boardgamegeek.com/{link}"

            scrapped_info = {
                "rank": rank,
                "id": game_id,
                "title": title,
                "page": game_page
            }
            yield scrapped_info

        next_page = response.css("a[title='next page'] ::attr(href)").get()
        yield scrapy.Request(url=response.urljoin(next_page),
                             callback=self.parse)

# todo fix yielding