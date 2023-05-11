import logging
import math
import os.path
from time import sleep

import numpy as np
import pandas as pd
import requests
from bs4 import BeautifulSoup
from scrapy.crawler import CrawlerProcess
from scrapy.utils.project import get_project_settings


from bgg_scrapper.settings import FEED_URI
from bgg_scrapper.spiders.bgg_spider import BggSpiderSpider
from utils.basic_utils import logger_setup, dfCleaner

SLEEP_DURATION = 1.5
SLEEP_FAILED = 3
API_URL = "https://www.boardgamegeek.com/xmlapi2/thing"
TOP_N_GAMES = 10
MAX_PAGE_SIZE = 100
EXPORT_FOLDER = os.path.join("C:\\", "Users", "idana", "kaggle_projects", "bgg_scrapper")
df_cleaner = dfCleaner()
logger_setup()
logger = logging.getLogger("basic_logger")


def make_api_call(params: dict) -> BeautifulSoup:
    response = requests.get(API_URL, params=params)
    while response.status_code != 200:
        logger.debug(f"failed request for {params.get('id')}")
        sleep(SLEEP_FAILED)
        response = requests.get(API_URL, params=params)
    soup = BeautifulSoup(response.text, "xml")
    return soup


def extract_grouping_attribute(game_soup: BeautifulSoup, group_type: str) -> list:
    group_list = game_soup.find_all("link", {"type": group_type})
    for index, attribute in enumerate(group_list):
        group_list[index] = attribute.get("value")
    return group_list


def extract_metadata_from_response(game_soup: BeautifulSoup, game_id: str) -> dict:
    game_info_dict = {}
    game_info_dict["id"] = int(game_id)
    game_info_dict['thumbnail'] = game_soup.find("thumbnail").get_text()
    game_info_dict['image'] = game_soup.find("image").get_text()
    game_info_dict['year_published'] = int(game_soup.find("yearpublished").get("value"))
    game_info_dict['min_players'] = int(game_soup.find("minplayers").get("value"))
    game_info_dict['max_players'] = int(game_soup.find("maxplayers").get("value"))
    game_info_dict['categories'] = extract_grouping_attribute(game_soup=game_soup, group_type="boardgamecategory")
    game_info_dict['mechanics'] = extract_grouping_attribute(game_soup=game_soup, group_type="boardgamemechanic")
    game_info_dict['artists'] = extract_grouping_attribute(game_soup=game_soup, group_type="boardgameartist")
    game_info_dict['publishers'] = extract_grouping_attribute(game_soup=game_soup, group_type="boardgamepublisher")
    ratings_section = game_soup.find("ratings")
    game_info_dict['n_users'] = int(ratings_section.find("usersrated").get("value"))
    game_info_dict['score'] = float(ratings_section.find("rank", {"id": "1"}).get("bayesaverage"))
    game_info_dict['n_comments'] = int(ratings_section.find("numcomments").get("value"))
    game_info_dict['weight'] = float(ratings_section.find("averageweight").get("value"))
    return game_info_dict


def extract_comments_from_soup(game_soup: BeautifulSoup, comments_list: list,
                               game_id: str) -> None:
    comments_section = game_soup.find_all("comment")
    for comment in comments_section:
        comment_dict = {
            "username": comment.get("username"),
            "rating": float(comment.get("rating")),
            "text": comment.get("value"),
            "id": int(game_id)
        }
        comments_list.append(comment_dict)


def extract_games_ids(games_metadata: pd.DataFrame) -> tuple[str, list]:
    top_n_games_ids = games_metadata["id"].astype("str").tolist()[:TOP_N_GAMES]
    ids_str = ','.join(top_n_games_ids)
    return ids_str, top_n_games_ids


def merge_tables(games_metadata: pd.DataFrame, games_stats: pd.DataFrame) -> pd.DataFrame:
    games_all_metadata = pd.merge(games_stats, games_metadata, on='id')
    games_all_metadata['n_pages'] = games_all_metadata['n_users'].apply(lambda x: math.ceil(x / MAX_PAGE_SIZE))
    return games_all_metadata


def get_games_comments(games_all_metadata: pd.DataFrame) -> pd.DataFrame:
    logger.info("Start: get games comments and ratings")
    comments_list = []
    max_pages = np.max(games_all_metadata['n_pages'])
    for page_num in np.arange(start=1, stop=max_pages, dtype=int):
        relevant_games = games_all_metadata[games_all_metadata['n_pages'] >= page_num]
        relevant_ids = relevant_games['id'].astype("str").tolist()
        id_list = ','.join(relevant_ids)
        params = {"id": id_list, "ratingcomments": 1, "pagesize": MAX_PAGE_SIZE, "page": page_num}
        sleep(SLEEP_DURATION)
        soup = make_api_call(params=params)
        for game_id in relevant_ids:
            game_soup = soup.find("item", {"id": game_id})
            extract_comments_from_soup(game_soup=game_soup, comments_list=comments_list,
                                       game_id=game_id)
        logger.debug(f"Finished getting comments for page {page_num} out of {max_pages}")
    logger.info("Finish: get games comments and ratings")
    game_comments = pd.DataFrame(comments_list)
    return game_comments


def get_games_stats(ids_str: str, top_n_games_ids: list) -> pd.DataFrame:
    logger.info("Start: Metadata API calls")
    params = {"id": ids_str, "stats": 1}
    soup = make_api_call(params=params)
    games_tables = []
    for game_id in top_n_games_ids:
        game_soup = soup.find("item", {"id": game_id})
        game_info_dict = extract_metadata_from_response(game_soup=game_soup, game_id=game_id)
        games_tables.append(game_info_dict)
    logger.info("Finish: Metadata API calls")
    games_stats = pd.DataFrame(games_tables)
    return games_stats


def get_games_metadata():
    # check if spider output is found, if so: delete it
    if os.path.isfile(FEED_URI):
        os.remove(FEED_URI)
    # getting game ids
    logger.info("Start: initiate spider for games metadata")
    process = CrawlerProcess(settings=get_project_settings())
    process.crawl(BggSpiderSpider)
    process.start()
    games_metadata = pd.read_csv(FEED_URI)
    logger.info("Finish: collect games metadata")
    return games_metadata


def main():

    logger.info("Script: start")
    games_metadata = get_games_metadata()
    ids_str, top_n_games_ids = extract_games_ids(games_metadata=games_metadata)
    logger.info("Params set")

    # getting game stats
    games_stats = get_games_stats(ids_str=ids_str, top_n_games_ids=top_n_games_ids)
    # merging stats and metadata
    games_all_metadata = merge_tables(games_metadata=games_metadata, 
                                      games_stats=games_stats)
    # get game comments
    game_comments = get_games_comments(games_all_metadata=games_all_metadata)
    df_cleaner.column_optimizer(df_to_optimize=game_comments)
    logger.info("Finished: getting all necessary game data")
    logger.info("Exporting tables")
    outputs = {
        "board_game_comments": game_comments,
        "board_games_metadata": games_all_metadata
    }
    for file_name, df_table in outputs.items():
        df_table.to_feather(os.path.join(EXPORT_FOLDER, f"{file_name}.feather"))


if __name__ == '__main__':
    main()
