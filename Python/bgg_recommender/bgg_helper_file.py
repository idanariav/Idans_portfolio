from kaggle.api.kaggle_api_extended import KaggleApi
from Python.utils.basic_utils import dfCleaner, logger
from sklearn.preprocessing import LabelEncoder
import pandas as pd
import os
import numpy as np
import zipfile

# constants
api = KaggleApi()
api.authenticate()
label_encoder = LabelEncoder()
DATASET_ID = 'jvanelteren/boardgamegeek-reviews'
WORKING_FOLDER = "C:\\Users\\idana\\kaggle_projects"
INPUTS_FOLDER = os.path.join(WORKING_FOLDER, "datasets")
OUTPUTS_FOLDER = os.path.join(WORKING_FOLDER, "bgg_recommender")
GAME_INFO_COLS = ['id','primary','yearpublished','minplayers','maxplayers',
                  'playingtime','boardgamecategory','boardgamemechanic',
                  'boardgamefamily','boardgamedesigner','boardgamepublisher',
                  'usersrated','average','Board Game Rank','numcomments',
                  'averageweight', 'thumbnail', 'description']
OUTLIERS_Z_SCORE = 3
df_cleaner = dfCleaner()
RATING_COLS = ['user', 'rating', 'ID']
RATING_COLS_INDEX = [1, 2, 4]
GAMES_INFO_FILENAME = "games_detailed_info"
GAMES_INFO_CSV = f"{GAMES_INFO_FILENAME}.csv"
GAMES_INFO_FEATHER = f"{GAMES_INFO_FILENAME}.feather"
REVIEWS_FILENAME = "bgg-19m-reviews"
REVIEWS_CSV = f"{REVIEWS_FILENAME}.csv"
REVIEWS_FEATHER = f"{REVIEWS_FILENAME}.feather"
TOP_GAMES = 500


def download_and_unzip_file(dataset, file_name):
    logger.info(f"start downloading {file_name}")
    api.dataset_download_file(dataset, file_name,
                              path=INPUTS_FOLDER)
    with zipfile.ZipFile(os.path.join(INPUTS_FOLDER,
                                      f'{file_name}.zip'),
                         'r') as zip_ref:
        zip_ref.extractall(INPUTS_FOLDER)
    logger.info(f"Finished downloading {file_name}")


def check_files_existence(folder, list_of_files):
    files_existence = []
    for file_name in list_of_files:
        files_existence.append(os.path.isfile(os.path.join(folder, file_name)))
    return any(files_existence)


def main():
    # step 0 - importing datasets
    if not check_files_existence(folder=INPUTS_FOLDER, list_of_files=[GAMES_INFO_FEATHER, GAMES_INFO_CSV]):
        download_and_unzip_file(dataset=DATASET_ID, file_name=GAMES_INFO_CSV)

    if not check_files_existence(folder=INPUTS_FOLDER, list_of_files=[REVIEWS_CSV, REVIEWS_FEATHER]):
        download_and_unzip_file(dataset=DATASET_ID, file_name=REVIEWS_CSV)

    # step 1 - cleaning (once per file, not per run)
    if not check_files_existence(folder=INPUTS_FOLDER, list_of_files=[GAMES_INFO_FEATHER]):
        logger.info("cleaning games info")
        games_info = pd.read_csv(os.path.join(INPUTS_FOLDER, GAMES_INFO_CSV),
                                 na_values=['Not Ranked'])
        games_info.reset_index(drop=True).to_feather(os.path.join(INPUTS_FOLDER, GAMES_INFO_FEATHER))

    if not check_files_existence(folder=INPUTS_FOLDER, list_of_files=[REVIEWS_FEATHER]):
        reviews = pd.read_csv(os.path.join(INPUTS_FOLDER, REVIEWS_CSV))
        reviews.to_feather(os.path.join(INPUTS_FOLDER, REVIEWS_FEATHER))

    # step 2 - import and clean games info
    logger.info("processing games info")
    games_info = pd.read_feather(os.path.join(INPUTS_FOLDER, GAMES_INFO_FEATHER))
    games_info = games_info[GAME_INFO_COLS]
    df_cleaner.outlier_replace_with_nan(df_to_clean=games_info, columns_to_clean=['playingtime', 'maxplayers'],
                                        z_score=OUTLIERS_Z_SCORE)
    games_info.loc[games_info['minplayers'] == 0, 'minplayers'] = np.NaN
    games_info['game_length'] = pd.cut(games_info['playingtime'], bins=[0, 60, 120, 180, 900],
                                       labels=['under_an_hour', "one_or_two_hours", "two_to_three_hours",
                                               "over_three_hours"])
    games_info['maxplayers'] = games_info['maxplayers'].astype("Int64")
    games_info['minplayers'] = games_info['minplayers'].astype("Int64")
    df_cleaner.column_optimizer(df_to_optimize=games_info, categorical_cols=['game_length'])

    # step 3 - import and process reviews
    logger.info("importing reviews")
    reviews = pd.read_feather(os.path.join(INPUTS_FOLDER, REVIEWS_FEATHER),
                              columns=RATING_COLS_INDEX)

    logger.info("processing reviews")
    df_cleaner.column_name_cleaner(df_to_clean=reviews)
    reviews['user'] = label_encoder.fit_transform(reviews['user'])
    df_cleaner.column_optimizer(df_to_optimize=reviews)
    reviews = reviews[reviews['id'].isin(games_info['id'])]

    # step 4 - create recommender matrix
    logger.info("creating recommender matrix")
    rated_games = reviews.groupby("id").agg({"rating": "mean", "user": "count"}).reset_index()
    rated_games = rated_games.sort_values(by='user', ascending=False)
    rated_games = rated_games.head(TOP_GAMES)
    sample_reviews = reviews[reviews['id'].isin(rated_games['id'])]
    sample_games_info = games_info[games_info["id"].isin(rated_games['id'])]

    # step 5 - export
    logger.info("exporting files")
    sample_reviews.reset_index(drop=True).to_feather(os.path.join(OUTPUTS_FOLDER, "sample_reviews.feather"))
    sample_games_info.reset_index(drop=True).to_feather(os.path.join(OUTPUTS_FOLDER, "sample_games_info.feather"))


if __name__ == '__main__':
    main()
