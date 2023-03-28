from kaggle.api.kaggle_api_extended import KaggleApi
import pandas as pd
import os
import numpy as np
import logging

api = KaggleApi()
api.authenticate()
DOWNLOAD_FOLDER = "C:\\Users\\idana\\Downloads\\kaggle_test"

# set up
logging.basicConfig(format='%(asctime)s %(message)s')
logger = logging.getLogger()
logger.setLevel(logging.DEBUG)

# download single file
#Signature: dataset_download_file(dataset, file_name, path=None, force=False, quiet=True)


def main():
    logger.info("start downloading file")
    api.dataset_download_file('jvanelteren/boardgamegeek-reviews', 'games_detailed_info.csv',
                              path=DOWNLOAD_FOLDER)
    logger.info("finished downloading file")
    # todo - unzip the file
    test = pd.read_csv(os.path.join(DOWNLOAD_FOLDER, 'games_detailed_info.csv',"games_detailed_info.csv"))
    logger.info("stop here")
    logger.info("stop here")


if __name__ == '__main__':
    main()
