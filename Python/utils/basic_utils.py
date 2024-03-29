import pandas as pd
import numpy as np
import scipy.stats as stats
import logging


class dfCleaner(object):

    @staticmethod
    def column_optimizer(df_to_optimize, categorical_cols=None):
        for column in df_to_optimize.columns:
            if df_to_optimize[column].dtype == "int64":
                df_to_optimize[column] = pd.to_numeric(df_to_optimize[column], downcast='integer')
            elif df_to_optimize[column].dtype == "float64":
                df_to_optimize[column] = pd.to_numeric(df_to_optimize[column], downcast='float')
        if categorical_cols is not None:
            for column in categorical_cols:
                df_to_optimize[column] = pd.Categorical(df_to_optimize[column])

    @staticmethod
    def column_name_cleaner(df_to_clean):
        df_to_clean.columns = [col.lower().replace(" ", "_") for col in df_to_clean.columns]

    @staticmethod
    def outlier_replace_with_nan(df_to_clean, z_score, columns_to_clean):
        for column in columns_to_clean:
            df_to_clean.loc[abs(stats.zscore(df_to_clean[column])) > z_score, column] = np.NaN

    @staticmethod
    def col_index_finder(df, list_of_cols):
        index_list = [df.columns.get_loc(c) for c in list_of_cols if c in df]
        index_list.sort()
        return index_list


class MyLogger(logging.Logger):
    def __new__(cls, name, level=logging.NOTSET):
        return logging.getLogger(name)


def logger_setup():
    # Set up the logger
    logger = MyLogger('basic_logger')
    logger.setLevel(logging.DEBUG)
    logger.propagate = False

    # Create a stream handler and set the level to DEBUG
    stream_handler = logging.StreamHandler()
    stream_handler.setLevel(logging.DEBUG)

    # Create a formatter and set it for the stream handler
    formatter = logging.Formatter('[%(levelname)s] %(asctime)s: %(message)s')
    stream_handler.setFormatter(formatter)

    # Add the stream handler to the logger
    logger.addHandler(stream_handler)
