import os
import pandas as pd
import streamlit as st
import ast
import re
from streamlit_utils import config_page, create_about_me_section, SessionNavigation, \
    prepare_navigation_section, create_contact_form

# constants

INPUTS_FOLDER = os.path.join("streamlit_recommender")
GAMES_INFO_FILENAME = "sample_games_info.feather"
REVIEWS_FILENAME = "sample_reviews.feather"
LIST_LOCATOR = re.compile(r"\[(.*)\]")
MIN_CORRELATION = 0


def extract_str_to_unique_list(df, column):
    column_str_values = df[~df[column].isna()][column].tolist()
    column_list_values = [ast.literal_eval(cell_list) for cell_list in column_str_values]
    entities_list = [single_entity for entity_list in column_list_values for single_entity in entity_list]
    entities_list = list(set(entities_list))
    return entities_list



@st.cache_data
def load_data():
    games_info = pd.read_feather(os.path.join(INPUTS_FOLDER, GAMES_INFO_FILENAME))
    sample_reviews = pd.read_feather(os.path.join(INPUTS_FOLDER, REVIEWS_FILENAME))
    games_matrix = sample_reviews.pivot_table(index='user', columns='id', values='rating')
    return games_info, games_matrix

@st.cache_data
def load_game_attributes(games_info):
    attributes_dict = {}
    attributes_dict["designer"] = extract_str_to_unique_list(df=games_info, column="boardgamedesigner")
    attributes_dict["publisher"] = extract_str_to_unique_list(df=games_info, column="boardgamepublisher")
    attributes_dict["max_players"] = int(games_info['maxplayers'].max())
    return attributes_dict


def format_list_output(option):
    new_option = option.replace("_"," ").capitalize()
    return new_option


def extract_single_entity_from_list(df_col, single_game_attributes):
    single_entity = LIST_LOCATOR.match(single_game_attributes[df_col]).group(1).replace("'","")
    single_entity = single_entity.split(",")[0]
    return single_entity


def get_games_names(games_info):
    game_names_dict = dict(zip(games_info['primary'], games_info['id']))
    game_names = list(game_names_dict.keys())
    game_names.sort()
    game_names.insert(0, "")
    return game_names, game_names_dict


def create_recommendation_section(best_recommendation, game_name_adjusted, game_name_search):
    st.subheader(best_recommendation.primary)
    image_col, text_col = st.columns((1, 2))
    with image_col:
        st.image(best_recommendation.thumbnail)
    with text_col:
        st.markdown(f"""
                        :globe_with_meridians:[Game Page](https://boardgamegeek.com/boardgame/{best_recommendation.id}/{game_name_adjusted})\n
                        :package:[Amazon](https://www.amazon.com/s?k={game_name_search + "+board+game"})\n
                        :video_camera:[Tutorials](https://www.youtube.com/results?search_query={game_name_search + "+gameplay"})
            """)
    with st.expander("See Description"):
        st.write(best_recommendation.description)


def run_optional_filters(attributes_dict, recommender_results, filters_dict,
                         single_game_attributes):
    if filters_dict["max_players"] < attributes_dict['max_players']:
        recommender_results = recommender_results[recommender_results['maxplayers'] <= filters_dict["max_players"]]
    if filters_dict["same_designer"]:
        designer = extract_single_entity_from_list(df_col="boardgamedesigner",
                                                   single_game_attributes=single_game_attributes)
        recommender_results = recommender_results[recommender_results['boardgamedesigner'].str.contains(designer)]
    if filters_dict["same_publisher"]:
        publisher = extract_single_entity_from_list(df_col="boardgamepublisher",
                                                    single_game_attributes=single_game_attributes)
        recommender_results = recommender_results[recommender_results['boardgamepublisher'].str.contains(publisher)]
    if filters_dict["game_length"] != "all":
        recommender_results = recommender_results[recommender_results['game_length'] == filters_dict["game_length"]]
    return recommender_results


def run_default_filters(game_id, recommender_results):
    recommender_results = recommender_results[recommender_results['id'] != game_id]
    recommender_results = recommender_results[recommender_results['correlation'] >= MIN_CORRELATION]
    return recommender_results


def get_recommendations(game_id, games_info, games_matrix):
    single_game_corr = games_matrix.corrwith(games_matrix[game_id])
    single_game_corr = pd.DataFrame(single_game_corr, columns=["correlation"])
    single_game_corr = single_game_corr.dropna().sort_values(by="correlation", ascending=False)
    single_game_corr.index.name = 'id'
    recommender_results = single_game_corr.join(games_info.set_index("id")).reset_index()
    return recommender_results


def create_optional_filters(attributes_dict):
    st.write("---")
    filters = {}
    filter_left, filter_mid, filter_right = st.columns(3)
    with filter_left:
        filters["max_players"] = st.slider(min_value=2, max_value=attributes_dict['max_players'], key="players_selector",
                                label="Filter by player count", value=attributes_dict['max_players'], step=1)
    with filter_mid:
        filters["same_designer"] = st.checkbox(label="Same designer", key='designer_selector')
        filters["same_publisher"] = st.checkbox(label="Same publisher", key='publisher_selector')
    with filter_right:
        filters["game_length"] = st.select_slider(label="Filter by game length",
                                                  options=['under_an_hour', "one_or_two_hours",
                                                           "two_to_three_hours", "over_three_hours", "all"],
                                                  format_func=format_list_output, key="length_selector",
                                                  value="all")
    return filters


def create_model_explanation():
    st.write("---")
    st.title("Board game recommendation")
    st.write("""
        looking for a new board game to play?\n 
        why not see what players like you recommend?\n
        Simply choose a game and see the recommendation.\n
        To make the experience more smooth, this is limited to the top 500 ranked games.
        """)


def main():
    # general set up
    config_page()
    recommendation_navigation = SessionNavigation(button_key='recommendation_number')
    games_found = False
    # about me section
    create_about_me_section()

    # model explanation
    with st.container():
        create_model_explanation()
    load = st.checkbox(label='Load Data', key="load_data")

    if load:
        games_info, games_matrix = load_data()
        attributes_dict = load_game_attributes(games_info=games_info)

        # game selector
        game_names, game_names_dict = get_games_names(games_info=games_info)
        with st.container():
            st.write("---")
            st.subheader("Which game do you like?")
            game_id = st.selectbox(label="choose a game", options=game_names, label_visibility="hidden",
                                   key="game_selector", on_change=recommendation_navigation.back_to_first)

        # optional filters
        with st.container():
            filters_dict = create_optional_filters(attributes_dict)

        # create recommendations
        if game_id != "":
            game_id = game_names_dict[game_id]
            recommender_results = get_recommendations(game_id=game_id,
                                                      games_info=games_info,
                                                      games_matrix=games_matrix)

            # filter results
            single_game_attributes = games_info.loc[games_info['id'] == game_id,:].to_dict('records')[0]
            recommender_results = run_default_filters(game_id=game_id, recommender_results=recommender_results)

            with st.container():
                st.write("---")
                st.write("Navigate between recommendations")
                back_button, next_button, prev_button, number = prepare_navigation_section()
                if next_button:
                    recommendation_navigation.next_result()
                if prev_button:
                    recommendation_navigation.prev_result()
                if back_button:
                    recommendation_navigation.back_to_first()
                with number:
                    st.write(st.session_state[recommendation_navigation.button_key] + 1)
                try:
                    recommender_results = run_optional_filters(attributes_dict=attributes_dict,
                                                               filters_dict=filters_dict,
                                                               recommender_results=recommender_results,
                                                               single_game_attributes=single_game_attributes)
                    best_recommendation = recommender_results.reset_index().loc[
                                          st.session_state['recommendation_number'], :]
                    games_found = True
                    game_name_adjusted = best_recommendation.primary.replace(" ", "_")
                    game_name_search = best_recommendation.primary.replace(" ", "+")
                except KeyError:
                    st.write("no games matches the current filter. Try out different filters")

        # visualize recommended result
        if games_found:
            with st.container():
                st.write("---")
                st.subheader("Then maybe you would like")
                create_recommendation_section(best_recommendation=best_recommendation,
                                              game_name_adjusted=game_name_adjusted,
                                              game_name_search=game_name_search)

        with st.container():
            create_contact_form()


if __name__ == '__main__':
    main()