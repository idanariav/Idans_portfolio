import streamlit as st
import requests
from streamlit_lottie import st_lottie
import os

DIR_PATH = os.path.dirname(__file__)
CSS_PATH = os.path.join(DIR_PATH, "style", "style.css")
ANIMATION_URL = "https://lottie.host/413c3872-78b4-4006-9b87-0e4323ece205/gs3RezDVz9.json"
PORTFOLIO_URL = "https://github.com/idanariav/Idans_portfolio/tree/main/Python"
LINKEDIN_URL = "https://www.linkedin.com/in/idan-ariav/"


def load_animation(url):
    request = requests.get(url=url)
    if request.status_code != 200:
        return None
    return request.json()


def local_css(file_path):
    with open(file_path) as css_file:
        st.markdown(f"<style>{css_file.read()}</style>", unsafe_allow_html=True)


def create_about_me_section():
    st.subheader("Hey, my name is Idan Ariav :wave:")
    about_text, about_image = st.columns((1, 2))
    animation = load_animation(url=ANIMATION_URL)
    with about_text:
        with st.container():
            st.title("Data Analyst")
            st.write("""
            I have a strong passion for the field of data science,
            and a deep interest in exploring innovative approaches to harnessing the power of machine learning. 
            My goal is to transform raw data into valuable and actionable insights that have a real impact.""")
            st.markdown(f"Additional projects: [Github]({PORTFOLIO_URL})")
            st.markdown(f"Linkedin profile: [Linkedin]({LINKEDIN_URL})")
    with about_image:
        st_lottie(animation_source=animation, height=300, key='dashboard')


def create_contact_form():
    st.write("---")
    st.subheader("Any feedback or recommendations? Let me know what you think")
    contact_form = """
            <form action="https://formsubmit.co/idanaok@proton.me" method="POST">
             <input type="hidden" name="_captcha" value="false">
             <input type="text" name="name" placeholder="Your name" required>
             <input type="email" name="email" placeholder="Your email" required>
             <textarea name="message" placeholder="Your message here" required> </textarea> 
             <button type="submit">Send</button>
            </form>
            """
    contact_left, contact_right = st.columns(2)
    with contact_left:
        st.markdown(contact_form, unsafe_allow_html=True)
    with contact_right:
        st.empty()


def prepare_navigation_section(col_number=12):
    prev, number, following, back, *rest = st.columns(col_number)
    with prev:
        prev_button = st.button(label="Back")
    with following:
        next_button = st.button(label="Next")
    with back:
        back_button = st.button(label="First")
    return back_button, next_button, prev_button, number


def config_page():
    st.set_page_config(page_title="ATS system", page_icon=":tada:", layout="wide")
    local_css(file_path=CSS_PATH)

