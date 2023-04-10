import streamlit as st
import requests
from streamlit_lottie import st_lottie


def load_animation(url):
    request = requests.get(url=url)
    if request.status_code != 200:
        return None
    return request.json()


def local_css(file_path):
    with open(file=file_path) as css_file:
        st.markdown(f"<style>{css_file.read()}</style>", unsafe_allow_html=True)


def create_about_me_section():
    st.subheader("Hey, my name is Idan Ariav :wave:")
    about_text, about_image = st.columns((1, 2))
    animation = load_animation(url="https://assets3.lottiefiles.com/private_files/lf30_ajzyv37m.json")
    with about_text:
        with st.container():
            st.title("Data Analyst")
            st.write("""
            I am passionate about the field of data science, 
            and exploring new ways of harnessing machine learning 
            into meaningful tangible insights""")
            st.markdown("Explore additional projects: [Github](https://github.com/idanariav/Idans_portfolio)")
            st.markdown("View linkedin profile: [Linkedin](https://www.linkedin.com/in/idan-ariav/)")
    with about_image:
        st_lottie(animation_data=animation, height=300, key='dashboard')


def create_contact_form():
    st.write("---")
    st.subheader("Let's connect")
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


def prepare_navigation_section(key, col_number=12):
    prev, number, following, back, *rest = st.columns(col_number)
    with prev:
        prev_button = st.button(label="Back")
    with following:
        next_button = st.button(label="Next")
    with back:
        back_button = st.button(label="First")
    return back_button, next_button, prev_button, number


def config_page(css_path):
    st.set_page_config(page_title="My website", page_icon=":tada:", layout="wide")
    local_css(file_path=css_path)


class SessionNavigation(object):
    def __init__(self, button_key):
        self.button_key = button_key
        if button_key not in st.session_state:
            st.session_state[button_key] = 0

    def back_to_first(self):
        st.session_state[self.button_key] = 0

    def next_result(self):
        st.session_state[self.button_key] += 1

    def prev_result(self):
        if st.session_state[self.button_key] > 0:
            st.session_state[self.button_key] -= 1