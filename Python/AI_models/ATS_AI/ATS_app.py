from dotenv import load_dotenv
import base64
import streamlit as st
import os
import io
import pdf2image
import google.generativeai as genai
from streamlit_utils import config_page, create_about_me_section, create_contact_form

# Load env with api key and configure
load_dotenv()
genai.configure(api_key=os.getenv("GOOGLE_API_KEY"))

# Constants
RESUME_ANALYSIS_PROMPT = """
 You are an experienced Technical Human Resource Manager in the field of data science and data analytics,
 your task is to review the provided resume against the job description. 
  Please share your professional evaluation on whether the candidate's profile aligns with the role. 
 Highlight the strengths and weaknesses of the applicant in relation to the specified job requirements.
"""

JOB_COMPARISON_PROMPT = """
You are an skilled ATS (Applicant Tracking System) scanner with a deep understanding of data science 
and ATS functionality, your task is to evaluate the resume against the provided job description. 
give me the percentage of match if the resume matches the job description. 
First the output should come as percentage and then keywords missing and last final thoughts.
"""


# Functions

def get_gemini_response(job_desc, cv_content, prompt):
    model = genai.GenerativeModel('gemini-pro-vision')
    api_response = model.generate_content([job_desc, cv_content[0], prompt])
    return api_response.text


def input_pdf_setup(uploaded_file):
    if uploaded_file is not None:
        # Convert the PDF to image
        images = pdf2image.convert_from_bytes(uploaded_file.read())
        first_page = images[0]

        # Convert to bytes
        img_byte_arr = io.BytesIO()
        first_page.save(img_byte_arr, format='JPEG')
        img_byte_arr = img_byte_arr.getvalue()

        pdf_parts = [
            {
                "mime_type": "image/jpeg",
                "data": base64.b64encode(img_byte_arr).decode()  # encode to base64
            }
        ]
        return pdf_parts
    else:
        raise FileNotFoundError("No file uploaded")


def analyse_api_response(uploaded_file, prompt, job_desc):
    pdf_content = input_pdf_setup(uploaded_file=uploaded_file)
    response = get_gemini_response(prompt=prompt, cv_content=pdf_content, job_desc=job_desc)
    st.subheader("The Response is")
    st.write(response)


# Streamlit App

# general set up
config_page()

# about me section
create_about_me_section()
with st.container():
    st.write("---")
    st.title("ATS Tracking System")
    st.write("""
    Today machine learning models are the first to review your resume, \n 
    so why not see how well your resume preforms? \n 
    Copy a job description and upload your resume \n
    to see your match score and insights on relevant missing skills
    """)
job_description = st.text_area(key="input", label="Job Description",
                               placeholder="Please copy the job description here")
cv_pdf = st.file_uploader("Upload your resume (PDF)...", type=["pdf"])

if cv_pdf is not None:
    st.write("PDF Uploaded Successfully")
    with st.container():
        resume_analysis = st.button("Tell Me About the Resume")
        job_comparison = st.button("Percentage match")
        if resume_analysis:
            analyse_api_response(prompt=RESUME_ANALYSIS_PROMPT, uploaded_file=cv_pdf, job_desc=job_description)
        elif job_comparison:
            analyse_api_response(prompt=JOB_COMPARISON_PROMPT, uploaded_file=cv_pdf, job_desc=job_description)

with st.container():
    create_contact_form()