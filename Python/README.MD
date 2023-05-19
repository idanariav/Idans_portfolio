# Python Projects

These are projects written in Python which involve data analytics and data science models. 
Some model are based using public available data through websites such as Kaggle, others through self generated data.

## Project list

For projects presented on Streamlit, feel free to explore the "streamlit_projects folder".
For simplicity, they are also included on this project list.

### ML Models

#### Supervised Learning

##### Logistic regression

Yes it's a classic (in movies as well as in data science), but my Titanic project
is where it all began. I preform EDA and by using a logistic regression model
we can predict who would have had a chance to survive the disaster. [Link](https://github.com/idanariav/Idans_portfolio/blob/main/Python/ML_models/Logistic%20Regression%20Model%20-%20Titanic.ipynb)

##### Sequential model

Using a neural network, we attempt to predict which clients will probably not pay back their loan.
This projects starts from EDA, data cleaning and computing missing data. Then we create a sequential model to predict the repayment odds and compare the results vs our known labels
using a confusion matrix. [Link](https://github.com/idanariav/Idans_portfolio/blob/main/Python/ML_models/Sequential%20Model%20-%20Loan%20Repayablity%20Prediction.ipynb)

### NLP

#### Sentiment analysis
Analysing comments taken from the Board Game Geek website, and comparing the sentiment score
with the actual rating to see how much can we learn from looking at the comment alone.
[Link](https://www.kaggle.com/code/idanariav/bgg-sentiment-analysis)

### Misc 

#### Recommender system
This is a simple yet elegant recommender model based on what similar users have rated.
For example, if you liked x, what would other users that have liked x recommend?
This model is designed to provide recommendations for board games, taken from the Board Game Geek website (like IMDB but for board games).
More details on the model, it's capabilities and other useful links can be found at the streamlit project page: [Link](https://bgg-recommender.streamlit.app)

#### Web scrapper

Using Scarpy and the native API of the Board Game Geek website (like IMDB but for board games),
we pulled board games metadata, rating and comments that could be used as a database for other projects
such as the [Recommender System](https://bgg-recommender.streamlit.app), and sentiment analysis.
[Link](https://github.com/idanariav/Idans_portfolio/blob/main/Python/bgg_scrapper/bgg_scrapper.py)


