### Quantifying Aggressiveness in Men's Tennis

# Introduction
Welcome to our submission for the 2024 CMSAC Reproducible Research Competition. Our coding process was done using a mix of Python and R, with a lot of the preprocessing and initial insights being done in Python and most of the modeling being done in R. We will be switching between both languages, but we will make sure you know when.

Our initial data can be found in Jeff Sackmann’s Github and was too big to attach here. It can be found here. As the link shows, the csv we used is called “charting-m-points-2020s.csv”. https://github.com/JeffSackmann/tennis_MatchChartingProject/blob/master/charting-m-points-2020s.csv. We were also only able to attach our Kmeans clustering datasets but have included code to save the larger, more important, datasets remotely.

# Context for attached files
The way we have documented our code below. The starting number indicates the relative order of the scripts according to the paper (although there is some overlap) and the last number indicates the section of the paper this script is for.

- 1---Processing_raw_data_2.3.ipynb - PYTHON - Used to process the raw data into shot-by-shot data by taking the rally codes and extrapolating them into multiple rows worth of data. Here, we converted Jeff Sackmann’s data to our liking as detailed in the paper.
- 2---Pre-model_data_wrangling_4.R - R -  Used to process the data to prepare for the model. This includes how we created the aggressiveness and reward metrics as well as some more datawrangling and variable creation. Input is the output of the first script.
- 3---Tiering_players_3.2.ipynb - PYTHON - Used to create the tiers for aggressiveness rating using a K-means clustering model - The input was the average aggressiveness of each player and the output is a csv with a column that categorizes the players based on aggressiveness level.
  - 3a---3_input.csv: list of players and their aggressiveness rating
  - 3b---3_output.csv: list of players, their aggressiveness rating, and the respective tier
- 4---Model_and_graphing_5.R - R - This is where the three XGBoost models were trained and tested. We also included the code for the graphs to make some pretty and color accessible visualizations of our results! Input is the main output from the second script. The output of the fifth script will also be inputted to test the model.
- 5---All_state_action_pairs_5.4.py - PYTHON - This is a simple script to create a dataset that contains every possible state (environment) and action pair as we detailed in the paper. This will ultimately be inputted into the XGBoost models to provide us with the optimal aggressiveness.
