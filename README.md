# Quantifying Aggressiveness in Men's Tennis

## Introduction
Welcome to our submission for the 2024 CMSAC Reproducible Research Competition. Aggressiveness is one of the most critical yet understudied aspects of men's tennis. In our project, we used a mix of machine learning and data analysis to determine the ideal aggressiveness a player should play with for a given shot based on the match context. Our final paper is also attached, labeled as "0---Paper.pdf"

Our coding process was done using a mix of Python and R, with a lot of the preprocessing and initial insights being done in Python and most of the modeling being done in R. We will be switching between both languages, but we will make sure you know when. We have also attached an "**R_helper**" file which is a markdown that provides pretty extensive commentary for both R scripts. It should do a good job in explaining the logic behind most of the code we wrote.

Our initial data can be found in Jeff Sackmann’s Github and was too big to attach here. It can be found [here](https://github.com/JeffSackmann/tennis_MatchChartingProject/blob/master/charting-m-points-2020s.csv). As the link shows, the csv we used is called “charting-m-points-2020s.csv”. We were also only able to attach our Kmeans clustering datasets but have included code to save the larger, more important, datasets remotely.

## Context for attached files
The way we have documented our code below. The starting number indicates the relative order of the scripts according to the paper (although there is some overlap) and the last number indicates the section of the paper this script is for.

- 0---Paper.pdf - Final Paper


- 1---Processing_raw_data_2.3.ipynb - JUPYTER NOTEBOOK - Used to process the raw data into shot-by-shot data by taking the rally codes and extrapolating them into multiple rows worth of data. Here, we converted Jeff Sackmann’s data to our liking as detailed in the paper.


- 2---Pre-model_data_wrangling_4.R - R -  Used to process the data to prepare for the model. This includes how we created the aggressiveness and reward metrics as well as some more datawrangling and variable creation. Input is the output of the first script.

  
- 3---Tiering_players_3.2.ipynb - JUPYTER NOTEBOOK - Used to create the tiers for aggressiveness rating using a K-means clustering model - The input was the average aggressiveness of each player and the output is a csv with a column that categorizes the players based on aggressiveness level.
  
  - 3a---3_input.csv: list of players and their aggressiveness rating
  
  - 3b---3_output.csv: list of players, their aggressiveness rating, and the respective tier

  
- 4---Model_and_graphing_5.R - R - This is where the three XGBoost models were trained and tested. We also included the code for the graphs to make some pretty and color accessible visualizations of our results! Input is the main output from the second script. The output of the fifth script will also be inputted to test the model.

  
- 5---All_state_action_pairs_5.4.py - PYTHON - This is a simple script to create a dataset that contains every possible state (environment) and action pair as we detailed in the paper. This will ultimately be inputted into the XGBoost models to provide us with the optimal aggressiveness.


  - 5b---5_output.csv: Every possible state/action pair. If you have this output, you do not need to run script 5. This will be inputted into script 4 as detailed in section 5.4 of our paper.

- R_helper.md - R MARKDOWN - Walks you through both R files and shows when to use the Python files
  
# How to reproduce our project

If you would like to reproduce our project, this is what you can do:
1) Download the initial data which we have linked
2) Run script 1 and save the output
3) Run script 2 by using the output of script 1 as the input. In the middle of script 2, we have average player aggressiveness. Save that as it will be the input for script 3. Continue running the script and save the final output. This will be used for script 4.
4) Run script 3 by using the first output of script 2 as the input. Save the output.
5) Run script 4 - Use the final output of script 2 as the input. After you train and test the initial XGBoost model, it is time to run script 5.
6) Run Script 5 or load the sa_pairs csv into script 4. Save the output and load it into script 4.
7) Continue running script 4. This will now get you the ideal aggressiveness for every shot in our dataset. We can also visualize the data now.

Note: If you follow the R_helper markdown file, you will get the code for both script 2 and script 4.
