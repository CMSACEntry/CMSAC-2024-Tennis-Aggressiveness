## Setting up the data

The first step is to load in the libraries that we will be working with.

``` r
library(tidyverse)
library(ggplot2)
library(xgboost)
library(caret)
library(stringr)
library(purrr)
library(rvest)
library(gt)
```

Next, we load in our data and do some initial conversions. We then
convert some NA values in our columns to “missing”. This makes it easier
to work with. The variable we are interested in for aggressiveness are
depth, serve zone, shot type, outcome, and direction. We also create a
“next direction” variable which shows where the ball goes after the
hitter hits the ball.

``` r
tennisdata <- read.csv("preprocessed_aggressiveness_mensince2020.csv")

tennisdata$depth[is.na(tennisdata$depth)] <- "missing"
tennisdata$serve_zone[is.na(tennisdata$serve_zone)] <- "missing"
tennisdata$shot_type[is.na(tennisdata$shot_type)] <- "missing"
tennisdata$outcome[is.na(tennisdata$outcome)] <- "missing"
tennisdata$direction[is.na(tennisdata$direction)] <- "missing"
tennisdata$next_direction <- lead(tennisdata$direction)

gt(head(tennisdata))
```
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="match_id">match_id</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Pt">Pt</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Set1">Set1</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Set2">Set2</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Gm1">Gm1</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Gm2">Gm2</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Pts">Pts</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Player_1_Pts">Player_1_Pts</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Player_2_Pts">Player_2_Pts</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Gm.">Gm.</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="TbSet">TbSet</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="TB.">TB.</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="TBpt">TBpt</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Svr">Svr</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Ret">Ret</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Serving">Serving</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="X1st">X1st</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="X2nd">X2nd</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Notes">Notes</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="X1stSV">X1stSV</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="X2ndSV">X2ndSV</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="X1stIn">X1stIn</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="X2ndIn">X2ndIn</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="isAce">isAce</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="isUnret">isUnret</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="isRallyWinner">isRallyWinner</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="isForced">isForced</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="isUnforced">isUnforced</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="isDouble">isDouble</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="PtWinner">PtWinner</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="isSvrWinner">isSvrWinner</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="rallyCount">rallyCount</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="serve_zone">serve_zone</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="shot_type">shot_type</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="direction">direction</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="error_type">error_type</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="outcome">outcome</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="depth">depth</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="position">position</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="net_cord">net_cord</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="approach">approach</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="serve_and_volley">serve_and_volley</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="shot_num">shot_num</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Player_1">Player_1</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Player_2">Player_2</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Hitting_Player">Hitting_Player</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="pt_id">pt_id</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="gm_id">gm_id</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="set_id">set_id</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="PtWinner_Player">PtWinner_Player</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="GmWinner_Player">GmWinner_Player</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="SetWinner_Player">SetWinner_Player</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="MatchWinner_Player">MatchWinner_Player</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="shot_in">shot_in</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="next_direction">next_direction</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="match_id" class="gt_row gt_left">20230908-M-US_Open-SF-Novak_Djokovic_-Ben_Shelton</td>
<td headers="Pt" class="gt_row gt_right">1</td>
<td headers="Set1" class="gt_row gt_right">0</td>
<td headers="Set2" class="gt_row gt_right">0</td>
<td headers="Gm1" class="gt_row gt_right">0</td>
<td headers="Gm2" class="gt_row gt_right">0</td>
<td headers="Pts" class="gt_row gt_right">0-0</td>
<td headers="Player_1_Pts" class="gt_row gt_right">0</td>
<td headers="Player_2_Pts" class="gt_row gt_right">0</td>
<td headers="Gm." class="gt_row gt_right">1 (1)</td>
<td headers="TbSet" class="gt_row gt_right">1</td>
<td headers="TB." class="gt_row gt_right">0</td>
<td headers="TBpt" class="gt_row gt_right">NA</td>
<td headers="Svr" class="gt_row gt_right">1</td>
<td headers="Ret" class="gt_row gt_right">2</td>
<td headers="Serving" class="gt_row gt_left">ND</td>
<td headers="X1st" class="gt_row gt_left">6f2n#</td>
<td headers="X2nd" class="gt_row gt_left"></td>
<td headers="Notes" class="gt_row gt_left">Roof closed</td>
<td headers="X1stSV" class="gt_row gt_right">0</td>
<td headers="X2ndSV" class="gt_row gt_right">NA</td>
<td headers="X1stIn" class="gt_row gt_right">1</td>
<td headers="X2ndIn" class="gt_row gt_right">NA</td>
<td headers="isAce" class="gt_row gt_left">False</td>
<td headers="isUnret" class="gt_row gt_left">False</td>
<td headers="isRallyWinner" class="gt_row gt_left">False</td>
<td headers="isForced" class="gt_row gt_left">True</td>
<td headers="isUnforced" class="gt_row gt_left">False</td>
<td headers="isDouble" class="gt_row gt_left">False</td>
<td headers="PtWinner" class="gt_row gt_right">1</td>
<td headers="isSvrWinner" class="gt_row gt_right">1</td>
<td headers="rallyCount" class="gt_row gt_right">1</td>
<td headers="serve_zone" class="gt_row gt_left">6</td>
<td headers="shot_type" class="gt_row gt_left">S</td>
<td headers="direction" class="gt_row gt_left">missing</td>
<td headers="error_type" class="gt_row gt_left"></td>
<td headers="outcome" class="gt_row gt_right"></td>
<td headers="depth" class="gt_row gt_left">missing</td>
<td headers="position" class="gt_row gt_right"></td>
<td headers="net_cord" class="gt_row gt_right"></td>
<td headers="approach" class="gt_row gt_right"></td>
<td headers="serve_and_volley" class="gt_row gt_left">False</td>
<td headers="shot_num" class="gt_row gt_right">1</td>
<td headers="Player_1" class="gt_row gt_left">Novak Djokovic </td>
<td headers="Player_2" class="gt_row gt_left">Ben Shelton</td>
<td headers="Hitting_Player" class="gt_row gt_left">Novak Djokovic </td>
<td headers="pt_id" class="gt_row gt_left">20230908-M-US_Open-SF-Novak_Djokovic_-Ben_Shelton_Pt1</td>
<td headers="gm_id" class="gt_row gt_left">20230908-M-US_Open-SF-Novak_Djokovic_-Ben_Shelton_Set1_Game1</td>
<td headers="set_id" class="gt_row gt_left">20230908-M-US_Open-SF-Novak_Djokovic_-Ben_Shelton_Set1</td>
<td headers="PtWinner_Player" class="gt_row gt_left">Novak Djokovic </td>
<td headers="GmWinner_Player" class="gt_row gt_left">Novak Djokovic </td>
<td headers="SetWinner_Player" class="gt_row gt_left">Novak Djokovic </td>
<td headers="MatchWinner_Player" class="gt_row gt_left">Novak Djokovic </td>
<td headers="shot_in" class="gt_row gt_right">1</td>
<td headers="next_direction" class="gt_row gt_left">2</td></tr>
    <tr><td headers="match_id" class="gt_row gt_left">20230908-M-US_Open-SF-Novak_Djokovic_-Ben_Shelton</td>
<td headers="Pt" class="gt_row gt_right">1</td>
<td headers="Set1" class="gt_row gt_right">0</td>
<td headers="Set2" class="gt_row gt_right">0</td>
<td headers="Gm1" class="gt_row gt_right">0</td>
<td headers="Gm2" class="gt_row gt_right">0</td>
<td headers="Pts" class="gt_row gt_right">0-0</td>
<td headers="Player_1_Pts" class="gt_row gt_right">0</td>
<td headers="Player_2_Pts" class="gt_row gt_right">0</td>
<td headers="Gm." class="gt_row gt_right">1 (1)</td>
<td headers="TbSet" class="gt_row gt_right">1</td>
<td headers="TB." class="gt_row gt_right">0</td>
<td headers="TBpt" class="gt_row gt_right">NA</td>
<td headers="Svr" class="gt_row gt_right">1</td>
<td headers="Ret" class="gt_row gt_right">2</td>
<td headers="Serving" class="gt_row gt_left">ND</td>
<td headers="X1st" class="gt_row gt_left">6f2n#</td>
<td headers="X2nd" class="gt_row gt_left"></td>
<td headers="Notes" class="gt_row gt_left">Roof closed</td>
<td headers="X1stSV" class="gt_row gt_right">0</td>
<td headers="X2ndSV" class="gt_row gt_right">NA</td>
<td headers="X1stIn" class="gt_row gt_right">1</td>
<td headers="X2ndIn" class="gt_row gt_right">NA</td>
<td headers="isAce" class="gt_row gt_left">False</td>
<td headers="isUnret" class="gt_row gt_left">False</td>
<td headers="isRallyWinner" class="gt_row gt_left">False</td>
<td headers="isForced" class="gt_row gt_left">True</td>
<td headers="isUnforced" class="gt_row gt_left">False</td>
<td headers="isDouble" class="gt_row gt_left">False</td>
<td headers="PtWinner" class="gt_row gt_right">1</td>
<td headers="isSvrWinner" class="gt_row gt_right">1</td>
<td headers="rallyCount" class="gt_row gt_right">1</td>
<td headers="serve_zone" class="gt_row gt_left">missing</td>
<td headers="shot_type" class="gt_row gt_left">f</td>
<td headers="direction" class="gt_row gt_left">2</td>
<td headers="error_type" class="gt_row gt_left">n</td>
<td headers="outcome" class="gt_row gt_right">#</td>
<td headers="depth" class="gt_row gt_left">missing</td>
<td headers="position" class="gt_row gt_right"></td>
<td headers="net_cord" class="gt_row gt_right"></td>
<td headers="approach" class="gt_row gt_right"></td>
<td headers="serve_and_volley" class="gt_row gt_left">False</td>
<td headers="shot_num" class="gt_row gt_right">2</td>
<td headers="Player_1" class="gt_row gt_left">Novak Djokovic </td>
<td headers="Player_2" class="gt_row gt_left">Ben Shelton</td>
<td headers="Hitting_Player" class="gt_row gt_left">Ben Shelton</td>
<td headers="pt_id" class="gt_row gt_left">20230908-M-US_Open-SF-Novak_Djokovic_-Ben_Shelton_Pt1</td>
<td headers="gm_id" class="gt_row gt_left">20230908-M-US_Open-SF-Novak_Djokovic_-Ben_Shelton_Set1_Game1</td>
<td headers="set_id" class="gt_row gt_left">20230908-M-US_Open-SF-Novak_Djokovic_-Ben_Shelton_Set1</td>
<td headers="PtWinner_Player" class="gt_row gt_left">Novak Djokovic </td>
<td headers="GmWinner_Player" class="gt_row gt_left">Novak Djokovic </td>
<td headers="SetWinner_Player" class="gt_row gt_left">Novak Djokovic </td>
<td headers="MatchWinner_Player" class="gt_row gt_left">Novak Djokovic </td>
<td headers="shot_in" class="gt_row gt_right">0</td>
<td headers="next_direction" class="gt_row gt_left">missing</td></tr>
    <tr><td headers="match_id" class="gt_row gt_left">20230908-M-US_Open-SF-Novak_Djokovic_-Ben_Shelton</td>
<td headers="Pt" class="gt_row gt_right">2</td>
<td headers="Set1" class="gt_row gt_right">0</td>
<td headers="Set2" class="gt_row gt_right">0</td>
<td headers="Gm1" class="gt_row gt_right">0</td>
<td headers="Gm2" class="gt_row gt_right">0</td>
<td headers="Pts" class="gt_row gt_right">15-0</td>
<td headers="Player_1_Pts" class="gt_row gt_right">15</td>
<td headers="Player_2_Pts" class="gt_row gt_right">0</td>
<td headers="Gm." class="gt_row gt_right">1 (2)</td>
<td headers="TbSet" class="gt_row gt_right">1</td>
<td headers="TB." class="gt_row gt_right">0</td>
<td headers="TBpt" class="gt_row gt_right">NA</td>
<td headers="Svr" class="gt_row gt_right">1</td>
<td headers="Ret" class="gt_row gt_right">2</td>
<td headers="Serving" class="gt_row gt_left">ND</td>
<td headers="X1st" class="gt_row gt_left">4n</td>
<td headers="X2nd" class="gt_row gt_left">6b19f1b2s1f+3f2j2*</td>
<td headers="Notes" class="gt_row gt_left"></td>
<td headers="X1stSV" class="gt_row gt_right">0</td>
<td headers="X2ndSV" class="gt_row gt_right">0</td>
<td headers="X1stIn" class="gt_row gt_right">0</td>
<td headers="X2ndIn" class="gt_row gt_right">1</td>
<td headers="isAce" class="gt_row gt_left">False</td>
<td headers="isUnret" class="gt_row gt_left">False</td>
<td headers="isRallyWinner" class="gt_row gt_left">True</td>
<td headers="isForced" class="gt_row gt_left">False</td>
<td headers="isUnforced" class="gt_row gt_left">False</td>
<td headers="isDouble" class="gt_row gt_left">False</td>
<td headers="PtWinner" class="gt_row gt_right">2</td>
<td headers="isSvrWinner" class="gt_row gt_right">0</td>
<td headers="rallyCount" class="gt_row gt_right">8</td>
<td headers="serve_zone" class="gt_row gt_left">4</td>
<td headers="shot_type" class="gt_row gt_left">S</td>
<td headers="direction" class="gt_row gt_left">missing</td>
<td headers="error_type" class="gt_row gt_left">n</td>
<td headers="outcome" class="gt_row gt_right"></td>
<td headers="depth" class="gt_row gt_left">missing</td>
<td headers="position" class="gt_row gt_right"></td>
<td headers="net_cord" class="gt_row gt_right"></td>
<td headers="approach" class="gt_row gt_right"></td>
<td headers="serve_and_volley" class="gt_row gt_left">False</td>
<td headers="shot_num" class="gt_row gt_right">1</td>
<td headers="Player_1" class="gt_row gt_left">Novak Djokovic </td>
<td headers="Player_2" class="gt_row gt_left">Ben Shelton</td>
<td headers="Hitting_Player" class="gt_row gt_left">Novak Djokovic </td>
<td headers="pt_id" class="gt_row gt_left">20230908-M-US_Open-SF-Novak_Djokovic_-Ben_Shelton_Pt2</td>
<td headers="gm_id" class="gt_row gt_left">20230908-M-US_Open-SF-Novak_Djokovic_-Ben_Shelton_Set1_Game1</td>
<td headers="set_id" class="gt_row gt_left">20230908-M-US_Open-SF-Novak_Djokovic_-Ben_Shelton_Set1</td>
<td headers="PtWinner_Player" class="gt_row gt_left">Ben Shelton</td>
<td headers="GmWinner_Player" class="gt_row gt_left">Novak Djokovic </td>
<td headers="SetWinner_Player" class="gt_row gt_left">Novak Djokovic </td>
<td headers="MatchWinner_Player" class="gt_row gt_left">Novak Djokovic </td>
<td headers="shot_in" class="gt_row gt_right">0</td>
<td headers="next_direction" class="gt_row gt_left">missing</td></tr>
    <tr><td headers="match_id" class="gt_row gt_left">20230908-M-US_Open-SF-Novak_Djokovic_-Ben_Shelton</td>
<td headers="Pt" class="gt_row gt_right">2</td>
<td headers="Set1" class="gt_row gt_right">0</td>
<td headers="Set2" class="gt_row gt_right">0</td>
<td headers="Gm1" class="gt_row gt_right">0</td>
<td headers="Gm2" class="gt_row gt_right">0</td>
<td headers="Pts" class="gt_row gt_right">15-0</td>
<td headers="Player_1_Pts" class="gt_row gt_right">15</td>
<td headers="Player_2_Pts" class="gt_row gt_right">0</td>
<td headers="Gm." class="gt_row gt_right">1 (2)</td>
<td headers="TbSet" class="gt_row gt_right">1</td>
<td headers="TB." class="gt_row gt_right">0</td>
<td headers="TBpt" class="gt_row gt_right">NA</td>
<td headers="Svr" class="gt_row gt_right">1</td>
<td headers="Ret" class="gt_row gt_right">2</td>
<td headers="Serving" class="gt_row gt_left">ND</td>
<td headers="X1st" class="gt_row gt_left">4n</td>
<td headers="X2nd" class="gt_row gt_left">6b19f1b2s1f+3f2j2*</td>
<td headers="Notes" class="gt_row gt_left"></td>
<td headers="X1stSV" class="gt_row gt_right">0</td>
<td headers="X2ndSV" class="gt_row gt_right">0</td>
<td headers="X1stIn" class="gt_row gt_right">0</td>
<td headers="X2ndIn" class="gt_row gt_right">1</td>
<td headers="isAce" class="gt_row gt_left">False</td>
<td headers="isUnret" class="gt_row gt_left">False</td>
<td headers="isRallyWinner" class="gt_row gt_left">True</td>
<td headers="isForced" class="gt_row gt_left">False</td>
<td headers="isUnforced" class="gt_row gt_left">False</td>
<td headers="isDouble" class="gt_row gt_left">False</td>
<td headers="PtWinner" class="gt_row gt_right">2</td>
<td headers="isSvrWinner" class="gt_row gt_right">0</td>
<td headers="rallyCount" class="gt_row gt_right">8</td>
<td headers="serve_zone" class="gt_row gt_left">6</td>
<td headers="shot_type" class="gt_row gt_left">S</td>
<td headers="direction" class="gt_row gt_left">missing</td>
<td headers="error_type" class="gt_row gt_left"></td>
<td headers="outcome" class="gt_row gt_right"></td>
<td headers="depth" class="gt_row gt_left">missing</td>
<td headers="position" class="gt_row gt_right"></td>
<td headers="net_cord" class="gt_row gt_right"></td>
<td headers="approach" class="gt_row gt_right"></td>
<td headers="serve_and_volley" class="gt_row gt_left">False</td>
<td headers="shot_num" class="gt_row gt_right">1</td>
<td headers="Player_1" class="gt_row gt_left">Novak Djokovic </td>
<td headers="Player_2" class="gt_row gt_left">Ben Shelton</td>
<td headers="Hitting_Player" class="gt_row gt_left">Novak Djokovic </td>
<td headers="pt_id" class="gt_row gt_left">20230908-M-US_Open-SF-Novak_Djokovic_-Ben_Shelton_Pt2</td>
<td headers="gm_id" class="gt_row gt_left">20230908-M-US_Open-SF-Novak_Djokovic_-Ben_Shelton_Set1_Game1</td>
<td headers="set_id" class="gt_row gt_left">20230908-M-US_Open-SF-Novak_Djokovic_-Ben_Shelton_Set1</td>
<td headers="PtWinner_Player" class="gt_row gt_left">Ben Shelton</td>
<td headers="GmWinner_Player" class="gt_row gt_left">Novak Djokovic </td>
<td headers="SetWinner_Player" class="gt_row gt_left">Novak Djokovic </td>
<td headers="MatchWinner_Player" class="gt_row gt_left">Novak Djokovic </td>
<td headers="shot_in" class="gt_row gt_right">1</td>
<td headers="next_direction" class="gt_row gt_left">1</td></tr>
    <tr><td headers="match_id" class="gt_row gt_left">20230908-M-US_Open-SF-Novak_Djokovic_-Ben_Shelton</td>
<td headers="Pt" class="gt_row gt_right">2</td>
<td headers="Set1" class="gt_row gt_right">0</td>
<td headers="Set2" class="gt_row gt_right">0</td>
<td headers="Gm1" class="gt_row gt_right">0</td>
<td headers="Gm2" class="gt_row gt_right">0</td>
<td headers="Pts" class="gt_row gt_right">15-0</td>
<td headers="Player_1_Pts" class="gt_row gt_right">15</td>
<td headers="Player_2_Pts" class="gt_row gt_right">0</td>
<td headers="Gm." class="gt_row gt_right">1 (2)</td>
<td headers="TbSet" class="gt_row gt_right">1</td>
<td headers="TB." class="gt_row gt_right">0</td>
<td headers="TBpt" class="gt_row gt_right">NA</td>
<td headers="Svr" class="gt_row gt_right">1</td>
<td headers="Ret" class="gt_row gt_right">2</td>
<td headers="Serving" class="gt_row gt_left">ND</td>
<td headers="X1st" class="gt_row gt_left">4n</td>
<td headers="X2nd" class="gt_row gt_left">6b19f1b2s1f+3f2j2*</td>
<td headers="Notes" class="gt_row gt_left"></td>
<td headers="X1stSV" class="gt_row gt_right">0</td>
<td headers="X2ndSV" class="gt_row gt_right">0</td>
<td headers="X1stIn" class="gt_row gt_right">0</td>
<td headers="X2ndIn" class="gt_row gt_right">1</td>
<td headers="isAce" class="gt_row gt_left">False</td>
<td headers="isUnret" class="gt_row gt_left">False</td>
<td headers="isRallyWinner" class="gt_row gt_left">True</td>
<td headers="isForced" class="gt_row gt_left">False</td>
<td headers="isUnforced" class="gt_row gt_left">False</td>
<td headers="isDouble" class="gt_row gt_left">False</td>
<td headers="PtWinner" class="gt_row gt_right">2</td>
<td headers="isSvrWinner" class="gt_row gt_right">0</td>
<td headers="rallyCount" class="gt_row gt_right">8</td>
<td headers="serve_zone" class="gt_row gt_left">missing</td>
<td headers="shot_type" class="gt_row gt_left">b</td>
<td headers="direction" class="gt_row gt_left">1</td>
<td headers="error_type" class="gt_row gt_left"></td>
<td headers="outcome" class="gt_row gt_right"></td>
<td headers="depth" class="gt_row gt_left">9</td>
<td headers="position" class="gt_row gt_right"></td>
<td headers="net_cord" class="gt_row gt_right"></td>
<td headers="approach" class="gt_row gt_right"></td>
<td headers="serve_and_volley" class="gt_row gt_left">False</td>
<td headers="shot_num" class="gt_row gt_right">2</td>
<td headers="Player_1" class="gt_row gt_left">Novak Djokovic </td>
<td headers="Player_2" class="gt_row gt_left">Ben Shelton</td>
<td headers="Hitting_Player" class="gt_row gt_left">Ben Shelton</td>
<td headers="pt_id" class="gt_row gt_left">20230908-M-US_Open-SF-Novak_Djokovic_-Ben_Shelton_Pt2</td>
<td headers="gm_id" class="gt_row gt_left">20230908-M-US_Open-SF-Novak_Djokovic_-Ben_Shelton_Set1_Game1</td>
<td headers="set_id" class="gt_row gt_left">20230908-M-US_Open-SF-Novak_Djokovic_-Ben_Shelton_Set1</td>
<td headers="PtWinner_Player" class="gt_row gt_left">Ben Shelton</td>
<td headers="GmWinner_Player" class="gt_row gt_left">Novak Djokovic </td>
<td headers="SetWinner_Player" class="gt_row gt_left">Novak Djokovic </td>
<td headers="MatchWinner_Player" class="gt_row gt_left">Novak Djokovic </td>
<td headers="shot_in" class="gt_row gt_right">1</td>
<td headers="next_direction" class="gt_row gt_left">1</td></tr>
    <tr><td headers="match_id" class="gt_row gt_left">20230908-M-US_Open-SF-Novak_Djokovic_-Ben_Shelton</td>
<td headers="Pt" class="gt_row gt_right">2</td>
<td headers="Set1" class="gt_row gt_right">0</td>
<td headers="Set2" class="gt_row gt_right">0</td>
<td headers="Gm1" class="gt_row gt_right">0</td>
<td headers="Gm2" class="gt_row gt_right">0</td>
<td headers="Pts" class="gt_row gt_right">15-0</td>
<td headers="Player_1_Pts" class="gt_row gt_right">15</td>
<td headers="Player_2_Pts" class="gt_row gt_right">0</td>
<td headers="Gm." class="gt_row gt_right">1 (2)</td>
<td headers="TbSet" class="gt_row gt_right">1</td>
<td headers="TB." class="gt_row gt_right">0</td>
<td headers="TBpt" class="gt_row gt_right">NA</td>
<td headers="Svr" class="gt_row gt_right">1</td>
<td headers="Ret" class="gt_row gt_right">2</td>
<td headers="Serving" class="gt_row gt_left">ND</td>
<td headers="X1st" class="gt_row gt_left">4n</td>
<td headers="X2nd" class="gt_row gt_left">6b19f1b2s1f+3f2j2*</td>
<td headers="Notes" class="gt_row gt_left"></td>
<td headers="X1stSV" class="gt_row gt_right">0</td>
<td headers="X2ndSV" class="gt_row gt_right">0</td>
<td headers="X1stIn" class="gt_row gt_right">0</td>
<td headers="X2ndIn" class="gt_row gt_right">1</td>
<td headers="isAce" class="gt_row gt_left">False</td>
<td headers="isUnret" class="gt_row gt_left">False</td>
<td headers="isRallyWinner" class="gt_row gt_left">True</td>
<td headers="isForced" class="gt_row gt_left">False</td>
<td headers="isUnforced" class="gt_row gt_left">False</td>
<td headers="isDouble" class="gt_row gt_left">False</td>
<td headers="PtWinner" class="gt_row gt_right">2</td>
<td headers="isSvrWinner" class="gt_row gt_right">0</td>
<td headers="rallyCount" class="gt_row gt_right">8</td>
<td headers="serve_zone" class="gt_row gt_left">missing</td>
<td headers="shot_type" class="gt_row gt_left">f</td>
<td headers="direction" class="gt_row gt_left">1</td>
<td headers="error_type" class="gt_row gt_left"></td>
<td headers="outcome" class="gt_row gt_right"></td>
<td headers="depth" class="gt_row gt_left">missing</td>
<td headers="position" class="gt_row gt_right"></td>
<td headers="net_cord" class="gt_row gt_right"></td>
<td headers="approach" class="gt_row gt_right"></td>
<td headers="serve_and_volley" class="gt_row gt_left">False</td>
<td headers="shot_num" class="gt_row gt_right">3</td>
<td headers="Player_1" class="gt_row gt_left">Novak Djokovic </td>
<td headers="Player_2" class="gt_row gt_left">Ben Shelton</td>
<td headers="Hitting_Player" class="gt_row gt_left">Novak Djokovic </td>
<td headers="pt_id" class="gt_row gt_left">20230908-M-US_Open-SF-Novak_Djokovic_-Ben_Shelton_Pt2</td>
<td headers="gm_id" class="gt_row gt_left">20230908-M-US_Open-SF-Novak_Djokovic_-Ben_Shelton_Set1_Game1</td>
<td headers="set_id" class="gt_row gt_left">20230908-M-US_Open-SF-Novak_Djokovic_-Ben_Shelton_Set1</td>
<td headers="PtWinner_Player" class="gt_row gt_left">Ben Shelton</td>
<td headers="GmWinner_Player" class="gt_row gt_left">Novak Djokovic </td>
<td headers="SetWinner_Player" class="gt_row gt_left">Novak Djokovic </td>
<td headers="MatchWinner_Player" class="gt_row gt_left">Novak Djokovic </td>
<td headers="shot_in" class="gt_row gt_right">1</td>
<td headers="next_direction" class="gt_row gt_left">2</td></tr>
  </tbody>
  
  
</table>
</div>

Next, let’s extract the year and tournament from the match id variable.

``` r
tennisdata$year <- gsub("^(.{4}).*", "\\1", tennisdata$match_id)
tennisdata$tournament <- gsub("^[^-]*-[^-]*-([^-]*)-.*$", "\\1", tennisdata$match_id)
```

## Creating the aggressiveness metric

Next, we are going to create the aggressiveness metric. This is the
weighted sum of the smaller features. Recall we split this into rallies,
serves, and returns.

``` r
#Rallies
tennisdata$next_outcome <- lead(tennisdata$outcome)
tennisdata <- tennisdata %>%
  mutate(outcome_shottype_aggressiveness = ifelse(shot_type == "o" | shot_type == "p" | shot_type == "u" | shot_type == "y" | shot_type == "t", 2, ifelse(shot_type == "j" | shot_type == "k" | shot_type =="v" | shot_type == "z" | shot_type== "h" | shot_type == "i" , 1.5, ifelse(shot_type == "r" | shot_type == "s" | shot_type == "l" | shot_type == "m", 0, 1)))) %>%
  mutate(direction_aggressiveness = ifelse(direction == 1 & next_direction == 3 | direction == 3 & next_direction == 1, 2, ifelse(next_direction == 2, 0, 1))) %>%
  mutate(approach_aggressiveness = ifelse(approach == "+", 2, 1)) %>%
  mutate(agg_rating =  2*outcome_shottype_aggressiveness + direction_aggressiveness + 2*approach_aggressiveness) %>%
  mutate(agg_rating = ifelse(shot_type == "o" | shot_type == "p" | shot_type == "u" | shot_type == "y", 10, agg_rating))
#Serves
tennisdata <- tennisdata %>%
  mutate(serve_aggressiveness = ifelse(shot_num == 1 & serve_zone == 6, 2, ifelse(shot_num == 1 & serve_zone == 5, 0, 1))) %>%
  mutate(outcome_aggressiveness = ifelse(shot_num == 1 & next_outcome == "*" | shot_num == 1 & next_outcome == "#", 2, 1)) %>%
  mutate(agg_rating = ifelse(shot_num == 1, serve_aggressiveness*2 + outcome_aggressiveness*2, agg_rating)) %>%
  mutate(agg_rating = ifelse(shot_num == 1 & next_outcome == "*" | shot_num == 1 & next_outcome == "#", 10, agg_rating))
#Returns
tennisdata <- tennisdata %>%
  mutate(outcome_shottype_aggressiveness = ifelse(shot_num == 2 & shot_type == "o" | shot_type == "p" | shot_type == "u" | shot_type == "y" | shot_type == "t", 2, ifelse(shot_num == 2 & shot_type == "j" | shot_type == "k" | shot_type =="v" | shot_type == "z" | shot_type== "h" | shot_type == "i" , 1.5, ifelse(shot_num == 2 & shot_type == "r" | shot_type == "s" | shot_type == "l" | shot_type == "m", 0, 1)))) %>%
  mutate(direction_aggressiveness = ifelse(shot_num == 2 & direction == 1 & next_direction == 3 | shot_num == 2 & direction == 3 & next_direction == 1, 2, ifelse(shot_num == 2 & direction == 2, 0, 1))) %>%
  mutate(depth_aggressiveness = ifelse(shot_num == 2 & depth == 9, 2, ifelse(shot_num == 2 & depth == 7,0, 1))) %>%
  mutate(agg_rating = ifelse(shot_num == 2, 2*outcome_shottype_aggressiveness + direction_aggressiveness + 2*depth_aggressiveness, agg_rating)) %>%
  mutate(agg_rating = ifelse(shot_type == "o" | shot_type == "p" | shot_type == "u" | shot_type == "y", 10, agg_rating))
```

There might be a small issue with spacing after the player name in the
variables. Let us remove the unused spaces.

``` r
tennisdata <- tennisdata %>%
  mutate(opposing_player = ifelse(Hitting_Player == Player_1, Player_2, Player_1))

tennisdata1 <- tennisdata %>%
  mutate(Hitting_Player = gsub("(\\w)\\s+$", "\\1", Hitting_Player, perl = TRUE)) %>%
  mutate(opposing_player = gsub("(\\w)\\s+$", "\\1", opposing_player, perl = TRUE)) %>%
  mutate(Player_1 = gsub("(\\w)\\s+$", "\\1", Player_1, perl = TRUE)) %>%
  mutate(Player_2 = gsub("(\\w)\\s+$", "\\1", Player_2, perl = TRUE)) %>%
  mutate(PtWinner_Player = gsub("(\\w)\\s+$", "\\1", PtWinner_Player, perl = TRUE)) %>%
  mutate(GmWinner_Player = gsub("(\\w)\\s+$", "\\1", GmWinner_Player, perl = TRUE)) %>%
  mutate(SetWinner_Player = gsub("(\\w)\\s+$", "\\1", SetWinner_Player, perl = TRUE)) %>%
  mutate(MatchWinner_Player = gsub("(\\w)\\s+$", "\\1", MatchWinner_Player, perl = TRUE))
```

And now, we are going to take faults into account. We make an
“is_faults” variable and apply our aggressiveness metric.

``` r
tennisdata1 <- tennisdata1 %>%
  group_by(Pt, match_id) %>%
  mutate(
    is_fault = if_else(
      shot_num == 1 & shot_in == 0, 1, 0
    )
  ) %>%
  ungroup()



tennisdata1 <- tennisdata1 %>%
  group_by(match_id, Pt) %>%
  mutate(shots_left = max(shot_num) -shot_num) %>%
  ungroup()

tennisdata1$tournament <- ifelse(tennisdata1$tournament == "Wimbledon_", "Wimbledon", tennisdata1$tournament)
tennisdata1$tournament <- ifelse(tennisdata1$tournament == "Australian_Open_", "Australian_Open", tennisdata1$tournament) 
tennisdata1$tournament <- ifelse(tennisdata1$tournament == "Roland_Garros_", "Roland_Garros", tennisdata1$tournament) 
tennisdata1$tournament <- ifelse(tennisdata1$tournament == "s_Hertogenbosch_", "s_Hertogenbosch", tennisdata1$tournament)



tennisdata1 <- filter(tennisdata1, tournament != "Wimbledon_Juniors")
tennisdata1 <- filter(tennisdata1, tournament != "Australian_Open_Juniors")

table(tennisdata1$agg_rating)
```

    ## 
    ##      0      1      2      3      4      5      6      7      8      9     10 
    ##   3175   1484  90560  84667 347051 405860 232280  29330   8797     29  66181

``` r
tennisdata1 <- tennisdata1 %>%
  mutate(agg_rating = ifelse(is_fault == 1 & error_type == "d" & serve_zone == 6 | is_fault == 1 & error_type == "w" & serve_zone == 6 | is_fault == 1 & error_type == "x" & serve_zone == 6, 10, ifelse(is_fault == 1 & serve_zone == 5, 2, ifelse(is_fault == 1 & serve_zone == 4 | is_fault == 1 & serve_zone == 6 & error_type != "x" & error_type != "w" & error_type != "d" | is_fault == 1 & serve_zone == "missing", 5, agg_rating))))
```

## Player rankings

As mentioned in the paper, we decided to scrape yearly ATP rankings from
ESPN. This would display the top 150 players and we will eventually join
this with the dataset by year.

``` r
years <- 2009:2024

# Create the URLs
urls <- stringr::str_c("https://www.espn.com/tennis/rankings/_/season/", years)

# Function to scrape and parse a table from a given URL
scrape_table <- function(url) {
  # Read the HTML content
  page <- read_html(url)
  
  # Extract the table
  table <- page %>%
    html_element("table") %>%
    html_table()
  
  # Add a column for the year
  table$Year <- str_extract(url, "\\d{4}")
  
  return(table)
}

# Apply the scrape_table function to all URLs and combine results into a single dataframe
df_list <- map(urls, safely(scrape_table))

# Extract the results
results <- map(df_list, "result")

# Combine all dataframes into one
combined_df <- bind_rows(results)
```

    ## New names:
    ## • `` -> `...2`

``` r
colnames(combined_df)
```

    ## [1] "RK"     "NAME"   "POINTS" "Year"   "...2"   "AGE"

``` r
rankings <- select(combined_df, rank = RK, name = NAME, year = Year)

atp_rankings <- select(combined_df, rank = RK, name = NAME, points = POINTS, year = Year)

#Get the ranking of both the hitting player and the opposing player
tennisdata1 <- left_join(tennisdata1, rankings, by = c("Hitting_Player" = "name", "year"))
tennisdata1 <- left_join(tennisdata1, rankings, by = c("opposing_player" = "name", "year"))

colnames(tennisdata1)[colnames(tennisdata1) == "rank.x"] <- "hitter_rank"
colnames(tennisdata1)[colnames(tennisdata1) == "rank.y"] <- "oposition_rank"
```

## Some more data wrangling

Some miscellaneous data wrangling: Creating a “game counter” variable,
removing some data that was inputted weirdly, and creating a new
dataframe with only the columns of interest. We also changed those whom
we labeled as “unranked” to 151 to make player ranking a numeric
variable.

``` r
tennisdata1 <- tennisdata1 %>%
  mutate(game_counter = str_extract(Gm., "^[^\\(]+")) %>%
  mutate(game_counter = as.numeric(game_counter))
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `game_counter = as.numeric(game_counter)`.
    ## Caused by warning:
    ## ! NAs introduced by coercion

``` r
final_tennis <- select(tennisdata1, match_id, year, shot_num, tournament, player_1 = Player_1, player_2 = Player_2, point_counter = Pt, game_counter, set1 = Set1, set2 = Set2, gm1 = Gm1, gm2 = Gm2, pt1 = Player_1_Pts, pt2 = Player_2_Pts, hitting_player = Hitting_Player, opposing_player, pt_winner = PtWinner_Player, gm_winner = GmWinner_Player, set_winner = SetWinner_Player, match_winner = MatchWinner_Player, hitter_rank, opposition_rank = oposition_rank, outcome_shottype_aggressiveness, is_fault, direction_aggressiveness, approach_aggressiveness, serve_aggressiveness, outcome_aggressiveness, depth_aggressiveness, agg_rating, shots_left)
final_tennis <- final_tennis %>%
  filter(match_id != "20200103-M-ATP_Cup-RR-Alex_De_Minaur-Alexander_Zverev")
final_tennis$hitter_rank[is.na(final_tennis$hitter_rank)] <- "151"
final_tennis$opposition_rank[is.na(final_tennis$opposition_rank)] <- "151"
```

We now have an aggressiveness rating for every shot in the dataset, on
average how aggressive is each player? Let’s find out. We are also going
to identify if the hitting player was the point winner.

``` r
player_agg <- final_tennis %>%
  group_by(hitting_player) %>%
  summarize(shots_played = n(), agg = mean(agg_rating)) 
gt(head(player_agg))
```

<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="hitting_player">hitting_player</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="shots_played">shots_played</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="agg">agg</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="hitting_player" class="gt_row gt_left">Abedallah Shelbayh</td>
<td headers="shots_played" class="gt_row gt_right">724</td>
<td headers="agg" class="gt_row gt_right">4.997238</td></tr>
    <tr><td headers="hitting_player" class="gt_row gt_left">Adrian Andreev</td>
<td headers="shots_played" class="gt_row gt_right">739</td>
<td headers="agg" class="gt_row gt_right">5.265223</td></tr>
    <tr><td headers="hitting_player" class="gt_row gt_left">Adrian Mannarino</td>
<td headers="shots_played" class="gt_row gt_right">9211</td>
<td headers="agg" class="gt_row gt_right">4.988166</td></tr>
    <tr><td headers="hitting_player" class="gt_row gt_left">Albert Ramos</td>
<td headers="shots_played" class="gt_row gt_right">5219</td>
<td headers="agg" class="gt_row gt_right">4.942135</td></tr>
    <tr><td headers="hitting_player" class="gt_row gt_left">Alejandro Davidovich Fokina</td>
<td headers="shots_played" class="gt_row gt_right">14078</td>
<td headers="agg" class="gt_row gt_right">4.865464</td></tr>
    <tr><td headers="hitting_player" class="gt_row gt_left">Alejandro Tabilo</td>
<td headers="shots_played" class="gt_row gt_right">1450</td>
<td headers="agg" class="gt_row gt_right">4.920000</td></tr>
  </tbody>
  
  
</table>
</div>

Now we save this player aggressiveness data as a csv so that it can be used as the input for script 3.
``` r
write.csv(player_agg, "player_agg.csv")
```

## Reward metric

Remember that the basis of the metric is whether or not the player won
the point and how many shots are left in the rally. We group by player,
point, and match, and create a shots left in rally variable by counting
down. We then use this variable and the point winner variable as the
basis for our reward metric.

``` r
final_tennis$is_hitter_winner <- ifelse(final_tennis$hitting_player == final_tennis$pt_winner, 1, 0)

final_tennis <- final_tennis %>%
  group_by(point_counter, hitting_player, match_id) %>%
  mutate(shots_left_player = n() - row_number()) %>%
  ungroup()

final_tennis <- final_tennis %>%
  mutate(is_double_fault = ifelse(lag(is_fault) == 1 & is_fault == 1, 1, 0))
final_tennis <- final_tennis %>%
  mutate(reward = ifelse(is_hitter_winner == 1, 10*0.5^shots_left_player, -5*0.5^shots_left_player)) %>%
  mutate(reward = ifelse(is_fault == 1 & is_double_fault == 0, 0, ifelse(is_fault == 1 & is_double_fault == 1, -5, reward)))
```

## Binning the variables

Now, we categorize each level of the game - point, game, and set. Before
this, we need to do some more data manipulation. We remove some matches
that had all NAs and determine which player is “player 1” and which
player is “player 2”.

``` r
tennis_test <- final_tennis %>%
  mutate(player_number = ifelse(hitting_player == player_1, 1, 2))
str(tennis_test)
```

    ## tibble [1,268,027 × 36] (S3: tbl_df/tbl/data.frame)
    ##  $ match_id                       : chr [1:1268027] "20230908-M-US_Open-SF-Novak_Djokovic_-Ben_Shelton" "20230908-M-US_Open-SF-Novak_Djokovic_-Ben_Shelton" "20230908-M-US_Open-SF-Novak_Djokovic_-Ben_Shelton" "20230908-M-US_Open-SF-Novak_Djokovic_-Ben_Shelton" ...
    ##  $ year                           : chr [1:1268027] "2023" "2023" "2023" "2023" ...
    ##  $ shot_num                       : int [1:1268027] 1 2 1 1 2 3 4 5 6 7 ...
    ##  $ tournament                     : chr [1:1268027] "US_Open" "US_Open" "US_Open" "US_Open" ...
    ##  $ player_1                       : chr [1:1268027] "Novak Djokovic" "Novak Djokovic" "Novak Djokovic" "Novak Djokovic" ...
    ##  $ player_2                       : chr [1:1268027] "Ben Shelton" "Ben Shelton" "Ben Shelton" "Ben Shelton" ...
    ##  $ point_counter                  : int [1:1268027] 1 1 2 2 2 2 2 2 2 2 ...
    ##  $ game_counter                   : num [1:1268027] 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ set1                           : int [1:1268027] 0 0 0 0 0 0 0 0 0 0 ...
    ##  $ set2                           : int [1:1268027] 0 0 0 0 0 0 0 0 0 0 ...
    ##  $ gm1                            : int [1:1268027] 0 0 0 0 0 0 0 0 0 0 ...
    ##  $ gm2                            : int [1:1268027] 0 0 0 0 0 0 0 0 0 0 ...
    ##  $ pt1                            : chr [1:1268027] "0" "0" "15" "15" ...
    ##  $ pt2                            : chr [1:1268027] "0" "0" "0" "0" ...
    ##  $ hitting_player                 : chr [1:1268027] "Novak Djokovic" "Ben Shelton" "Novak Djokovic" "Novak Djokovic" ...
    ##  $ opposing_player                : chr [1:1268027] "Ben Shelton" "Novak Djokovic" "Ben Shelton" "Ben Shelton" ...
    ##  $ pt_winner                      : chr [1:1268027] "Novak Djokovic" "Novak Djokovic" "Ben Shelton" "Ben Shelton" ...
    ##  $ gm_winner                      : chr [1:1268027] "Novak Djokovic" "Novak Djokovic" "Novak Djokovic" "Novak Djokovic" ...
    ##  $ set_winner                     : chr [1:1268027] "Novak Djokovic" "Novak Djokovic" "Novak Djokovic" "Novak Djokovic" ...
    ##  $ match_winner                   : chr [1:1268027] "Novak Djokovic" "Novak Djokovic" "Novak Djokovic" "Novak Djokovic" ...
    ##  $ hitter_rank                    : chr [1:1268027] "1" "17" "1" "1" ...
    ##  $ opposition_rank                : chr [1:1268027] "17" "1" "17" "17" ...
    ##  $ outcome_shottype_aggressiveness: num [1:1268027] 1 1 1 1 1 1 1 0 1 1 ...
    ##  $ is_fault                       : num [1:1268027] 0 0 1 0 0 0 0 0 0 0 ...
    ##  $ direction_aggressiveness       : num [1:1268027] 1 0 1 1 1 1 1 1 1 1 ...
    ##  $ approach_aggressiveness        : num [1:1268027] 1 1 1 1 1 1 1 1 2 1 ...
    ##  $ serve_aggressiveness           : num [1:1268027] 2 1 1 2 1 1 1 1 1 1 ...
    ##  $ outcome_aggressiveness         : num [1:1268027] 2 1 1 1 1 1 1 1 1 1 ...
    ##  $ depth_aggressiveness           : num [1:1268027] 1 1 1 1 2 1 1 1 1 1 ...
    ##  $ agg_rating                     : num [1:1268027] 10 4 5 6 7 4 5 4 6 5 ...
    ##  $ shots_left                     : int [1:1268027] 1 0 6 6 5 4 3 2 1 0 ...
    ##  $ is_hitter_winner               : num [1:1268027] 1 0 0 0 1 0 1 0 1 0 ...
    ##  $ shots_left_player              : int [1:1268027] 0 0 4 3 2 2 1 1 0 0 ...
    ##  $ is_double_fault                : num [1:1268027] 0 0 0 0 0 0 0 0 0 0 ...
    ##  $ reward                         : num [1:1268027] 10 -5 0 -0.625 2.5 -1.25 5 -2.5 10 -5 ...
    ##  $ player_number                  : num [1:1268027] 1 2 1 1 2 1 2 1 2 1 ...

``` r
tennis_test <- tennis_test %>%
  mutate(set1 = ifelse(match_id == "20220703-M-Wimbledon-R16-Jannik_Sinner-Carlos_Alcaraz" & set1 == 3, 2, set1)) %>%
  mutate(set2 = ifelse(match_id == "20220703-M-Wimbledon-R16-Jannik_Sinner-Carlos_Alcaraz" & point_counter >  185, 1, set2)) %>%
  mutate(pt1 = ifelse(match_id == "20220703-M-Wimbledon-R16-Jannik_Sinner-Carlos_Alcaraz" & point_counter ==  185, 5, pt1)) %>%
  mutate(pt2 = ifelse(match_id == "20220703-M-Wimbledon-R16-Jannik_Sinner-Carlos_Alcaraz" & point_counter ==  185, 6, pt2))
```

Now, we try to create 6 variables: point difference, total points, game
difference, total games, set difference, and total sets. We also do some
point manipulation that we explained in our paper (converting them into
sequential numbers instead of 15, 30,40, etc). We do that here. We
create a new dataframe to store this information.

``` r
tennis_test <- tennis_test %>%
  mutate(set_diff = ifelse(player_number == 1, set1-set2, set2-set1)) %>%
  mutate(gm_diff = ifelse(player_number == 1, gm1-gm2, gm2-gm1)) %>%
  mutate(point1 = ifelse(pt1 == "15", 1, ifelse(pt1 == "30", 2, ifelse(pt1 == "40", 3, ifelse(pt1 == "AD", 4, pt1))))) %>%
  mutate(point2 = ifelse(pt2 == "15", 1, ifelse(pt2 == "30", 2, ifelse(pt2 == "40", 3, ifelse(pt2 == "AD", 4, pt2))))) %>%
  mutate(point1 = ifelse(gm1 == 6 & gm2 == 6 & tournament != "Roland_Garros" & year != 2020 & year != 2021, pt1, point1)) %>%
  mutate(point2 = ifelse(gm1 == 6 & gm2 == 6 & tournament != "Roland_Garros" & year != 2020 & year != 2021, pt2, point2))

tennis_test$point1 <- as.numeric(tennis_test$point1)
tennis_test$point2 <- as.numeric(tennis_test$point2)

tennis_test <- tennis_test %>%
  mutate(pt_diff = ifelse(player_number == 1, point1-point2, point2-point1)) %>%
  mutate(total_pts = point1+point2) %>%
  mutate(total_gms = gm1+gm2) %>%
  mutate(total_sets = set1+set2)




final_tennis_data <- select(tennis_test, match_id, year, player_1, player_2, player_number, shot_num, point_counter, game_counter, hitting_player, opposing_player, pt_winner, gm_winner, set_winner, match_winner, shots_left_player, is_hitter_winner, pt_diff, gm_diff, set_diff, total_pts, total_gms, total_sets, hitter_rank, opposition_rank, agg_rating, reward)
```

Now we can categorize the situation in terms of points, games, and sets.
This will give us 3 variables. We then categorize the hitter/opposition
rankings. This will give us 2 more variables. In total, we will have 5
variables for the model. We then convert these variables into factors
and then separate the entire dataset into serves, returns, and rallies.

``` r
final_tennis_data <- final_tennis_data %>%
  mutate(points = ifelse(pt_diff == 1 & total_pts < 4, 1, 0)) %>% # Winning close no clinch
  mutate(points = ifelse(pt_diff == -1 & total_pts < 4, 2, points)) %>% # Losing close no clinch
  mutate(points = ifelse(pt_diff == 0, 3, points)) %>% # tied
  mutate(points = ifelse(pt_diff == 1 & total_pts > 4, 4, points)) %>% #Winning close clinch
  mutate(points = ifelse(pt_diff == -1 & total_pts > 4, 5, points)) %>% # Losing close clinch
  mutate(points = ifelse(pt_diff > 1, 6, points)) %>% # Winning not close
  mutate(points = ifelse(pt_diff < -1, 7, points)) %>% # Losing not close
  mutate(games = ifelse((gm_diff) == 1 & total_gms > 8, 1, 0)) %>% # Close + High pressure + winning
  mutate(games = ifelse((gm_diff) == -1 & total_gms > 8, 2, games)) %>% # Close + High pressure + losing
  mutate(games = ifelse(abs(gm_diff) == 0 & total_gms > 8, 3, games)) %>% # Close + High pressure + tied
  mutate(games = ifelse(abs(gm_diff) < 1.1 & total_gms < 9, 4, games)) %>% # Close + Not high pressure
  mutate(games = ifelse(abs(gm_diff) == 2, 5, games)) %>% # Game has been broken
  mutate(games = ifelse(abs(gm_diff) > 2 , 6, games)) #Blowout

final_tennis_data <- final_tennis_data %>%
  mutate(set = ifelse((set_diff) == 1, 1, 0 )) %>% # Close up 1
  mutate(set = ifelse((set_diff) == -1, 2, set )) %>% # Close down 1
  mutate(set = ifelse((set_diff) == 0, 3, set )) %>% # tied
  mutate(set = ifelse(abs(set_diff) > 1, 4, set )) # Not close

final_tennis_data$hitter_rank <- as.numeric(final_tennis_data$hitter_rank)
final_tennis_data$opposition_rank <- as.numeric(final_tennis_data$opposition_rank)
final_tennis_data <- final_tennis_data %>%
  mutate(hitter_ranking = ifelse(hitter_rank > 0 & hitter_rank < 11, 1, 0)) %>% #top 10
  mutate(hitter_ranking = ifelse(hitter_rank > 10 & hitter_rank < 33, 2, hitter_ranking)) %>% #11-32
  mutate(hitter_ranking = ifelse(hitter_rank > 32 & hitter_rank < 105, 3, hitter_ranking)) %>% #33-104
  mutate(hitter_ranking = ifelse(hitter_rank > 104 & hitter_rank < 151, 4, hitter_ranking)) %>% #105-150
  mutate(hitter_ranking = ifelse(hitter_rank == 151, 5, hitter_ranking))#unranked

final_tennis_data <- final_tennis_data %>%
  mutate(opposition_ranking = ifelse(opposition_rank > 0 & opposition_rank < 11, 1, 0)) %>%
  mutate(opposition_ranking = ifelse(opposition_rank > 10 & opposition_rank < 33, 2, opposition_ranking)) %>%
  mutate(opposition_ranking = ifelse(opposition_rank > 32 & opposition_rank < 105, 3, opposition_ranking)) %>%
  mutate(opposition_ranking = ifelse(opposition_rank > 104 & opposition_rank < 151, 4, opposition_ranking)) %>%
  mutate(opposition_ranking = ifelse(opposition_rank == 151, 5, opposition_ranking))

final_tennis_data <- final_tennis_data %>%
  arrange(match_id, point_counter)

final_tennis_data <- final_tennis_data %>%
  mutate(hitter_rank = ifelse(hitter_rank == "unranked", 151, hitter_rank)) %>%
  mutate(opposition_rank = ifelse(opposition_rank == "unranked", 151, opposition_rank))

final_tennis_data$points <- as.factor(final_tennis_data$points)
final_tennis_data$games <- as.factor(final_tennis_data$games)
final_tennis_data$set <- as.factor(final_tennis_data$set)
final_tennis_data$hitter_ranking <- as.factor(final_tennis_data$hitter_ranking)
final_tennis_data$opposition_ranking <- as.factor(final_tennis_data$opposition_ranking)

serves_only <- filter(final_tennis_data, shot_num == 1)
returns_only <- filter(final_tennis_data, shot_num == 2)
rallies_only <- filter(final_tennis_data, shot_num > 2)
```

We are now ready to being the model!

## Creating and training the model

Since we have 3 datasets, we will be creating 3 models. Let’s begin with
serves

First we need to set up our model. We select the variables we want. The
label/outcome variable is the reward and the predictor variables are the
5 other variables. We ensure that these 5 other variables are factored
and turn each bin into their separate variable using the “caret”
package. Our training dataset is all data from 2020-2022 and our testing
dataset is data from 2023. We then convert our two datasets to matrices
to be palatable to the model.

``` r
selected_serves <- select(serves_only, year, label = reward, points, games, set, hitter_ranking, opposition_ranking, agg = agg_rating)

selected_serves$points <- as.factor(selected_serves$points)
selected_serves$games <- as.factor(selected_serves$games)
selected_serves$set <- as.factor(selected_serves$set)
selected_serves$hitter_ranking <- as.factor(selected_serves$hitter_ranking)
selected_serves$opposition_ranking <- as.factor(selected_serves$opposition_ranking)
selected_serves$year <- as.numeric(selected_serves$year)
dmy <- dummyVars(" ~ .", data = selected_serves)
serves_xgb <- data.frame(predict(dmy, newdata = selected_serves))

serves_train <- filter(serves_xgb, year < 2023)
train_s <- serves_train[2:30]
serves_test <- filter(serves_xgb, year == 2023)
test_s <- serves_test[2:30]
train_s <- as.matrix(train_s)
test_s <- as.matrix(test_s)
```

And now we run our model! The XGBoost parameters are specified in the
code. We have also included code to see the root mean-squared error and
the important factors. We also bind our results with the original data.

``` r
serves_xgb_model <-
  xgboost(
    data = train_s[, 2:29],
    label = train_s[, 1],
    nrounds = 250,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 6,
    eta = .18
  )   

pred_xgb <- predict(serves_xgb_model, test_s[, 2:29])

# Seeing our RMSE
yhat <- pred_xgb
y <- test_s[, 1]
postResample(yhat, y)
xgb.importance(model = serves_xgb_model)

#Bind the expected reward data with the existing dataset
expected_serves <- as.data.frame(pred_xgb)
serves_test_context <- filter(serves_only, year == 2023)
final_data_serves <- cbind(serves_test_context, expected_serves)
final_data_serves$diff <- final_data_serves$reward - final_data_serves$pred_xgb
```

And now we repeat the same exact thing for the return and rally model.

``` r
#Returns next
selected_returns <- select(returns_only, year, label = reward, points, games, set, hitter_ranking, opposition_ranking, agg = agg_rating)

selected_returns$points <- as.factor(selected_returns$points)
selected_returns$games <- as.factor(selected_returns$games)
selected_returns$set <- as.factor(selected_returns$set)
selected_returns$hitter_ranking <- as.factor(selected_returns$hitter_ranking)
selected_returns$opposition_ranking <- as.factor(selected_returns$opposition_ranking)
selected_returns$year <- as.numeric(selected_returns$year)
dmy <- dummyVars(" ~ .", data = selected_returns)
returns_xgb <- data.frame(predict(dmy, newdata = selected_returns))

returns_train <- filter(returns_xgb, year < 2023)
train_re <- returns_train[2:30]
returns_test <- filter(returns_xgb, year == 2023)
test_re <- returns_test[2:30]
train_re <- as.matrix(train_re)
test_re <- as.matrix(test_re)

returns_xgb_model <-
  xgboost(
    data = train_re[, 2:29],
    label = train_re[, 1],
    nrounds = 250,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 6,
    eta = .18
  )   

pred_xgb <- predict(returns_xgb_model, test_re[, 2:29])

# Seeing our RMSE
yhat <- pred_xgb
y <- test_re[, 1]
postResample(yhat, y)
xgb.importance(model = returns_xgb_model)

expected_returns <- as.data.frame(pred_xgb)
returns_test_context <- filter(returns_only, year == 2023)

final_data_returns <- cbind(returns_test_context, expected_returns)
final_data_returns$diff <- final_data_returns$reward - final_data_returns$pred_xgb


#Rallies next
selected_rallies <- select(rallies_only, year, label = reward, points, games, set, hitter_ranking, opposition_ranking, agg = agg_rating)

selected_rallies$points <- as.factor(selected_rallies$points)
selected_rallies$games <- as.factor(selected_rallies$games)
selected_rallies$set <- as.factor(selected_rallies$set)
selected_rallies$hitter_ranking <- as.factor(selected_rallies$hitter_ranking)
selected_rallies$opposition_ranking <- as.factor(selected_rallies$opposition_ranking)
selected_rallies$year <- as.numeric(selected_rallies$year)
dmy <- dummyVars(" ~ .", data = selected_rallies)
rallies_xgb <- data.frame(predict(dmy, newdata = selected_rallies))

rallies_train <- filter(rallies_xgb, year < 2023)
train_ra <- rallies_train[2:30]
rallies_test <- filter(rallies_xgb, year == 2023)
test_ra <- rallies_test[2:30]
train_ra <- as.matrix(train_ra)
test_ra <- as.matrix(test_ra)

rallies_xgb_model <-
  xgboost(
    data = train_ra[, 2:29],
    label = train_ra[, 1],
    nrounds = 250,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 6,
    eta = .18
  )   

pred_xgb <- predict(rallies_xgb_model, test_ra[, 2:29])

# Seeing our RMSE
yhat <- pred_xgb
y <- test_ra[, 1]
postResample(yhat, y)
xgb.importance(model = rallies_xgb_model)

expected_rallies <- as.data.frame(pred_xgb)
rallies_test_context <- filter(rallies_only, year == 2023)

final_data_rallies <- cbind(rallies_test_context, expected_rallies)
final_data_rallies$diff <- final_data_rallies$reward - final_data_rallies$pred_xgb
```

## Testing the models 

We import the dataset that has all of the possible
environment-action pairs (the output for script 5). We will eventually be inputting this into our
model. We convert the factor variables into dummy variables and then
input it separately into each model.

The later half of this code details how we determined the maximum reward
for each environment and took the associated aggressiveness level. We
then name these variables as “optimal_agg” which is the aggressiveness
level associated with the highest reward and “optimal_reward” which is
the reward associated with the aggressiveness level. We then join this
dataset with our test dataset for serves, returns, and rallies, ensuring
that the environment matches. Now every single entry in all three of our
models have an optimal aggressive and optimal reward and we have
completed our tasks.

``` r
# Now we apply the model to all the state action pairs
sa_pairs <- read.csv("state_action_pairs.csv")
sa_pairs$points <- as.factor(sa_pairs$points)
sa_pairs$games <- as.factor(sa_pairs$games)
sa_pairs$set <- as.factor(sa_pairs$set)
sa_pairs$hitter_ranking <- as.factor(sa_pairs$hitter_ranking)
sa_pairs$opposition_ranking <- as.factor(sa_pairs$opposition_ranking)
dmy <- dummyVars(" ~ .", data = sa_pairs)
sa_pairs_xgb <- data.frame(predict(dmy, newdata = sa_pairs))
sa_pairs_xgb <- as.matrix(sa_pairs_xgb)

#And now we can predict
best_serve <- predict(serves_xgb_model, sa_pairs_xgb)
best_serve <- as.data.frame(best_serve)
best_return <- predict(returns_xgb_model, sa_pairs_xgb)
best_return <- as.data.frame(best_return)
best_rally <- predict(rallies_xgb_model, sa_pairs_xgb)
best_rally <- as.data.frame(best_rally)

sa_pairs <- as.data.frame(sa_pairs)
sa_pairs_serve <- cbind(sa_pairs, best_serve)
sa_pairs_return <- cbind(sa_pairs, best_return)
sa_pairs_rally <- cbind(sa_pairs, best_rally)

sa_best_serve <- sa_pairs_serve %>%
  group_by(points, games, set, hitter_ranking, opposition_ranking) %>%
  filter(best_serve == max(best_serve)) %>%  # Filter rows where best_serve is at maximum within each group
  ungroup()

sa_best_return <- sa_pairs_return %>%
  group_by(points, games, set, hitter_ranking, opposition_ranking) %>%
  filter(best_return == max(best_return)) %>%  # Filter rows where best_serve is at maximum within each group
  ungroup()

sa_best_rally <- sa_pairs_rally %>%
  group_by(points, games, set, hitter_ranking, opposition_ranking) %>%
  filter(best_rally == max(best_rally)) %>%  # Filter rows where best_serve is at maximum within each group
  ungroup()

sa_best_serve <- sa_best_serve %>%
  group_by(points, games, set, hitter_ranking, opposition_ranking) %>%
  filter(agg == min(agg)) %>% 
  rename(optimal_reward = best_serve) %>%
  rename(optimal_agg = agg) %>%
  ungroup()
sa_best_return <- sa_best_return %>%
  group_by(points, games, set, hitter_ranking, opposition_ranking) %>%
  filter(agg == min(agg)) %>%  # Filter rows where best_serve is at maximum within each group
  rename(optimal_reward = best_return) %>%
  rename(optimal_agg = agg) %>%
  ungroup()
sa_best_rally <- sa_best_rally %>%
  group_by(points, games, set, hitter_ranking, opposition_ranking) %>%
  filter(agg == min(agg)) %>%  # Filter rows where best_serve is at maximum within each group
  rename(optimal_reward = best_rally) %>%
  rename(optimal_agg = agg) %>%
  ungroup()

final_data_serves <- left_join(final_data_serves, sa_best_serve, by = c("points", "games", "set", "hitter_ranking", "opposition_ranking"))
final_data_returns <- left_join(final_data_returns, sa_best_return, by = c("points", "games", "set", "hitter_ranking", "opposition_ranking"))
final_data_rallies <- left_join(final_data_rallies, sa_best_rally, by = c("points", "games", "set", "hitter_ranking", "opposition_ranking"))
```

With that, we have completed the coding portion of our project! Please
refer to our paper for the visualizations
