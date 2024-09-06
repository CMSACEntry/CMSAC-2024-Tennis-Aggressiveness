library(ggtext) #graphing
library(ggplot2) #graphing
library(tidyverse)
library(stringr) #webscraping
library(purrr) #functions
library(rvest) #webscraping
library(xgboost) #model
library(caret) #model

#Setting up the data----
tennisdata <- read.csv("preprocessed_aggressiveness_mensince2020.csv")

tennisdata$depth[is.na(tennisdata$depth)] <- "missing"
tennisdata$serve_zone[is.na(tennisdata$serve_zone)] <- "missing"
tennisdata$shot_type[is.na(tennisdata$shot_type)] <- "missing"
tennisdata$outcome[is.na(tennisdata$outcome)] <- "missing"
tennisdata$direction[is.na(tennisdata$direction)] <- "missing"
tennisdata$next_direction <- lead(tennisdata$direction)

tennisdata$year <- gsub("^(.{4}).*", "\\1", tennisdata$match_id)
tennisdata$tournament <- gsub("^[^-]*-[^-]*-([^-]*)-.*$", "\\1", tennisdata$match_id)

#Creating the aggressiveness metric----
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
table(tennisdata$agg_rating)

tennisdata <- tennisdata %>%
  mutate(opposing_player = ifelse(Hitting_Player == Player_1, Player_2, Player_1))

player_agg <- tennisdata %>%
  group_by(Hitting_Player) %>%
  summarize(agg = mean(agg_rating))

tennisdata1 <- tennisdata %>%
  mutate(Hitting_Player = gsub("(\\w)\\s+$", "\\1", Hitting_Player, perl = TRUE)) %>%
  mutate(opposing_player = gsub("(\\w)\\s+$", "\\1", opposing_player, perl = TRUE)) %>%
  mutate(Player_1 = gsub("(\\w)\\s+$", "\\1", Player_1, perl = TRUE)) %>%
  mutate(Player_2 = gsub("(\\w)\\s+$", "\\1", Player_2, perl = TRUE)) %>%
  mutate(PtWinner_Player = gsub("(\\w)\\s+$", "\\1", PtWinner_Player, perl = TRUE)) %>%
  mutate(GmWinner_Player = gsub("(\\w)\\s+$", "\\1", GmWinner_Player, perl = TRUE)) %>%
  mutate(SetWinner_Player = gsub("(\\w)\\s+$", "\\1", SetWinner_Player, perl = TRUE)) %>%
  mutate(MatchWinner_Player = gsub("(\\w)\\s+$", "\\1", MatchWinner_Player, perl = TRUE))

tennisdata1 <- tennisdata1 %>%
  group_by(Pt) %>%
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

tennisdata1 <- tennisdata1 %>%
  mutate(agg_rating = ifelse(is_fault == 1 & error_type == "d" & serve_zone == 6 | is_fault == 1 & error_type == "w" & serve_zone == 6 | is_fault == 1 & error_type == "x" & serve_zone == 6, 10, ifelse(is_fault == 1 & serve_zone == 5, 2, ifelse(is_fault == 1 & serve_zone == 4 | is_fault == 1 & serve_zone == 6 & error_type != "x" & error_type != "w" & error_type != "d" | is_fault == 1 & serve_zone == "missing", 5, agg_rating))))

#Player rankings----
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

# View the combined dataframe
colnames(combined_df)
rankings <- select(combined_df, rank = RK, name = NAME, year = Year)

atp_rankings <- select(combined_df, rank = RK, name = NAME, points = POINTS, year = Year)

tennisdata1 <- left_join(tennisdata1, rankings, by = c("Hitting_Player" = "name", "year"))
tennisdata1 <- left_join(tennisdata1, rankings, by = c("opposing_player" = "name", "year"))

colnames(tennisdata1)[colnames(tennisdata1) == "rank.x"] <- "hitter_rank"
colnames(tennisdata1)[colnames(tennisdata1) == "rank.y"] <- "oposition_rank"


#Some more data wrangling----
tennisdata1 <- tennisdata1 %>%
  mutate(game_counter = str_extract(Gm., "^[^\\(]+")) %>%
  mutate(game_counter = as.numeric(game_counter))

final_tennis <- select(tennisdata1, match_id, year, shot_num, tournament, player_1 = Player_1, player_2 = Player_2, point_counter = Pt, game_counter, set1 = Set1, set2 = Set2, gm1 = Gm1, gm2 = Gm2, pt1 = Player_1_Pts, pt2 = Player_2_Pts, hitting_player = Hitting_Player, opposing_player, pt_winner = PtWinner_Player, gm_winner = GmWinner_Player, set_winner = SetWinner_Player, match_winner = MatchWinner_Player, hitter_rank, opposition_rank = oposition_rank, outcome_shottype_aggressiveness, is_fault, direction_aggressiveness, approach_aggressiveness, serve_aggressiveness, outcome_aggressiveness, depth_aggressiveness, agg_rating, shots_left)
final_tennis <- final_tennis %>%
  filter(match_id != "20200103-M-ATP_Cup-RR-Alex_De_Minaur-Alexander_Zverev")
final_tennis$hitter_rank[is.na(final_tennis$hitter_rank)] <- "151"
final_tennis$opposition_rank[is.na(final_tennis$opposition_rank)] <- "151"
table(final_tennis$tournament)

player_rankings <- final_tennis %>%
  group_by(hitting_player) %>%
  summarize(shots_played = n(), agg = mean(agg_rating) )
final_tennis$is_hitter_winner <- ifelse(final_tennis$hitting_player == final_tennis$pt_winner, 1, 0)

write.csv(player_rankings, "player_agg.csv") #For script 3

#Reward metric----
final_tennis <- final_tennis %>%
  group_by(point_counter, hitting_player, match_id) %>%
  mutate(shots_left_player = n() - row_number()) %>%
  ungroup()

final_tennis <- final_tennis %>%
  mutate(is_double_fault = ifelse(lag(is_fault) == 1 & is_fault == 1, 1, 0))
final_tennis <- final_tennis %>%
  mutate(reward = ifelse(is_hitter_winner == 1, 10*0.5^shots_left_player, -5*0.5^shots_left_player)) %>%
  mutate(reward = ifelse(is_fault == 1 & is_double_fault == 0, 0, ifelse(is_fault == 1 & is_double_fault == 1, -5, reward)))

#Binning the variables----
tennis_test <- final_tennis %>%
  mutate(player_number = ifelse(hitting_player == player_1, 1, 2))
str(tennis_test)

tennis_test <- tennis_test %>%
  mutate(set1 = ifelse(match_id == "20220703-M-Wimbledon-R16-Jannik_Sinner-Carlos_Alcaraz" & set1 == 3, 2, set1)) %>%
  mutate(set2 = ifelse(match_id == "20220703-M-Wimbledon-R16-Jannik_Sinner-Carlos_Alcaraz" & point_counter >  185, 1, set2)) %>%
  mutate(pt1 = ifelse(match_id == "20220703-M-Wimbledon-R16-Jannik_Sinner-Carlos_Alcaraz" & point_counter ==  185, 5, pt1)) %>%
  mutate(pt2 = ifelse(match_id == "20220703-M-Wimbledon-R16-Jannik_Sinner-Carlos_Alcaraz" & point_counter ==  185, 6, pt2))

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
  table(r$opposition_rank)

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

write.csv(serves_only. "serves_only.csv") #For script 4
write.csv(returns_only "returns_only.csv") #For script 4
write.csv(rallies_only "rallies_only.csv") #For script 4

write.csv(final_tennis_data, "final_tennis_data.csv")


