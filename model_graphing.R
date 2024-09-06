library(xgboost)
library(caret)
library(ggplot2)
library(ggtext)
serves_only <- read.csv("serves_only.csv")
returns_only <- read.csv("returns_only.csv")
rallies_only <- read.csv("rallies_only.csv")
#XGBoost----
#serves first
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
train <- serves_train[2:30]
serves_test <- filter(serves_xgb, year == 2023)
test <- serves_test[2:30]
train <- as.matrix(train)
test <- as.matrix(test)
str(train)

serves_xgb_model <-
  xgboost(
    data = train[, 2:29],
    label = train[, 1],
    nrounds = 250,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 6,
    eta = .18
  )   

vip(serves_xgb_model)
pred_xgb <- predict(serves_xgb_model, test[, 2:29])

# Seeing our RMSE
yhat <- pred_xgb
y <- test[, 1]
postResample(yhat, y)
xgb.importance(model = serves_xgb_model)

expected_serves <- as.data.frame(pred_xgb)
serves_test_context <- filter(serves_only, year == 2023)
final_data_serves <- cbind(serves_test_context, expected_serves)
final_data_serves$diff <- final_data_serves$reward - final_data_serves$pred_xgb

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
train <- returns_train[2:30]
returns_test <- filter(returns_xgb, year == 2023)
test <- returns_test[2:30]
train <- as.matrix(train)
test <- as.matrix(test)
str(train)

returns_xgb_model <-
  xgboost(
    data = train[, 2:29],
    label = train[, 1],
    nrounds = 250,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 6,
    eta = .18
  )   

vip(returns_xgb_model)
pred_xgb <- predict(returns_xgb_model, test[, 2:29])

# Seeing our RMSE
yhat <- pred_xgb
y <- test[, 1]
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
train <- rallies_train[2:30]
rallies_test <- filter(rallies_xgb, year == 2023)
test <- rallies_test[2:30]
train <- as.matrix(train)
test <- as.matrix(test)
str(train)
rallies_xgb_model <-
  xgboost(
    data = train[, 2:29],
    label = train[, 1],
    nrounds = 250,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 6,
    eta = .18
  )   

vip(rallies_xgb_model)
pred_xgb <- predict(rallies_xgb_model, test[, 2:29])

# Seeing our RMSE
yhat <- pred_xgb
y <- test[, 1]
postResample(yhat, y)
xgb.importance(model = rallies_xgb_model)

expected_rallies <- as.data.frame(pred_xgb)
rallies_test_context <- filter(rallies_only, year == 2023)

final_data_rallies <- cbind(rallies_test_context, expected_rallies)
final_data_rallies$diff <- final_data_rallies$reward - final_data_rallies$pred_xgb

#Tesing the model----
# Now we apply the model to all the state action pairs - This will serve as the test
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



write.csv(final_data_tennis, "finalized_tennis_dataset.csv")
write.csv(final_data_serves, "finalized_tennis_dataset_serves.csv")
write.csv(final_data_returns, "finalized_tennis_dataset_returns.csv")
write.csv(final_data_rallies, "finalized_tennis_dataset_rallies.csv")

#Graphing the data----
serves_importance <- read.csv("tennis_serves.csv") #Just a manually inputed csv of the vip/important factors - Did this for all 3 models
serves_importance$variable <- reorder(serves_importance$variable, serves_importance$importance)

ggplot(serves_importance, aes(x = variable, y = importance, fill = variable)) +
  geom_bar(stat = "identity", color = "gray20") + 
  coord_flip() +
  labs(
    title = "<span style = 'font-size:28pt;'>**Important factors for the xR model - *serves*** </span><br> 
             <span style = 'font-size:20pt;'>  Visualizing the top eight of the 27 factors in the *Expected Reward* model </span>", 
    y = "Importance", 
    x = ""
  ) +
  scale_fill_manual(values = c("#4C6A92", "#8AB969", "#E38A8A", "#887094", "#B49149", "#66b2b2", "#dfc471", "#bea4cb"))+
  theme_minimal() + 
  theme(
    plot.title = element_markdown(family = "serif", size = 18, lineheight = 1.2, margin = margin(t = 10, b = 10)), # Centered title with adjusted margin
    axis.title.y = element_text(size = 20),  
    axis.text.y = element_text(size = 17),
    axis.text = element_text(family = "serif", color = "black", size = 13), 
    axis.title.x = element_text(size = 20, family = "serif"),
    legend.position = "none",    panel.background = element_rect(fill = "gray85", color = NA),  # Light blue-gray background
    plot.background = element_rect(fill = "white", color = NA)
  )

returns_importance <- read.csv("tennis_returns.csv")
returns_importance$variable <- reorder(returns_importance$variable, returns_importance$importance)
ggplot(returns_importance, aes(x = variable, y = importance, fill = variable)) +
  geom_bar(stat = "identity", color = "gray20") + 
  coord_flip() +
  labs(
    title = "<span style = 'font-size:28pt;'>**Important factors for the xR model - *Returns*** </span><br> 
             <span style = 'font-size:20pt;'>  Visualizing the top eight of the 27 factors in the *Expected Reward* model </span>", 
    y = "Importance", 
    x = ""
  ) +
  scale_fill_manual(values = c("#5F6E7F", "#8AB969", "#66b2b2", "#D18E54", "#66b2b2", "#E38A8A", "#B49149", "#bea4cb"))+
  theme_minimal() + 
  theme(
    plot.title = element_markdown(family = "serif", size = 18, lineheight = 1.2, margin = margin(t = 10, b = 10)), # Centered title with adjusted margin
    axis.title.y = element_text(size = 20),  
    axis.text.y = element_text(size = 17),
    axis.text = element_text(family = "serif", color = "black", size = 13), 
    axis.title.x = element_text(size = 20, family = "serif"),
    legend.position = "none",    panel.background = element_rect(fill = "gray85", color = NA),  # Light blue-gray background
    plot.background = element_rect(fill = "white", color = NA)
  )

rallies_importance <- read.csv("tennis_rallies.csv")
rallies_importance$variable <- reorder(rallies_importance$variable, rallies_importance$importance)
ggplot(rallies_importance, aes(x = variable, y = importance, fill = variable)) +
  geom_bar(stat = "identity", color = "gray20") + 
  coord_flip() +
  labs(
    title = "<span style = 'font-size:28pt;'>**Important factors for the xR model - *Rallies*** </span><br> 
             <span style = 'font-size:20pt;'>  Visualizing the top eight of the 27 factors in the *Expected Reward* model </span>", 
    y = "Importance", 
    x = ""
  ) +
  scale_fill_manual(values = c("#C48CA3", "#A37A74", "#6CA77D", "#E38A8A", "#7F8C5A", "#887094", "#B49149", "#bea4cb"))+
  theme_minimal() + 
  theme(
    plot.title = element_markdown(family = "serif", size = 18, lineheight = 1.2, margin = margin(t = 10, b = 10)), # Centered title with adjusted margin
    axis.title.y = element_text(size = 20),  
    axis.text.y = element_text(size = 17),
    axis.text = element_text(family = "serif", color = "black", size = 13), 
    axis.title.x = element_text(size = 20, family = "serif"),
    legend.position = "none",    panel.background = element_rect(fill = "gray85", color = NA),  # Light blue-gray background
    plot.background = element_rect(fill = "white", color = NA)
  )

#Here is the code for how we created our score differential and rank differential variables

final_data_serves <- final_data_serves %>%
  mutate(points_value = pt_diff) %>%
  mutate(games_value = 4*gm_diff) %>%
  mutate(set_value = 24*set_diff)

final_data_serves$differential <- final_data_serves$points_value + final_data_serves$games_value + final_data_serves$set_value

final_data_returns <- final_data_returns %>%
  mutate(points_value = pt_diff) %>%
  mutate(games_value = 4*gm_diff) %>%
  mutate(set_value = 24*set_diff)

final_data_returns$differential <- final_data_returns$points_value + final_data_returns$games_value + final_data_returns$set_value

final_data_rallies <- final_data_rallies %>%
  mutate(points_value = pt_diff) %>%
  mutate(games_value = 4*gm_diff) %>%
  mutate(set_value = 24*set_diff)

final_data_rallies$differential <- final_data_rallies$points_value + final_data_rallies$games_value + final_data_rallies$set_value

#Binding the data from all three models together
final_data_tennis <- bind_rows(final_data_serves, final_data_returns, final_data_rallies)
final_data_tennis <- final_data_tennis %>%
  arrange(match_id, game_counter, point_counter, shot_num)

# Create the rank_difference variable
final_data_tennis <- final_data_tennis %>%
  mutate(rank_difference = as.numeric(opposition_ranking ) - as.numeric(hitter_ranking)) # Adjust column names accordingly

#Graphing the results
ggplot(final_data_tennis, aes(x = differential, y = optimal_agg, fill = optimal_reward)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "#aa0000") + # Customize the color scale
  labs(
    title = "<span style = 'font-size:28pt;'>**Heatmap of reward by *score* differential and aggressiveness** </span> <br>
    <span style = 'font-size:20pt;'> Visualizing the expected reward of every optimal aggressiveness for each score differential in the dataset</span>",
    x = "Score differential",
    y = "Aggressiveness",
    fill = "Reward"
  ) +
  theme(
    plot.title = element_textbox_simple(family = "serif", size = 18, lineheight = 1, hjust = 0.5, padding = margin(0, 0, 5, 0)),
    axis.title.y = element_text(size = 20, family = "serif"),  
    axis.text = element_text(family = "serif", color = "black", size = 13), 
    axis.title.x = element_text(size = 20, family = "serif"),
    panel.background = element_rect(fill = "white", color = NA),  # Light blue-gray background
    plot.background = element_rect(fill = "white", color = NA),
    legend.title = element_text(size = 18, family = "serif"),
    legend.text = element_text(size = 15, family = "serif"),
    legend.key.size = unit(1.75, "cm"), # Increase the size of the legend key
    
  )

ggplot(final_data_tennis, aes(x = rank_difference, y = optimal_agg, fill = optimal_reward)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkgreen") + # Customize the color scale
  labs(
    title = "<span style = 'font-size:28pt;'>**Heatmap of Reward by *rank* differential and Aggressiveness** </span> <br>
    <span style = 'font-size:20pt;'> Visualizing the expected reward of every optimal aggressiveness for each rank differential in the dataset</span>",
    x = "Rank differential",
    y = "Aggressiveness",
    fill = "Reward"
  ) +
  scale_x_continuous(breaks = seq(min(final_data_tennis$rank_difference), max(final_data_tennis$rank_difference), by = 1)) + # Set x-axis labels every 1 unit
  theme_minimal() +
  theme(
    plot.title = element_textbox_simple(family = "serif", size = 18, lineheight = 1, hjust = 0.5, padding = margin(0, 0, 5, 0)),
    axis.title.y = element_text(size = 20, family = "serif"),  
    axis.text = element_text(family = "serif", color = "black", size = 13), 
    axis.title.x = element_text(size = 20, family = "serif"),
    panel.background = element_rect(fill = "white", color = NA),  # Light blue-gray background
    plot.background = element_rect(fill = "white", color = NA),
    legend.title = element_text(size = 18, family = "serif"),
    legend.text = element_text(size = 15, family = "serif"),
    legend.key.size = unit(1.75, "cm"), # Increase the size of the legend key
    
  )





ggplot(final_data_tennis, aes(x = optimal_agg)) +
  geom_bar(aes(fill = (optimal_agg > 5)), color = "gray20", alpha = 0.5) +
  scale_fill_manual(values = c("#a94642", "#136c7f")) +
  scale_x_continuous(breaks = seq(min(final_data_tennis$optimal_agg), max(final_data_tennis$optimal_agg), by = 1)) + # Set x-axis labels every 1 unit
  labs(
    title = "<span style = 'font-size:28pt;'>**How often are different levels of aggressiveness recommended by the model?**</span> <br>
                <span style = 'font-size:20pt;'>  Visualizing the optimal aggressiveness for every shot in the dataset, noting if it was <span style='color:#a94642;'>low aggressiveness</span>, or <span style='color:#136c7f;'>high aggressiveness</span>. </span>",
    x = "Aggressiveness level",
    y = "Number of observations"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_markdown(family = "serif", size = 18, lineheight = 1.2, margin = margin(t = 10, b = 10)), # Centered title with adjusted margin
    axis.title.y = element_text(size = 20, family = "serif"),  
    axis.text.y = element_text(size = 17),
    axis.text = element_text(family = "serif", color = "black", size = 13), 
    axis.title.x = element_text(size = 20, family = "serif"),
    legend.position = "none",    panel.background = element_rect(fill = "gray85", color = NA),  # Light blue-gray background
    plot.background = element_rect(fill = "white", color = NA)
  )