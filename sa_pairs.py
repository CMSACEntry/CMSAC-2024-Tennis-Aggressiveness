import pandas as pd
import itertools

# Define the range for each column
points = range(1, 8)  # 1 to 7 inclusive
games = range(1, 7)   # 1 to 6 inclusive
sets = range(1, 5)    # 1 to 4 inclusive
rankings = range(1, 6)  # 1 to 5 inclusive for both hitter_ranking and opposition_ranking
agg = range(0, 11)
# Generate all combinations using itertools.product
combinations = list(itertools.product(points, games, sets, rankings, rankings, agg))

# Create a DataFrame from the combinations
df_combinations = pd.DataFrame(combinations, columns=['points', 'games', 'set', 'hitter_ranking', 'opposition_ranking', 'agg'])

df_combinations.to_csv('state_action_pairs.csv', index=False)  # Display the first few rows of the dataframe
