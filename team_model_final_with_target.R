# Preparing Team-Level Dataset for Net Rating Modeling
# 1. Load Required Libraries
library(tidyverse)

# 2. Compute Team Cluster Composition from Players
df <- read_csv("path/Merged_Player_Team_Data.csv")

# Remove entries with missing clustering or minutes played
df <- df %>% filter(!is.na(Group_Cluster), !is.na(MP))

# Aggregate: Percentage of minutes from each cluster per team-season
team_agg <- df %>%
  group_by(Season, Team) %>%
  summarise(
    Total_MP = sum(MP, na.rm = TRUE),
    Cluster_0_Pct = sum(MP[Group_Cluster == 0], na.rm = TRUE) / sum(MP),
    Cluster_1_Pct = sum(MP[Group_Cluster == 1], na.rm = TRUE) / sum(MP),
    Cluster_2_Pct = sum(MP[Group_Cluster == 2], na.rm = TRUE) / sum(MP),
    Cluster_3_Pct = sum(MP[Group_Cluster == 3], na.rm = TRUE) / sum(MP),
    Cluster_4_Pct = sum(MP[Group_Cluster == 4], na.rm = TRUE) / sum(MP)
  ) %>%
  ungroup()

# Save team cluster composition
write_csv(team_agg, "Team_Cluster_Composition.csv")

# 3. Compute Team-Level Weighted Player Profiles
merged_data <- read_csv("path/Merged_Player_Team_Data.csv")

# Clean problematic column names
names(merged_data) <- names(merged_data) %>%
  str_replace_all("%", ".") %>%
  str_replace_all("\\.", "_")

# Select variables for weighting
vars_to_weight <- c(
  "OBPM", "DBPM", "VORP", "PER", "TS_", "USG_", 
  "AST_", "TOV_", "STL_", "BLK_", "avg_pts", "avg_ast", 
  "avg_reb", "avg_fg_pct", "avg_fg3_pct", "avg_ft_pct", 
  "avg_fga", "avg_fg3a", "avg_fta", "avg_tov"
)

# Calculate weighted mean per team-season
team_profiles <- merged_data %>%
  group_by(Season, Team) %>%
  summarise(across(
    all_of(vars_to_weight),
    ~ weighted.mean(.x, w = MP, na.rm = TRUE),
    .names = "weighted_{.col}"
  )) %>%
  ungroup()

# Save weighted profiles
write_csv(team_profiles, "Team_Weighted_Profiles.csv")

# 4. Merge Cluster and Profile Data into One Frame
cluster_data <- read_csv("path/Team_Cluster_Composition.csv")
stat_data    <- read_csv("path/Team_Weighted_Profiles.csv")

# Merge on Season and Team
final_model_df <- left_join(cluster_data, stat_data, by = c("Season", "Team"))
print(head(final_model_df))

# Save the merged dataset
write_csv(final_model_df, "Final_Team_Model_Dataset.csv")

# 5. Clean Merged Dataset for Downstream Modeling
team_model_data <- read_csv("path/Final_Team_Model_Dataset.csv")

# Step 1: Drop rows with missing Team
team_model_data <- team_model_data %>% filter(!is.na(Team))

# Step 2: Inspect missing values
colSums(is.na(team_model_data))

# Step 3: Remove rows with missing values (strict)
team_model_data_clean <- team_model_data %>% drop_na()

# Step 4: Save cleaned data
write_csv(team_model_data_clean, "Team_Model_Clean.csv")
glimpse(team_model_data_clean)

# 6. Merge Clean Features with Net Rating Target Labels
team_features    <- read_csv("path/Team_Model_Clean.csv")
net_rating_data  <- read_csv("path/Final_Team_Net_Rating_Dataset.csv")

# Join datasets
final_model_data <- team_features %>%
  left_join(net_rating_data, by = c("Season", "Team"))

# Check for unmatched entries
missing_net_rating <- final_model_data %>% filter(is.na(NRtg))
print(missing_net_rating)

# Rename column for modeling compatibility
final_model_data <- final_model_data %>%
  rename(Net_Rating = NRtg)

# Save final dataset for modeling
write_csv(final_model_data, "Team_Model_Final_With_Target.csv")
glimpse(final_model_data)
