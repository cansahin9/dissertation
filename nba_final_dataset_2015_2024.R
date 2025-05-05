# Merge NBA Box Score with Advanced Stats (2015–2024)

# 1. Load Required Libraries
library(readr)
library(dplyr)
library(stringr)

# 2. Load Input Datasets
box_score_data <- read_csv("path/nba_player_season_stats_2015_2024.csv")
advanced_stats_raw <- read_csv("path/nba_advanced_stats_2015_2024.csv")

# 3. Clean Player Names for Matching
box_score_data <- box_score_data %>%
  mutate(
    namePlayer = str_replace_all(namePlayer, "\\*", ""),
    namePlayer = str_trim(namePlayer)
  )

advanced_stats <- advanced_stats_raw %>%
  mutate(
    Player = str_replace_all(Player, "\\*", ""),
    Player = str_trim(Player)
  )

# 4. Handle Mid-Season Trades: Keep Aggregated "TOT"/"2TM" Entries
advanced_stats <- advanced_stats %>%
  group_by(Player, Season) %>%
  filter(if_any(Team, ~ .x == "TOT" | .x == "2TM") | n() == 1) %>%
  ungroup()

# 5. Merge Datasets on Player Name and Season
final_data <- box_score_data %>%
  left_join(advanced_stats, by = c("namePlayer" = "Player", "yearSeason" = "Season"))

# 6. Inspect Merged Output
glimpse(final_data)

# 7. Save Final Output
write_csv(final_data, "nba_final_dataset_2015_2024.csv")

