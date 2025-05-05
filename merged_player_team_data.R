# Merge Player Clustering with Team Net Rating Data
# 1. Load Required Libraries
library(dplyr)
library(readr)

# 2. Load Player and Team-Level Datasets
player_df <- read_csv("path/nba_position_specific_clusters_updated.csv")
team_df   <- read_csv("path/Final_Team_Net_Rating_Dataset.csv")

# 3. Convert yearSeason to NBA Season Format (e.g., "2015" -> "14-15")
player_df <- player_df %>%
  mutate(Season = paste0(substr(yearSeason - 1, 3, 4), "-", substr(yearSeason, 3, 4)))

# 🏷️ 4. Define Lookup Table: Team Abbreviation to Full Team Name
team_abbrev_to_name <- c(
  ATL = "Atlanta Hawks",        BOS = "Boston Celtics",     BRK = "Brooklyn Nets",
  CHI = "Chicago Bulls",        CHO = "Charlotte Hornets",  CLE = "Cleveland Cavaliers",
  DAL = "Dallas Mavericks",     DEN = "Denver Nuggets",     DET = "Detroit Pistons",
  GSW = "Golden State Warriors",HOU = "Houston Rockets",    IND = "Indiana Pacers",
  LAC = "Los Angeles Clippers", LAL = "Los Angeles Lakers", MEM = "Memphis Grizzlies",
  MIA = "Miami Heat",           MIL = "Milwaukee Bucks",    MIN = "Minnesota Timberwolves",
  NOP = "New Orleans Pelicans", NYK = "New York Knicks",    OKC = "Oklahoma City Thunder",
  ORL = "Orlando Magic",        PHI = "Philadelphia 76ers", PHO = "Phoenix Suns",
  POR = "Portland Trail Blazers",SAC = "Sacramento Kings",  SAS = "San Antonio Spurs",
  TOR = "Toronto Raptors",      UTA = "Utah Jazz",          WAS = "Washington Wizards"
)

#  5. Map Abbreviated Team Codes to Full Team Names
player_df$Team <- team_abbrev_to_name[player_df$slugTeam]

# 6. Merge Player and Team-Level Datasets by Season/Team
merged_df <- left_join(player_df, team_df, by = c("Team", "Season"))

# 7. Save Merged Dataset
write_csv(merged_df, "Merged_Player_Team_Data.csv")

# 8. (Optional) Preview Output
print(head(merged_df))
