#NBA Player Game Logs Aggregated to Season-Level (2015–2024) via `nbastatR`

#1. Increase Data Buffer Size (VROOM Engine)
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 5)  # Increase buffer for large downloads

#2. Load Required Libraries
library(nbastatR)
library(dplyr)
library(purrr)
library(readr)

# 3. Define Seasons to Process
seasons <- 2015:2024

# 4. Function to Fetch and Summarize Player Game Logs per Season
get_season_stats <- function(season_year) {
  message("📊 Processing season: ", season_year)
  
  logs <- game_logs(seasons = season_year, result_types = "player")
  
  stats <- logs %>%
    group_by(namePlayer, idPlayer, slugTeam, yearSeason) %>%
    summarise(
      games_played = n(),
      total_minutes = sum(minutes, na.rm = TRUE),
      avg_pts  = mean(pts, na.rm = TRUE),
      avg_ast  = mean(ast, na.rm = TRUE),
      avg_oreb = mean(oreb, na.rm = TRUE),
      avg_dreb = mean(dreb, na.rm = TRUE),
      avg_reb  = mean(oreb + dreb, na.rm = TRUE),
      avg_fg_pct   = mean(fgm / (fga + 1e-6), na.rm = TRUE),
      avg_fg3_pct  = mean(fg3m / (fg3a + 1e-6), na.rm = TRUE),
      avg_ft_pct   = mean(ftm / (fta + 1e-6), na.rm = TRUE),
      avg_fga  = mean(fga, na.rm = TRUE),
      avg_fg3a = mean(fg3a, na.rm = TRUE),
      avg_fta  = mean(fta, na.rm = TRUE),
      avg_tov  = mean(tov, na.rm = TRUE),
      avg_stl  = mean(stl, na.rm = TRUE),
      avg_blk  = mean(blk, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(games_played >= 30, total_minutes >= 500)  # Filter for consistent contributors
  
  return(stats)
}

#5. Apply Aggregation to All Seasons (2015–2024)
all_seasons_stats <- map_dfr(seasons, get_season_stats)

# 6. Save Final Aggregated Dataset to CSV
write_csv(all_seasons_stats, "path/nba_player_season_stats_2015_2024.csv")

