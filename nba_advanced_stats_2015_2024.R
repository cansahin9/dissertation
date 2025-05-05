# Combine NBA Advanced Stats CSVs (Multiple Seasons, 2015–2024)

# 1. Load Required Libraries
library(readr)
library(dplyr)
library(stringr)
library(purrr)

# 2. Define Folder Containing Extracted CSV Files
data_folder <- "path/data"

#  3. List All CSV Files in the Directory
file_list <- list.files(path = data_folder, pattern = "\\.csv$", full.names = TRUE)

# 4. Function: Read, Clean, and Tag Each File by Season
load_and_process <- function(file_path) {
  df <- read_csv(file_path)
  
  # Extract season format (e.g., "15-16") from filename
  file_name <- basename(file_path)
  season <- str_extract(file_name, "\\d{2}-\\d{2}")
  
  # Convert "15-16" → 2016 (the end year of the season)
  season_end_year <- as.numeric(paste0("20", str_sub(season, 4, 5)))
  
  # Clean player names and attach season info
  df <- df %>%
    mutate(
      Player = str_replace_all(Player, "\\*", ""),
      Season = season_end_year
    )
  
  return(df)
}

# 5. Combine All Files into a Single Dataset
advanced_stats_all <- file_list %>%
  map_dfr(load_and_process)

# 6. Preview Merged Output
head(advanced_stats_all)

# 7. Save Final Combined CSV
write_csv(advanced_stats_all, "nba_advanced_stats_2015_2024.csv")

