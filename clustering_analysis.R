# Position-Specific Clustering + Archetype Mapping of NBA Players (2015–2024)

# 1. Load Required Libraries
library(dplyr)
library(FactoMineR)
library(factoextra)
library(caret)
library(readr)
library(ggplot2)

# 2. Load Mapped Dataset with Group Labels
final_data_mapped <- read_csv("path/nba_final_dataset_with_groups.csv")

# 3. Define Expanded Feature Set for Clustering
selected_features <- c(
  "PER", "TS%", "USG%", "ORB%", "DRB%", "AST%", "TOV%", "STL%", "BLK%", 
  "BPM", "WS", "VORP", "avg_pts", "avg_ast", "avg_reb", 
  "3PAr", "FTr", "OBPM", "DBPM"
)

# 4. Function to Run PCA + KMeans for Each Position Group Separately
cluster_by_group <- function(data, group_name, n_clusters) {
  message("\nRunning clustering for: ", group_name)
  
  group_data <- data %>%
    filter(Position_Group == group_name) %>%
    select(all_of(selected_features)) %>%
    drop_na()
  
  preProc <- preProcess(group_data, method = c("center", "scale"))
  group_data_norm <- predict(preProc, group_data)
  
  pca_result <- PCA(group_data_norm, scale.unit = TRUE, graph = FALSE)
  pca_vars_5d <- as.data.frame(pca_result$ind$coord)[, 1:5]
  
  set.seed(123)
  kmeans_result <- kmeans(pca_vars_5d, centers = n_clusters, nstart = 25)
  
  return(kmeans_result$cluster)
}

# 5. Run Updated Clustering with Expanded Group Definitions
guards_clusters   <- cluster_by_group(final_data_mapped, "Guard",   4)
forwards_clusters <- cluster_by_group(final_data_mapped, "Forward", 4)
bigs_clusters     <- cluster_by_group(final_data_mapped, "Big",     3)

# 6. Assign Clusters Back to the Main Dataset
final_data_mapped$Group_Cluster <- NA_character_

final_data_mapped$Group_Cluster[final_data_mapped$Position_Group == "Guard"]   <- as.character(guards_clusters)
final_data_mapped$Group_Cluster[final_data_mapped$Position_Group == "Forward"] <- as.character(forwards_clusters)
final_data_mapped$Group_Cluster[final_data_mapped$Position_Group == "Big"]     <- as.character(bigs_clusters)

# Save updated clustered dataset
write_csv(final_data_mapped, "nba_position_specific_clusters_updated.csv")

# 7. Reload for Summarization and Mapping
final_data_mapped_updated <- read_csv("path/nba_position_specific_clusters_updated.csv")

# 8. Function to Summarize Each Cluster by Group Type
summarize_clusters <- function(data, group_name) {
  cat("\n\n Cluster Summaries for:", group_name, "\n")
  
  summary <- data %>%
    filter(Position_Group == group_name) %>%
    group_by(Group_Cluster) %>%
    summarise(across(all_of(selected_features), mean, na.rm = TRUE))
  
  print(summary)
}

# Cluster summaries
summarize_clusters(final_data_mapped_updated, "Guard")
summarize_clusters(final_data_mapped_updated, "Forward")
summarize_clusters(final_data_mapped_updated, "Big")

# 9. Map Cluster-Group Combinations to Archetypes
final_data_mapped_updated <- final_data_mapped_updated %>%
  mutate(Archetype = case_when(
    Position_Group == "Guard" & Group_Cluster == "1" ~ "Offensive Engines",
    Position_Group == "Guard" & Group_Cluster == "2" ~ "Defensive Guards",
    Position_Group == "Guard" & Group_Cluster == "3" ~ "Combo Guards",
    Position_Group == "Guard" & Group_Cluster == "4" ~ "Backup Guards",
    
    Position_Group == "Forward" & Group_Cluster == "1" ~ "3&D Wings",
    Position_Group == "Forward" & Group_Cluster == "2" ~ "Rebounding Forwards",
    Position_Group == "Forward" & Group_Cluster == "3" ~ "Star Forwards",
    Position_Group == "Forward" & Group_Cluster == "4" ~ "Bench Forwards",
    
    Position_Group == "Big" & Group_Cluster == "1" ~ "Dominant Bigs",
    Position_Group == "Big" & Group_Cluster == "2" ~ "Stretch 5s",
    Position_Group == "Big" & Group_Cluster == "3" ~ "Defensive Anchors",
    
    TRUE ~ NA_character_
  ))

# 10. Visualize Final Archetype Distributions
ggplot(final_data_mapped_updated %>% filter(!is.na(Archetype)),
       aes(x = Archetype, fill = Position_Group)) +
  geom_bar() +
  theme_minimal() +
  coord_flip() +
  labs(
    title = "NBA Player Archetypes by Position Group (Expanded)",
    x = "Player Archetype",
    y = "Number of Player-Seasons",
    fill = "Position Group"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )
