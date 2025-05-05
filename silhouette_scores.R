# Cluster Comparison: K-Means vs Ward’s Method (NBA 2015–2024)

# 1. Load Required Libraries
library(readr)
library(dplyr)
library(tidyr)
library(caret)
library(FactoMineR)
library(factoextra)
library(cluster)

# 2. Load Clustered and Raw Player Datasets
kmeans_data <- read_csv("path/nba_clustered_dataset_2015_2024.csv")
ward_data   <- read_csv("path/nba_hierarchical_clustered_dataset_2015_2024.csv")
final_data  <- read_csv("path/nba_final_dataset_2015_2024.csv")

# 3. Compare Cluster Sizes
cat("Cluster Sizes (K-Means):\n")
print(table(kmeans_data$Cluster))

cat("\n Cluster Sizes (Ward's Method):\n")
print(table(ward_data$Hierarchical_Cluster))

# 4. Cluster Feature Profiles (Mean by Cluster)
selected_features <- c(
  "PER", "TS%", "USG%", "ORB%", "DRB%", "AST%", "TOV%", 
  "STL%", "BLK%", "BPM", "WS", "VORP", 
  "avg_pts", "avg_ast", "avg_reb"
)

cat("\n K-Means Cluster Feature Means:\n")
kmeans_profiles <- kmeans_data %>%
  group_by(Cluster) %>%
  summarise(across(all_of(selected_features), mean, na.rm = TRUE))
print(kmeans_profiles)

cat("\n Ward’s Method Cluster Feature Means:\n")
ward_profiles <- ward_data %>%
  group_by(Hierarchical_Cluster) %>%
  summarise(across(all_of(selected_features), mean, na.rm = TRUE))
print(ward_profiles)

# 5. Silhouette Analysis for Cluster Quality
# Prepare normalized PCA data
clustering_data <- final_data %>%
  select(all_of(selected_features)) %>%
  drop_na()

preProc <- preProcess(clustering_data, method = c("center", "scale"))
clustering_data_norm <- predict(preProc, clustering_data)

# Perform PCA and extract first 5 components
pca_result <- PCA(clustering_data_norm, scale.unit = TRUE, graph = FALSE)
pca_vars_5d <- as.data.frame(pca_result$ind$coord)[, 1:5]

# Silhouette: K-Means
sil_kmeans <- silhouette(as.integer(kmeans_data$Cluster), dist(pca_vars_5d))
avg_sil_kmeans <- mean(sil_kmeans[, 3])

# Silhouette: Ward
sil_ward <- silhouette(as.integer(ward_data$Hierarchical_Cluster), dist(pca_vars_5d))
avg_sil_ward <- mean(sil_ward[, 3])

# Output Silhouette Scores
cat("\n Average Silhouette Width (K-Means):", round(avg_sil_kmeans, 3), "\n")
cat("Average Silhouette Width (Ward's Method):", round(avg_sil_ward, 3), "\n")
