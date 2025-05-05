# NBA Player Clustering (2015–2024) via PCA + K-Means + HC

# 1. Load Required Libraries
library(readr)
library(dplyr)
library(tidyr)
library(caret)
library(FactoMineR)
library(factoextra)
library(cluster)

# 2. Load and Preprocess Player Dataset
final_data <- read_csv("path/nba_final_dataset_2015_2024.csv")

# Selected features for clustering
selected_features <- c(
  "PER", "TS%", "USG%", "ORB%", "DRB%", "AST%", "TOV%", 
  "STL%", "BLK%", "BPM", "WS", "VORP", 
  "avg_pts", "avg_ast", "avg_reb"
)

# Filter relevant columns and drop NAs
clustering_data <- final_data %>%
  select(all_of(selected_features)) %>%
  drop_na()

# 3. Normalize Data Before PCA and Clustering
preProc <- preProcess(clustering_data, method = c("center", "scale"))
clustering_data_norm <- predict(preProc, clustering_data)

# 4. Principal Component Analysis (PCA)
pca_result <- PCA(clustering_data_norm, scale.unit = TRUE, graph = FALSE)

# Scree plot: How much variance each component explains
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50)) +
  ggtitle("Scree Plot: Variance Explained by Principal Components")

# Extract top 5 principal components
pca_vars <- as.data.frame(pca_result$ind$coord)
pca_vars_5d <- pca_vars[, 1:5]

# 5. K-Means Clustering on PCA-Reduced Features
# Determine optimal number of clusters using Elbow method
fviz_nbclust(pca_vars_5d, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) +
  labs(subtitle = "Elbow Method for Optimal Clusters")

# Final clustering with 4 clusters
set.seed(123)
kmeans_result <- kmeans(pca_vars_5d, centers = 4, nstart = 25)

# Append cluster labels to original dataset
final_clustered_data <- final_data %>%
  filter(!is.na(PER)) %>%
  mutate(Cluster = factor(kmeans_result$cluster))

# Save final clustered dataset
write_csv(final_clustered_data, "nba_clustered_dataset_2015_2024.csv")

# 6. Hierarchical Clustering (Ward's Method on PCA Space)
# Step 1: Distance matrix (Euclidean)
dist_matrix <- dist(pca_vars_5d, method = "euclidean")

# Step 2: Apply Ward’s method
hc_ward <- hclust(dist_matrix, method = "ward.D2")

# Step 3: Dendrogram visualization
fviz_dend(hc_ward, k = 4, rect = TRUE, show_labels = FALSE,
          main = "Hierarchical Clustering Dendrogram (Ward's Method)")

# Step 4: Cut tree to assign cluster labels
hc_clusters <- cutree(hc_ward, k = 4)

# Step 5: Attach hierarchical clusters to original data
final_clustered_data_hc <- final_data %>%
  filter(!is.na(PER)) %>%
  mutate(Hierarchical_Cluster = factor(hc_clusters))

# Step 6: Save hierarchical clustering results
write_csv(final_clustered_data_hc, "nba_hierarchical_clustered_dataset_2015_2024.csv")

