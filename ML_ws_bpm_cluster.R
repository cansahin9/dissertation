# Player Performance Prediction (BPM) Using Multiple ML Models (2015–2024)

# 1. Load Required Libraries
library(dplyr)
library(readr)
library(stringr)
library(caret)
library(xgboost)
library(glmnet)
library(ranger)
library(Metrics)
library(tidyr)
library(Cubist)
library(lightgbm)
library(kernlab)
library(ggplot2)

# 2. Load and Prepare Clustered Dataset
final_data_mapped_updated <- read_csv("path/nba_position_specific_clusters_updated.csv")

# Set seasons for training and testing
train_seasons <- 2015:2023
test_seasons  <- 2024

# Selected features (box + advanced stats)
selected_features <- c(
  "avg_pts", "avg_ast", "avg_reb", "avg_fg_pct", "avg_fg3_pct", "avg_ft_pct",
  "avg_fga", "avg_fg3a", "avg_fta", "avg_tov", "avg_stl", "avg_blk",
  "PER", "TS%", "USG%", "ORB%", "DRB%", "AST%", "TOV%", "STL%", "BLK%",
  "BPM", "WS", "VORP", "3PAr", "FTr", "OBPM", "DBPM"
)

# Add cluster label to features
full_features <- c(selected_features, "Group_Cluster")

# Build train and test datasets
train_data <- final_data_mapped_updated %>%
  filter(yearSeason %in% train_seasons) %>%
  select(all_of(full_features), WS, BPM)

test_data <- final_data_mapped_updated %>%
  filter(yearSeason %in% test_seasons) %>%
  select(all_of(full_features), WS, BPM)

# Save datasets
write_csv(train_data, "train_data_prediction.csv")
write_csv(test_data, "test_data_prediction.csv")

# 3. Load Clean Data for Modeling
train_data <- read_csv("path/train_data_prediction.csv") %>% drop_na()
test_data  <- read_csv("path/test_data_prediction.csv") %>% drop_na()


# 4. Model Setup — Target: Box Plus-Minus (BPM)
target_variable <- "BPM"
predictor_columns <- setdiff(names(train_data), c("WS", "BPM"))

# Cross-validation setup
train_control <- trainControl(method = "cv", number = 5)

# List of models to train
models_to_train <- c("ranger", "xgbTree", "glmnet", "svmRadial", "cubist", "gbm")
results <- list()

# Train each model
for (model_name in models_to_train) {
  message("\nTraining Model: ", model_name)
  
  model <- train(
    x = train_data[, predictor_columns],
    y = train_data[[target_variable]],
    method = model_name,
    trControl = train_control,
    preProcess = c("center", "scale")
  )
  
  results[[model_name]] <- model
}

# 5. Compare Cross-Validated Results (RMSE, MAE)
resamples <- resamples(results)
summary(resamples)
bwplot(resamples, metric = "RMSE")

# 6. Evaluate Each Model on Test Set (2024 Season)
evaluate_on_test <- function(model, model_name) {
  preds <- predict(model, newdata = test_data[, predictor_columns])
  true_vals <- test_data[[target_variable]]
  
  rmse_val <- rmse(true_vals, preds)
  mae_val <- mae(true_vals, preds)
  wape_val <- sum(abs(true_vals - preds)) / sum(abs(true_vals))
  
  cat("\n", model_name, "on Test Set:\n")
  cat("RMSE:", round(rmse_val, 3), "\n")
  cat("MAE:", round(mae_val, 3), "\n")
  cat("WAPE:", round(wape_val * 100, 2), "%\n")
}

for (model_name in names(results)) {
  evaluate_on_test(results[[model_name]], model_name)
}

# 7. Feature Importance Visualization for Cubist Model
cubist_importance <- varImp(results[["cubist"]])
importance_df <- as.data.frame(cubist_importance$importance)
importance_df$Feature <- rownames(importance_df)
importance_df <- importance_df[order(-importance_df$Overall), ]

# Plot
ggplot(importance_df, aes(x = reorder(Feature, Overall), y = Overall)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Feature Importance — Cubist Model (Predicting BPM)",
    x = "Feature", y = "Importance Score"
  ) +
  theme_minimal()
