# Predicting NBA Team Net Rating with Ensemble Modeling

# 1. Load Libraries and Setup
library(tidyverse)
library(caret)
library(xgboost)
library(gbm)
library(e1071)
library(Cubist)
library(randomForest)
library(patchwork)
library(DALEX)
library(iml)
library(caretEnsemble)
library(tibble)
library(dplyr)
library(forcats)
library(corrplot)
library(naniar)

set.seed(123)

# 2. Load and Preprocess Input Data
data <- read_csv("path/Team_Model_Final_With_Target.csv")

# Remove zero-variance and leaky features
data <- data %>%
  select(-Cluster_0_Pct, -MOV, -`MOV/A`, -ORtg, -DRtg, -`ORtg/A`, -`DRtg/A`, -`NRtg/A`)

#3. Train-Test Split Creation
train_index <- createDataPartition(data$Net_Rating, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data  <- data[-train_index, ]

# 4. Define Predictors and Target Column
target_variable <- "Net_Rating"
predictor_columns <- setdiff(names(data), c("Season", "Team", target_variable))

# 5. Feature Selection via Recursive Feature Elim
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, savePredictions = "final")
rfe_ctrl <- rfeControl(functions = rfFuncs, method = "repeatedcv", number = 10, repeats = 5)

rfe_result <- rfe(
  x = train_data[, predictor_columns],
  y = train_data[[target_variable]],
  sizes = c(5, 10, 15, 20, 25, 28),
  rfeControl = rfe_ctrl
)
print(rfe_result)
plot(rfe_result, type = c("g", "o"))

# 6. Train Base Machine Learning Models
models <- list(
  glmnet = "glmnet",
  cubist = "cubist",
  rf = "rf",
  xgbTree = "xgbTree"
)

results <- list()
for (model_name in names(models)) {
  cat("Training model:", model_name, "\n")
  model <- train(
    x = train_data[, predictor_columns],
    y = train_data[[target_variable]],
    method = models[[model_name]],
    trControl = ctrl,
    preProcess = c("center", "scale")
  )
  results[[model_name]] <- model
}

# 7. Stacked Ensemble Model
stack_ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, savePredictions = "final")
stack_list <- caretList(
  x = train_data[, predictor_columns],
  y = train_data[[target_variable]],
  trControl = stack_ctrl,
  methodList = c("glmnet", "cubist"),
  preProcess = c("center", "scale")
)
stack_model <- caretStack(stack_list, method = "glm")

# 8. Evaluate Models on Test Set
evaluate_on_test <- function(model, model_name, test_data, target_variable, predictor_columns) {
  preds <- predict(model, newdata = test_data[, predictor_columns])
  true_vals <- test_data[[target_variable]]
  tibble(
    Model = model_name,
    RMSE  = RMSE(preds, true_vals),
    MAE   = MAE(preds, true_vals),
    WAPE  = sum(abs(true_vals - preds)) / sum(abs(true_vals)) * 100
  )
}

test_metrics <- map_dfr(names(results), function(name) {
  evaluate_on_test(results[[name]], name, test_data, target_variable, predictor_columns)
})

stack_preds <- predict(stack_model, newdata = test_data[, predictor_columns])
stack_eval <- tibble(
  Model = "Stacked (GLMNet + Cubist)",
  RMSE  = RMSE(unlist(stack_preds), test_data[[target_variable]]),
  MAE   = MAE(unlist(stack_preds), test_data[[target_variable]]),
  WAPE  = sum(abs(test_data[[target_variable]] - unlist(stack_preds))) / sum(abs(test_data[[target_variable]])) * 100
)
test_metrics <- bind_rows(test_metrics, stack_eval)

# 9. Visualize Model Performance
test_metrics %>%
  mutate(Model = fct_reorder(Model, RMSE)) %>%
  ggplot(aes(x = Model, y = RMSE, fill = Model)) +
  geom_col() +
  labs(title = "Test RMSE by Model", y = "RMSE") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()

stack_resid <- test_data[[target_variable]] - unlist(stack_preds)
ggplot(data.frame(True = test_data[[target_variable]], Residual = stack_resid),
       aes(x = True, y = Residual)) +
  geom_point(color = "darkgreen") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. True Net Rating (Stacked Ensemble)") +
  theme_minimal()

# 10. SHAP Explainability for XGBoost (Full Model)
train_df <- train_data[, predictor_columns] %>% as.data.frame()
test_df  <- test_data[, predictor_columns] %>% as.data.frame()
train_label <- train_data[[target_variable]]
test_label  <- test_data[[target_variable]]

dalex_predict <- function(model, data) predict(model, data.matrix(data))
xgb_model <- results[["xgbTree"]]$finalModel

explainer <- explain(
  model = xgb_model,
  data = test_df,
  y = test_label,
  predict_function = dalex_predict,
  label = "XGBoost"
)

shap_values <- predict_parts(
  explainer = explainer,
  new_observation = test_df[1, , drop = FALSE],
  type = "shap"
)
plot(shap_values)

# 11. RFE-Based Modeling with Reduced Features
predictor_columns <- predictors(rfe_result)
train_x <- train_data[, predictor_columns]
test_x  <- test_data[, predictor_columns]
train_y <- train_data[[target_variable]]
test_y  <- test_data[[target_variable]]

results_rfe <- list()
for (model_name in names(models)) {
  cat("Training (RFE) model:", model_name, "\n")
  model <- train(
    x = train_x,
    y = train_y,
    method = models[[model_name]],
    trControl = ctrl,
    preProcess = c("center", "scale")
  )
  results_rfe[[model_name]] <- model
}

stack_list_rfe <- caretList(
  x = train_x,
  y = train_y,
  trControl = ctrl,
  methodList = c("glmnet", "cubist"),
  preProcess = c("center", "scale")
)
stack_model_rfe <- caretStack(stack_list_rfe, method = "glm")

evaluate_on_test_rfe <- function(model, model_name) {
  preds <- predict(model, newdata = test_x)
  tibble(
    Model = model_name,
    RMSE  = RMSE(preds, test_y),
    MAE   = MAE(preds, test_y),
    WAPE  = sum(abs(test_y - preds)) / sum(abs(test_y)) * 100
  )
}

test_metrics_rfe <- map_dfr(names(results_rfe), function(name) {
  evaluate_on_test_rfe(results_rfe[[name]], name)
})

stack_preds_rfe <- predict(stack_model_rfe, newdata = test_x)
stack_eval_rfe <- tibble(
  Model = "Stacked (GLMNet + Cubist)",
  RMSE  = RMSE(unlist(stack_preds_rfe), test_y),
  MAE   = MAE(unlist(stack_preds_rfe), test_y),
  WAPE  = sum(abs(test_y - unlist(stack_preds_rfe))) / sum(abs(test_y)) * 100
)
test_metrics_rfe <- bind_rows(test_metrics_rfe, stack_eval_rfe)

test_metrics_rfe %>%
  mutate(Model = fct_reorder(Model, RMSE)) %>%
  ggplot(aes(x = Model, y = RMSE, fill = Model)) +
  geom_col() +
  labs(title = "Test RMSE by Model (RFE Features)", y = "RMSE") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()

# 12. SHAP on RFE Model (XGBoost)
selected_features <- predictors(rfe_result)
train_rfe_df <- train_data[, selected_features]
test_rfe_df  <- test_data[, selected_features]

xgb_model_rfe <- xgboost::xgboost(
  data = data.matrix(train_rfe_df),
  label = train_label,
  nrounds = 100,
  objective = "reg:squarederror",
  verbose = 0
)

explainer_rfe <- explain(
  model = xgb_model_rfe,
  data = test_rfe_df,
  y = test_label,
  predict_function = dalex_predict,
  label = "XGBoost (RFE)"
)

shap_all <- predict_parts(
  explainer = explainer_rfe,
  new_observation = test_rfe_df,
  type = "shap",
  B = 10
)
plot(shap_all) +
  ggtitle("SHAP Summary Plot – XGBoost (RFE Features)")

# 13. Correlation and Missing Data Insights
num_data <- select_if(data, is.numeric)
clean_names <- gsub("weighted_", "", colnames(num_data))
clean_names <- gsub("Cluster_", "C", clean_names)
cor_matrix <- cor(num_data, use = "complete.obs")
colnames(cor_matrix) <- clean_names
rownames(cor_matrix) <- clean_names

corrplot(cor_matrix,
         method = "color",
         type = "upper",
         col = colorRampPalette(c("steelblue", "white", "tomato"))(200),
         tl.cex = 0.8,
         tl.col = "black",
         number.cex = 0.6,
         mar = c(0, 0, 2, 0),
         title = "Correlation Heatmap of Model Features (Simplified Labels)")

gg_miss_var(data) +
  labs(title = "Missing Values per Variable",
       x = "Variable", y = "Missing Count") +
  theme_minimal()

data %>%
  summarise(across(starts_with("Cluster_"), mean)) %>%
  pivot_longer(everything(), names_to = "Cluster", values_to = "AvgShare") %>%
  ggplot(aes(x = Cluster, y = AvgShare, fill = Cluster)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Average Team Composition by Cluster",
       x = "Cluster Archetype", y = "Average Proportion") +
  theme_minimal()
