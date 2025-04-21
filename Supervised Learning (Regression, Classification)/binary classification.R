# ====================================
# 0. Load Libraries and Setup
# ====================================
set.seed(123) 
library(tidyverse)
library(caret)
library(MLmetrics)
library(pROC)
library(pdp)
library(rnaturalearth)
library(sf)

# ====================================
# 1. Load Feature Data (.rds files)
# ====================================
setwd("C:/Users/SEC/Desktop/2024WS/projectR/dataset/filtered_dataset_finalized")

rds_files <- list.files(pattern = "\\.rds$")
for (file in rds_files) {
  var_name <- tools::file_path_sans_ext(file)
  assign(var_name, readRDS(file))
}

# ====================================
# 2. Create Population Growth Labels
# ====================================
setwd("C:/Users/SEC/Desktop/2024WS/projectR/dataset/classification")
population_labels <- population %>%
  dplyr::select(Country.Name, Country.Code, X2002, X2021) %>%
  mutate(
    growth_rate = (X2021 - X2002) / X2002 * 100,
    Label = case_when(
      growth_rate < 30 ~ "Slow_growth", 
      TRUE ~ "Rapid_growth"
    )
  ) %>%
  dplyr::select(Country.Code, Label)

label_levels <- c("Slow_growth", "Rapid_growth")
table(population_labels$Label)

# ====================================
# 3. Average Features Per Country
# ====================================
YEAR_RANGE <- paste0("X", 2002:2021)

feature_list <- list(
  water_productivity = water_productivity, 
  available_resources = available_resources,
  water_stress = water_stress,
  # withdrawals = withdrawals,  
  agriculture = agriculture,
  # domestic = domestic, Two features were removed due to redundancy 
  industry = industry
  # precipitation = precipitation
)

extended_features <- function(df, feature_name) {
  df %>%
    mutate(across(all_of(YEAR_RANGE), as.numeric)) %>%
    rowwise() %>%
    mutate(
      !!paste0(feature_name, "_mean") := mean(c_across(all_of(YEAR_RANGE)), na.rm = TRUE),
      !!paste0(feature_name, "_sd") := sd(c_across(all_of(YEAR_RANGE)), na.rm = TRUE),
      !!paste0(feature_name, "_min") := min(c_across(all_of(YEAR_RANGE)), na.rm = TRUE),
      !!paste0(feature_name, "_max") := max(c_across(all_of(YEAR_RANGE)), na.rm = TRUE),
      !!paste0(feature_name, "_slope") := coef(lm(c_across(all_of(YEAR_RANGE)) ~ as.numeric(2002:2021)))[2]
    ) %>%
    ungroup() %>%
    dplyr::select(Country.Code, ends_with("_mean"), ends_with("_sd"),
           ends_with("_min"), ends_with("_max"), ends_with("_slope"))
}

extended_features_list <- map2(feature_list, names(feature_list), extended_features)
X_dynamic <- reduce(extended_features_list, left_join, by = "Country.Code")

final_data <- left_join(X_dynamic, population_labels, by = "Country.Code") %>%
  drop_na()

X_full <- final_data %>% dplyr::select(-Country.Code, -Label)
y_factor <- factor(final_data$Label, levels = label_levels)

# ====================================
# 4. Cross‑Validation & Tuning Control
# ====================================
f1_summary <- function(data, lev = NULL, model = NULL) {
  f1 <- F1_Score(data$pred, data$obs, positive = "Rapid_growth")
  c(F1 = f1)
}

rand_ctrl <- trainControl(
  method = "repeatedcv", number = 5, repeats = 10,
  search = "random", summaryFunction = f1_summary,
  classProbs = TRUE
)

# ====================================
# 5. Nested Cross‑Validation Training and Evaluation
# ====================================
# Outer 5‑fold split
outer_folds <- createFolds(y_factor, k = 5, returnTrain = TRUE)

# Prepare a results data.frame
nested_results <- data.frame(
  fold      = integer(),
  model     = character(),
  F1        = numeric(),
  Accuracy  = numeric(),
  stringsAsFactors = FALSE
)

# Initialize storage for GLMNET’s out‑of‑fold predictions and true labels by country
all_glm_preds <- tibble(
  Country = character(),
  truth   = factor(levels = levels(y_factor)),
  pred    = factor(levels = levels(y_factor))
)

# Initialize a separate container for GLMNET-specific metrics
glmnet_metrics <- data.frame(
  fold        = integer(),
  Sensitivity = numeric(),
  Specificity = numeric(),
  AUC         = numeric(),
  stringsAsFactors = FALSE
)

# Nested Cross Validation Loop
for (i in seq_along(outer_folds)) {
  train_idx <- outer_folds[[i]]
  test_idx  <- setdiff(seq_len(nrow(X_full)), train_idx)
  
  X_train <- X_full[train_idx, ];   y_train <- y_factor[train_idx]
  X_test  <- X_full[test_idx,  ];   y_test  <- y_factor[test_idx]
  
  # --- Inner tuning & training for each model ---
  # 1) Random Forest
  model_rf <- train(
    x         = X_train, y = y_train,
    method    = "rf",
    trControl = rand_ctrl,
    tuneLength= 30,
    preProcess= "range",
    metric    = "F1"
  )
  preds_rf <- predict(model_rf, newdata = X_test)
  f1_rf    <- F1_Score(preds_rf, y_test, positive = "Rapid_growth")
  acc_rf   <- mean(preds_rf == y_test)
  
  nested_results <- rbind(nested_results, 
                          data.frame(fold = i, model = "RF",  F1 = f1_rf,  Accuracy = acc_rf)
  )
  
  # 2) XGBoost
  model_xgb <- train(
    x         = X_train, y = y_train,
    method    = "xgbTree",
    trControl = rand_ctrl,
    tuneLength= 30,
    preProcess= "range",
    metric    = "F1"
  )
  preds_xgb <- predict(model_xgb, newdata = X_test)
  f1_xgb    <- F1_Score(preds_xgb, y_test, positive = "Rapid_growth")
  acc_xgb   <- mean(preds_xgb == y_test)
  
  nested_results <- rbind(nested_results, 
                          data.frame(fold = i, model = "XGBoost", F1 = f1_xgb, Accuracy = acc_xgb)
  )
  
  # 3) GLMNET
  model_glmnet <- train(
    x         = X_train, y = y_train,
    method    = "glmnet",
    trControl = rand_ctrl,
    tuneLength= 30,
    preProcess= c("center", "scale"),
    metric    = "F1"
  )
  preds_glm <- predict(model_glmnet, newdata = X_test) # hard predictions
  probs_glm <- predict(model_glmnet, newdata = X_test, type = "prob")[, "Rapid_growth"] # probabilities for AUC
  
  # capture out‑of‑fold predictions for the error map
  all_glm_preds <- bind_rows(all_glm_preds, 
    tibble(Country = final_data$Country.Code[test_idx], truth = y_test, pred = preds_glm))
  
  # performance metrics
  f1_glm    <- F1_Score(preds_glm, y_test, positive = "Rapid_growth")
  acc_glm   <- mean(preds_glm == y_test)
  cm_glm <- confusionMatrix(preds_glm, y_test, positive = "Rapid_growth")
  auc_glm <- as.numeric(auc(roc(response = y_test, predictor = probs_glm)))
  
  # append F1 and accuracy to nested_results
  nested_results <- rbind(nested_results, 
                          data.frame(fold = i, model = "GLMNET", F1 = f1_glm, Accuracy = acc_glm))
  
  # append GLMNET‑only metrics                        
  glmnet_metrics <- rbind(glmnet_metrics,
    data.frame(
      fold        = i,
      Sensitivity = cm_glm$byClass["Sensitivity"],
      Specificity = cm_glm$byClass["Specificity"],
      AUC         = auc_glm,
      stringsAsFactors = FALSE)
  )
}

# Random baseline model: predict each class uniformly at random
baseline_results <- nested_results[1:0, ]  # empty df with same cols
for (i in seq_along(outer_folds)) {
  test_idx <- setdiff(seq_len(nrow(X_full)), outer_folds[[i]])
  y_test   <- y_factor[test_idx]
  
  # randomly predict “Slow_growth” or “Rapid_growth” with equal probability
  p_base <- factor(
    sample(levels(y_factor), size = length(y_test), replace = TRUE),
    levels = levels(y_factor)
  )
  
  baseline_results <- rbind(
    baseline_results,
    data.frame(
      fold     = i,
      model    = "RandomBaseline",
      F1       = F1_Score(p_base, y_test, positive = "Rapid_growth"),
      Accuracy = mean(p_base == y_test),
      stringsAsFactors = FALSE
    )
  )
}

# Combine with nested_results 
baseline_results <- baseline_results[-1, ]
nested_results <- bind_rows(nested_results, baseline_results)

# ====================================
# 6. Summarize Nested CV Comparison
# ====================================
# Numeric summary
summary_tbl <- nested_results %>%
  group_by(model) %>%
  summarise(
    mean_F1  = mean(F1),
    sd_F1    = sd(F1),
    mean_Accuracy = mean(Accuracy),
    sd_Accuracy   = sd(Accuracy),
    .groups = "drop"
  )
print(summary_tbl)

# Summarize GLMNET-specific metrics
glmnet_summary_ci <- glmnet_metrics %>%
  summarise(
    mean_Sensitivity = mean(Sensitivity, na.rm = TRUE),
    ci_Sensitivity_L = mean_Sensitivity - qt(0.975, df = n() - 1) * sd(Sensitivity, na.rm = TRUE) / sqrt(n()),
    ci_Sensitivity_U = mean_Sensitivity + qt(0.975, df = n() - 1) * sd(Sensitivity, na.rm = TRUE) / sqrt(n()),
    
    mean_Specificity = mean(Specificity, na.rm = TRUE),
    ci_Specificity_L = mean_Specificity - qt(0.975, df = n() - 1) * sd(Specificity, na.rm = TRUE) / sqrt(n()),
    ci_Specificity_U = mean_Specificity + qt(0.975, df = n() - 1) * sd(Specificity, na.rm = TRUE) / sqrt(n()),
    
    mean_AUC = mean(AUC, na.rm = TRUE),
    ci_AUC_L = mean_AUC - qt(0.975, df = n() - 1) * sd(AUC, na.rm = TRUE) / sqrt(n()),
    ci_AUC_U = mean_AUC + qt(0.975, df = n() - 1) * sd(AUC, na.rm = TRUE) / sqrt(n())
  )

print(glmnet_summary_ci)

# Boxplot of out‑of‑sample F1 by model
ggplot(nested_results, aes(x = model, y = F1, fill = model)) +
  geom_boxplot() +
  labs(
    title = "Nested CV out of Sample F1 by Model",
    x     = "Algorithm",
    y     = "F1 score"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Boxplot of out‑of‑sample Accuracy
ggplot(nested_results, aes(x = model, y = Accuracy, fill = model)) +
  geom_boxplot() +
  labs(
    title = "Nested CV out of Sample Accuracy by Model",
    x     = "Algorithm",
    y     = "Accuracy"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# <<Hypothesis test for the comparison of F1, Accuracy across models>>
# Reshape nested_results into wide format
f1_wide <- nested_results %>%
  filter(model %in% c("GLMNET","RF","XGBoost")) %>%
  select(fold, model, F1) %>%
  pivot_wider(names_from = model, values_from = F1)

acc_wide <- nested_results %>%
  filter(model %in% c("GLMNET","RF","XGBoost")) %>%
  select(fold, model, Accuracy) %>%
  pivot_wider(names_from = model, values_from = Accuracy)

# Wilcoxon signed‑rank tests (nonparametric alternative)
w_f1_glm_rf  <- wilcox.test(f1_wide$GLMNET, f1_wide$RF,  paired = TRUE, exact = FALSE)
w_f1_glm_xgb <- wilcox.test(f1_wide$GLMNET, f1_wide$XGBoost, paired = TRUE, exact = FALSE)
w_f1_rf_xgb  <- wilcox.test(f1_wide$RF, f1_wide$XGBoost, paired = TRUE, exact = FALSE)

w_acc_glm_rf  <- wilcox.test(acc_wide$GLMNET, acc_wide$RF,  paired = TRUE, exact = FALSE)
w_acc_glm_xgb <- wilcox.test(acc_wide$GLMNET, acc_wide$XGBoost, paired = TRUE, exact = FALSE)
w_acc_rf_xgb  <- wilcox.test(acc_wide$RF, acc_wide$XGBoost, paired = TRUE, exact = FALSE)

# Effect sizes (mean differences)  
eff_f1_glm_rf  <- mean(f1_wide$GLMNET - f1_wide$RF)
eff_f1_glm_xgb <- mean(f1_wide$GLMNET - f1_wide$XGBoost)
eff_f1_rf_xgb  <- mean(f1_wide$RF - f1_wide$XGBoost)

eff_acc_glm_rf  <- mean(acc_wide$GLMNET - acc_wide$RF)
eff_acc_glm_xgb <- mean(acc_wide$GLMNET - acc_wide$XGBoost)
eff_acc_rf_xgb  <- mean(acc_wide$RF - acc_wide$XGBoost)

# Summarize results
results_df <- tibble(
  Comparison       = c("GLMNET vs RF", "GLMNET vs XGBoost", "RF vs XGBoost"),
  wilcox_p_f1      = c(w_f1_glm_rf$p.value, w_f1_glm_xgb$p.value, w_f1_rf_xgb$p.value),
  mean_diff_F1     = c(eff_f1_glm_rf, eff_f1_glm_xgb, eff_f1_rf_xgb),
  wilcox_p_accuracy= c(w_acc_glm_rf$p.value, w_acc_glm_xgb$p.value, w_acc_rf_xgb$p.value),
  mean_diff_Acc    = c(eff_acc_glm_rf, eff_acc_glm_xgb, eff_acc_rf_xgb)
)

print(results_df)

# ====================================
# 7. Model Interpretation
# ====================================
# Tune GLMNET globally on 100% of the data
tuned_glmnet_full <- train(
  x         = X_full,
  y         = y_factor,
  method    = "glmnet",
  trControl = rand_ctrl,     # repeatedCV + random search as before
  tuneLength= 30,
  preProcess= c("center", "scale"),
  metric    = "F1"
)
best_glmnet_params <- tuned_glmnet_full$bestTune

# Fit final GLMNET on 100% of the data with that bestTune
final_glm <- train(
  x         = as.data.frame(X_full),    # avoid tibble row‑name warnings
  y         = y_factor,
  method    = "glmnet",
  trControl = trainControl(method = "none"), 
  tuneGrid  = best_glmnet_params,        # the global bestTune
  preProcess= c("center", "scale"),
  metric    = "F1"
)

# Variable Importance (Top 10)
vip <- varImp(final_glm)$importance %>%
  rownames_to_column("feature") %>%
  arrange(desc(Overall)) %>%
  slice(1:10)

ggplot(vip, aes(x = reorder(feature, Overall), y = Overall)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 Predictors by GLMNET Variable Importance",
    x     = NULL,
    y     = "Importance"
  ) +
  theme_minimal()

# Coefficient Directions & Odds Ratios
coefs_mat <- as.matrix(
  coef(final_glm$finalModel, s = final_glm$bestTune$lambda)
)

coefs_df <- data.frame(
  term     = rownames(coefs_mat),
  estimate = coefs_mat[, 1],
  row.names = NULL,
  stringsAsFactors = FALSE
) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    odds_ratio = exp(estimate),
    direction  = ifelse(estimate > 0, "↑ Rapid_growth", "↑ Slow_growth")
  ) %>%
  arrange(desc(abs(estimate))) %>%
  slice(1:10)

print(coefs_df)

# Partial Dependence Plots for Top 3
top3 <- vip$feature[1:3]
for (feat in top3) {
  pd <- partial(final_glm, pred.var = feat, 
                prob = TRUE, which.class = "Rapid_growth")
  p <- autoplot(pd) +
    labs(title = paste("PDP for", feat), x = feat, y = "Predicted P(Rapid_growth)") +
    theme_minimal()
  print(p)
}

# ====================================
# 8. Country‑Level Misclassification Map (codes only)
# ====================================
# Compute per country misclassification rate from out‑of‑fold GLMNET predictions
error_rate <- all_glm_preds %>%
  group_by(Country) %>%
  summarise(
    mis_rate = mean(pred != truth),
    .groups  = "drop"
  )

# keep only the ISO3 code and geometry
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(iso_a3, geometry)

# join on the ISO3 code
world_err <- left_join(
  world,
  error_rate,
  by = c("iso_a3" = "Country")
)

# Plot choropleth and label each country by its ISO3 code
ggplot(world_err) +
  geom_sf(aes(fill = mis_rate), color = NA) +
  geom_sf_text(aes(label = iso_a3), size = 2) +
  scale_fill_viridis_c(
    name     = "Misclassification\nRate",
    na.value = "grey90"
  ) +
  labs(
    title    = "GLMNET Out of Fold Misclassification by Country (ISO3 codes)",
    subtitle = "Each label is the 3 letter country code"
  ) +
  theme_minimal() +
  theme(
    axis.text  = element_blank(),
    panel.grid = element_line(color = "transparent")
  )

# ====================================
# 9. Save Models
# ====================================

# Save the final GLMNET model
saveRDS(final_glm, "final_glmnet_model.rds")

# Also save the three models used for comparison
saveRDS(model_glmnet, "glmnet_model.rds")
saveRDS(model_rf,  "random_forest_model.rds")
saveRDS(model_xgb, "xgboost_model.rds")
