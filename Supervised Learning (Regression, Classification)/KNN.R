rm(list = ls())

# ========== 0 :: LOAD LIBRARIES ========== #
library(tidyverse)
library(caret)
library(ggplot2)
library(MLmetrics)
library(VIM)

# ========== 1 :: READ ALL FEATURE FILES ========== #
rds_files <- list.files(pattern = "\\.rds$")
for (file in rds_files) {
  var_name <- tools::file_path_sans_ext(file)
  assign(var_name, readRDS(file))
}

# ========== 2 :: CREATE POPULATION LABELS ========== #
population_labels <- population %>%
  select(Country.Name, Country.Code, X2002, X2021) %>%
  mutate(
    growth_rate = (X2021 - X2002) / X2002 * 100,
    Label = ifelse(growth_rate < 30, "Slow_growth", "Rapid_growth")
  ) %>%
  select(Country.Code, Label)

label_levels <- c("Slow_growth", "Rapid_growth")
table(population_labels$Label)

# ========== 3 :: IMPUTE NATURAL DISASTER DATA (kNN) ========== #
natural_disasters <- natural_disasters %>%
  filter(Country.Code %in% water_stress$Country.Code)

nd <- natural_disasters %>%
  left_join(info %>% select(Country.Code, Region), by = "Country.Code")

nd_imputed <- kNN(nd, variable = "X2000", k = 5, imp_var = FALSE)

natural_disasters$X2000 <- nd_imputed$X2000
year_cols <- paste0("X", 2002:2021)
for (yr in 2002:2021) {
  natural_disasters[[paste0("X", yr)]] <- natural_disasters$X2000
}
natural_disasters <- natural_disasters %>% select(-X2000)

# ========== 4 :: COMBINE ALL FEATURES & LABELS ========== #
feature_list <- list(
  water_productivity = water_productivity,
  available_resources = available_resources,
  water_stress = water_stress,
  withdrawals = withdrawals,
  agriculture = agriculture,
  domestic = domestic,
  industry = industry,
  precipitation = precipitation,
  natural_disasters = natural_disasters
)

feature_list <- feature_list[
  map_lgl(feature_list, ~ any(year_cols %in% colnames(.x)))
]

feature_long <- reduce(
  map2(names(feature_list), feature_list, ~{
    .y %>%
      select(Country.Code, any_of(year_cols)) %>%
      pivot_longer(
        cols = any_of(year_cols),
        names_to = "Year",
        names_prefix = "X",
        values_to = .x
      ) %>%
      mutate(Year = as.integer(Year))
  }),
  full_join, by = c("Country.Code", "Year")
)

data_long <- feature_long %>%
  left_join(population_labels, by = "Country.Code") %>%
  drop_na(Label)

# ========== 5 :: CONVERT TO WIDE FORMAT FOR MODELLING ========== #
data_wide <- data_long %>%
  select(Country.Code, Year, all_of(names(feature_list)), Label) %>%
  pivot_wider(
    names_from = Year,
    values_from = all_of(names(feature_list)),
    names_sep = "_Y"
  ) %>%
  distinct()

X_full <- data_wide %>% select(-Country.Code, -Label)
y_factor <- factor(data_wide$Label, levels = label_levels)

complete_cases <- complete.cases(X_full)
X_full <- X_full[complete_cases, ]
y_factor <- y_factor[complete_cases]

# ========== 6 :: CROSS VALIDATION SETUP ========== #
ctrl_knn <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 10,
  verboseIter = TRUE,
  allowParallel = TRUE,
  classProbs = FALSE,
  summaryFunction = defaultSummary,
  savePredictions = "final"
)

knn_grid <- expand.grid(
  k = seq(3, 15, 2)
)

# ========== 7 :: TRAIN KNN MODEL ========== #
set.seed(123)

model_knn <- train(
  x = X_full,
  y = y_factor,
  method = "knn",
  tuneGrid = knn_grid,
  trControl = ctrl_knn,
  preProcess = "range",
  metric = "Accuracy"
)

# ========== 8 :: PLOT PERFORMANCE ========== #
ggplot(model_knn$results, aes(x = k, y = Accuracy)) +
  geom_point(color = "darkgreen") +
  geom_line(color = "darkgreen") +
  labs(
    title = "KNN Model Performance (Accuracy)",
    x = "Number of Neighbors (k)",
    y = "Accuracy"
  ) +
  theme_minimal()


# ========== 9 :: FINAL METRICS ========== #
knn_best <- model_knn$pred %>%
  filter(k == model_knn$bestTune$k)

cat("\n>>> KNN CONFUSION MATRIX:\n")
print(confusionMatrix(knn_best$pred, knn_best$obs))

cat("\n>>> KNN METRICS (Positive = 'Rapid_growth')\n")
cat("F1 Score:", round(F1_Score(knn_best$pred, knn_best$obs, positive = "Rapid_growth"), 4), "\n")
cat("Precision:", round(Precision(knn_best$pred, knn_best$obs, positive = "Rapid_growth"), 4), "\n")
cat("Recall:", round(Recall(knn_best$pred, knn_best$obs, positive = "Rapid_growth"), 4), "\n")

