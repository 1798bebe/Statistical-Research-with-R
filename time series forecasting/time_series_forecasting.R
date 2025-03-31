# ================================
# 0. Setup and Data Loading
# ================================

# Set working directory (update path as needed)
setwd("C:/Users/SEC/Desktop/2024WS/projectR/dataset/filtered_dataset_optimized")

# Load all .rds files in the directory
rds_files <- list.files(pattern = "\\.rds$")
for (file in rds_files) {
  var_name <- tools::file_path_sans_ext(file)
  assign(var_name, readRDS(file))
}

# Load required libraries
library(tidyverse)
library(forecast)
library(prophet)
library(lubridate)
setwd("C:/Users/SEC/Desktop/2024WS/projectR/dataset/time_series_forecasting")

# ================================
# 1. Time Series Plot: Withdrawals
# ================================

# Target variable and country selection
feature_df <- withdrawals
country_code <- "DEU"  # Germany

# Justification for choosing this variable:
# - Freshwater withdrawals represent direct water demand across all sectors
# - It is mathematically tied to other derived features:
#     GDP (USD) = Water Productivity × Withdrawals
#     Water Stress = Withdrawals / Renewable Water Resources × 100
#   Therefore, orecasting this feature supports economic and sustainability-related insights

# Justification for choosing Germany:
# - Strong long-term declining trend (well-suited for modeling)
# - Moderate fluctuations in mid-years (enables learning variance)
# - No long flat periods (unlike many other countries)
#  : All these properties make the series forecastable and informative

# Reshape data for time series analysis
df <- feature_df %>%
  filter(Country.Code == country_code) %>%
  pivot_longer(cols = starts_with("X"), names_to = "Year", values_to = "Value") %>%
  mutate(Year = as.integer(sub("X", "", Year)))

# Visualize the series
ggplot(df, aes(x = Year, y = Value)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "black") +
  labs(
    title = paste("Time Series of Freshwater Withdrawals -", df$Country.Name[1]),
    x = "Year",
    y = "Withdrawals (billion m³)"
  ) +
  theme_minimal()

# ================================
# 2. ARIMA Forecasting
# ================================

# --- Full Forecast Using Entire Data ---

# Convert to time series object
ts_germany <- ts(df$Value, start = min(df$Year), frequency = 1) 
# frequency = 1 means yearly data (as opposed to monthly = 12, quarterly = 4).

# Fit ARIMA model: This uses the auto.arima() function to automatically determine 
# the best-fitting ARIMA model for the time series.
arima_model <- auto.arima(ts_germany)

# Forecast for 5 future years
arima_forecast <- forecast(arima_model, h = 5)

# Save ARIMA forecast plot
p1 <- autoplot(arima_forecast) +
  labs(title = "ARIMA Forecast - Germany Freshwater Withdrawals", x = "Year", y = "Withdrawals (billion m³)") +
  theme_minimal()
ggsave("arima_forecast_plot.png", p1, width = 8, height = 5)

# Save ARIMA forecast values
arima_out <- data.frame(Year = time(arima_forecast$mean), Forecast = as.numeric(arima_forecast$mean))
write.csv(arima_out, "arima_forecast_values.csv", row.names = FALSE)

# Display model summary
summary(arima_model)

# --- Evaluation on Held-Out Test Set (2018–2021) ---

# Split into training and testing sets
train <- df %>% filter(Year <= 2017)
test  <- df %>% filter(Year > 2017)

# Train ARIMA on training period
ts_train <- ts(train$Value, start = min(train$Year), frequency = 1)
arima_model_eval <- auto.arima(ts_train)

# Forecast test period
arima_fc <- forecast(arima_model_eval, h = nrow(test))
predicted <- as.numeric(arima_fc$mean)
actual <- test$Value

# Compute evaluation metrics
MAE  <- mean(abs(actual - predicted))
RMSE <- sqrt(mean((actual - predicted)^2))
MAPE <- mean(abs((actual - predicted) / actual)) * 100

# Print evaluation results
cat("ARIMA Metrics:\n")
cat("MAE: ", MAE, "\nRMSE:", RMSE, "\nMAPE:", MAPE, "%\n")

# ================================
# 3. Prophet Forecasting
# ================================

# --- Full Forecast Using Entire Data ---

# Prepare data for Prophet
prophet_df <- df %>%
  rename(ds = Year, y = Value) %>%
  mutate(ds = ymd(paste0(ds, "-01-01")))

# Fit Prophet model with yearly seasonality disabled (not needed for annual data)
prophet_model <- prophet(prophet_df, yearly.seasonality = FALSE)

# Create future dates (5 years ahead)
future <- make_future_dataframe(prophet_model, periods = 5, freq = "year")

# Forecast future withdrawals
forecast_prophet <- predict(prophet_model, future)

# Save Prophet forecast plot
p_prophet <- plot(prophet_model, forecast_prophet) +
  ggtitle("Prophet Forecast - Germany Freshwater Withdrawals")
ggsave("prophet_forecast_plot.png", plot = p_prophet, width = 8, height = 5)

# Save Prophet forecast values
prophet_out <- forecast_prophet %>% select(ds, yhat, yhat_lower, yhat_upper)
write.csv(prophet_out, "prophet_forecast_values.csv", row.names = FALSE)

# Save trend and uncertainty components
p_components <- prophet_plot_components(prophet_model, forecast_prophet)

# Save each component separately (There is only one component though.)
for (i in seq_along(p_components)) {
  ggsave(
    filename = paste0("prophet_component_", i, ".png"),
    plot = p_components[[i]],
    width = 8, height = 5
  )
}

# --- Evaluation on Held-Out Test Set (2018–2021) ---

# Re-train Prophet on training period only
prophet_train <- train %>%
  rename(ds = Year, y = Value) %>%
  mutate(ds = ymd(paste0(ds, "-01-01")))

prophet_model_eval <- prophet(prophet_train, yearly.seasonality = FALSE)

# Create future dates for test range
future_eval <- make_future_dataframe(prophet_model_eval, periods = nrow(test), freq = "year")
forecast_prophet_eval <- predict(prophet_model_eval, future_eval)

# Extract predictions for test period
predicted <- forecast_prophet_eval %>%
  filter(ds >= ymd("2018-01-01", tz = "UTC")) %>%
  pull(yhat)
actual <- test$Value

# Compute evaluation metrics
MAE  <- mean(abs(actual - predicted))
RMSE <- sqrt(mean((actual - predicted)^2))
MAPE <- mean(abs((actual - predicted) / actual)) * 100

# Print evaluation results
cat("Prophet Metrics:\n")
cat("MAE: ", MAE, "\nRMSE:", RMSE, "\nMAPE:", MAPE, "%\n")

