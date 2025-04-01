rm(list = ls())

# Load necessary libraries
library(tidyverse)

# Define data file names
data_files <- c("agriculture", "domestic", "gdppc", "gdppc_from1993", "industry", "info", 
                "natural_disasters", "population", "private_investment", "water_productivity", 
                "water_stress", "withdrawals", "available_resources", "gdp_ppp", "gdp_usd")

lapply(data_files, function(file) assign(file, readRDS(paste0(file, ".rds")), envir = .GlobalEnv))

# Select year columns
years <- paste0("X", 1994:2021)

# Compute mean values and merge datasets
data_merged <- population %>%
  select(Country.Code, all_of(years)) %>%
  rowwise() %>%
  mutate(pop_mean = mean(c_across(starts_with("X")), na.rm = TRUE)) %>%
  ungroup() %>%
  inner_join(
    withdrawals %>%
      select(Country.Code, all_of(years)) %>%
      rowwise() %>%
      mutate(withdraw_mean = mean(c_across(starts_with("X")), na.rm = TRUE)) %>%
      ungroup(),
    by = "Country.Code"
  ) %>%
  select(Country.Code, pop_mean, withdraw_mean) %>%
  mutate(
    log_pop_mean = log10(pop_mean + 1),
    log_withdraw_mean = log10(withdraw_mean + 1)
  )

# Visualization
ggplot(data_merged, aes(x = log_pop_mean, y = log_withdraw_mean)) +
  geom_point(color = "blue", alpha = 0.6) +
  labs(
    title = "Population vs Freshwater Withdrawals (Log Scale)",
    x = "Log(Average Population)",
    y = "Log(Average Freshwater Withdrawals)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(labels = scales::math_format(10^.x)) +
  scale_y_continuous(labels = scales::math_format(10^.x))