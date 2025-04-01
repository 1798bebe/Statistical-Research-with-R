# Clear environment
rm(list = ls())

# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# Prevent scientific notation
options(scipen = 999)

# Load datasets
data_list <- c("agriculture", "domestic", "gdppc", "gdppc_from1993", "industry", "info", "natural_disasters", 
               "population", "private_investment", "water_productivity", "water_stress", "withdrawals", 
               "available_resources", "gdp_ppp", "gdp_usd")
lapply(data_list, function(x) assign(x, readRDS(paste0(x, ".rds")), envir = .GlobalEnv))

# Compute mean GDP per capita
gdppc_avg <- gdppc %>%
  rowwise() %>%
  mutate(Mean_GDPPC = mean(c_across(starts_with("X")), na.rm = TRUE)) %>%
  ungroup() %>%
  select(Country.Name, Country.Code, Mean_GDPPC) %>%
  distinct()

# Compute geometric mean of water stress
water_stress_avg <- water_stress %>%
  pivot_longer(cols = starts_with("X"), names_to = "Year", values_to = "Water_Stress") %>%
  mutate(Water_Stress = as.numeric(format(Water_Stress, scientific = FALSE))) %>%
  group_by(Country.Code) %>%
  summarise(GeoMean_WaterStress = exp(mean(log(Water_Stress + 1), na.rm = TRUE)) - 1) %>%
  left_join(info %>% select(Country.Code, IncomeGroup), by = "Country.Code") %>%
  left_join(gdppc_avg, by = "Country.Code")

# Scatter plot of GDP per capita vs. water stress
colors <- c("High income" = "red", "Upper middle income" = "blue", "Lower middle income" = "green", "Low income" = "purple")

ggplot(water_stress_avg, aes(x = Mean_GDPPC, y = GeoMean_WaterStress, color = IncomeGroup)) +
  geom_point() +
  scale_x_log10(labels = comma) +
  scale_y_log10(labels = comma) +
  scale_color_manual(values = colors, limits = names(colors)) +
  theme_minimal() +
  labs(
    title = "GDP per Capita vs. Water Stress (Geometric Mean)",
    x = "Average GDP per Capita (Log Scale)",
    y = "Geometric Mean of Water Stress"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "bottom",
    legend.justification = "center"
  )