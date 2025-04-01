# Clear environment
rm(list = ls())

# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Load datasets
data_list <- c("agriculture", "domestic", "gdppc", "gdppc_from1993", "industry", "info", "natural_disasters", 
               "population", "private_investment", "water_productivity", "water_stress", "withdrawals", 
               "available_resources", "gdp_ppp", "gdp_usd")
lapply(data_list, function(x) assign(x, readRDS(paste0(x, ".rds")), envir = .GlobalEnv))

# Merge water_stress with IncomeGroup data
merged_data <- merge(water_stress, info, by = "Country.Code", all.x = TRUE)

# Calculate geometric mean of water stress by IncomeGroup and Year
geometric_means <- merged_data %>%
  group_by(IncomeGroup) %>%
  summarise(across(starts_with("X"), ~exp(mean(log(.), na.rm = TRUE)), .names = "geom_mean_{.col}"))

# Transform data into long format
gem_means_long <- geometric_means %>%
  pivot_longer(cols = starts_with("geom_mean"), 
               names_to = "Year", 
               values_to = "Geometric_Mean") %>%
  mutate(Year = gsub("geom_mean_X", "", Year))

# Plot geometric mean of water stress by IncomeGroup
ggplot(gem_means_long, aes(x = Year, y = Geometric_Mean, color = IncomeGroup, group = IncomeGroup)) +
  geom_line() +
  geom_point() +
  labs(title = "Geometric Mean of Water Stress by Income Group (1994-2021)",
       x = "Year", y = "Geometric Mean of Water Stress") +
  theme_minimal() +
  theme(
    legend.position = "bottom", 
    legend.justification = "center",
    plot.title = element_text(hjust = 0.5)
  ) +
  guides(color = guide_legend(nrow = 1))
