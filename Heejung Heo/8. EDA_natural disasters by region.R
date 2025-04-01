# Clear the workspace
rm(list = ls())

# Load datasets
datasets <- c("agriculture", "domestic", "gdppc", "gdppc_from1993", "industry", "info", "natural_disasters", 
              "population", "private_investment", "water_productivity", "water_stress", "withdrawals", 
              "available_resources", "gdp_ppp", "gdp_usd")
lapply(datasets, function(name) assign(name, readRDS(paste0(name, ".rds")), envir = .GlobalEnv))


# Load necessary packages
library(dplyr)
library(ggplot2)

# 1. Merge data on 'Country.Code' with appropriate filters
merged_data <- natural_disasters %>%
  inner_join(info, by = "Country.Code") %>%
  filter(!is.na(Region), !is.na(X2000))  # Remove rows where Region or X2000 are missing

# 2. Calculate total natural disasters by Region
region_disasters <- merged_data %>%
  group_by(Region) %>%
  summarise(total_disasters = sum(X2000, na.rm = TRUE))  # Sum of disasters per region, ignoring NAs

# 3. Plot a bar chart of natural disasters by Region
ggplot(region_disasters, aes(x = reorder(Region, -total_disasters), y = total_disasters, fill = Region)) +
  geom_bar(stat = "identity") +  # Bar plot with total disasters
  theme_minimal() +  # Minimal theme for the plot
  labs(
    title = "Natural Disasters by Region",  # Title of the plot
    x = "Region",  # X-axis label
    y = "Total Disasters"  # Y-axis label
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    plot.title = element_text(hjust = 0.5, size = 16),  # Center align title and set size
    legend.position = "none"  # Remove legend
  ) +
  scale_fill_brewer(palette = "Set3")  # Use a different color palette for regions