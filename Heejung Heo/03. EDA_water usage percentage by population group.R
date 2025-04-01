# Clear environment
rm(list = ls())

# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Load datasets
rds_files <- list.files(pattern = "\\.rds$")
for(file in rds_files){
  var_name <- tools::file_path_sans_ext(file)
  assign(var_name, readRDS(file))
}

# Reshape population data and calculate average
population_long <- population %>%
  pivot_longer(cols = starts_with("X"), names_to = "Year", values_to = "Population") %>%
  mutate(Year = as.numeric(sub("X", "", Year))) %>%
  group_by(Country.Code) %>%
  summarise(average_population = mean(Population, na.rm = TRUE))

# Function to merge and reshape datasets
reshape_and_merge <- function(df) {
  df %>%
    pivot_longer(cols = starts_with("X"), names_to = "Year", values_to = "Value") %>%
    mutate(Year = as.numeric(sub("X", "", Year))) %>%
    left_join(population_long, by = "Country.Code")
}

# Apply function to datasets
domestic_merged <- reshape_and_merge(domestic)
agriculture_merged <- reshape_and_merge(agriculture)
industry_merged <- reshape_and_merge(industry)

# Define population groups
population_cutoffs <- quantile(domestic_merged$average_population, probs = seq(0, 1, 0.2), na.rm = TRUE)
population_groups <- c("Very Low", "Low", "Average", "High", "Very High")

# Assign population groups
assign_population_group <- function(df) {
  df %>% mutate(PopulationGroup = cut(average_population, breaks = population_cutoffs, labels = population_groups, include.lowest = TRUE))
}

domestic_grouped <- assign_population_group(domestic_merged)
agriculture_grouped <- assign_population_group(agriculture_merged)
industry_grouped <- assign_population_group(industry_merged)

# Calculate average usage by population group and year
calculate_average_usage <- function(df, usage_col) {
  df %>%
    group_by(PopulationGroup, Year) %>%
    summarise(!!usage_col := mean(Value, na.rm = TRUE), .groups = "drop")
}

domestic_avg <- calculate_average_usage(domestic_grouped, "average_domestic_usage")
agriculture_avg <- calculate_average_usage(agriculture_grouped, "average_agriculture_usage")
industry_avg <- calculate_average_usage(industry_grouped, "average_industry_usage")

# Function to generate plots
plot_usage <- function(data, y_col, title, y_label) {
  ggplot(data, aes(x = Year, y = !!sym(y_col), color = PopulationGroup)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = seq(1994, 2021, by = 1)) +
    ggtitle(title) +
    xlab("Year") +
    ylab(y_label) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.justification = "center", plot.title = element_text(hjust = 0.5)) +
    guides(color = guide_legend(nrow = 1))
}

# Generate and display plots
plot_usage(domestic_avg, "average_domestic_usage", "Domestic Usage by Population Group", "Average Domestic Usage")
plot_usage(agriculture_avg, "average_agriculture_usage", "Agriculture Usage by Population Group", "Average Agriculture Usage")
plot_usage(industry_avg, "average_industry_usage", "Industry Usage by Population Group", "Average Industry Usage")
