# Clear environment
rm(list = ls())

# Load datasets
rds_files <- list.files(pattern = "\\.rds$")
for(file in rds_files){
  var_name <- tools::file_path_sans_ext(file)
  assign(var_name, readRDS(file))
}

# Load necessary libraries
library(tidyr)
library(dplyr)
library(ggplot2)

#1. Function to reshape and merge datasets
reshape_and_merge <- function(data, info) {
  data %>%
    left_join(info, by = "Country.Code") %>%
    mutate(Region = ifelse(is.na(Region), "Unknown", Region),
           IncomeGroup = ifelse(is.na(IncomeGroup), "Unknown", IncomeGroup)) %>%
    pivot_longer(cols = starts_with("X"),
                 names_to = "Year",
                 values_to = "Value") %>%
    mutate(Year = as.numeric(gsub("X", "", Year)))
}

#2. Reshape and merge all datasets
merged_domestic_long <- reshape_and_merge(domestic, info)
merged_agriculture_long <- reshape_and_merge(agriculture, info)
merged_industry_long <- reshape_and_merge(industry, info)

#3. Function to calculate averages
group_average <- function(data, group_var) {
  data %>%
    group_by(across(all_of(group_var)), Year) %>%
    summarise(Average_Usage = mean(Value, na.rm = TRUE), .groups = "drop")
}

#4. Calculate averages
average_domestic_income <- group_average(merged_domestic_long, "IncomeGroup")
average_agriculture_income <- group_average(merged_agriculture_long, "IncomeGroup")
average_industry_income <- group_average(merged_industry_long, "IncomeGroup")

average_domestic_region <- group_average(merged_domestic_long, "Region")
average_agriculture_region <- group_average(merged_agriculture_long, "Region")
average_industry_region <- group_average(merged_industry_long, "Region")

#5. Function to generate plots
plot_usage <- function(data, title, y_label, color_var) {
  ggplot(data, aes(x = Year, y = Average_Usage, color = .data[[color_var]])) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = seq(1994, 2021, by = 1)) +
    ggtitle(title) +
    xlab("Year") +
    ylab(y_label) +
    theme_minimal() +
    theme(
      legend.position = "bottom", 
      legend.justification = "center",
      plot.title = element_text(hjust = 0.5)
    ) +
    guides(color = guide_legend(nrow = 1))
}

#6. Generate and display plots
plot_usage(average_domestic_income, "Domestic Usage by Income Group", "Average Domestic Usage", "IncomeGroup")
plot_usage(average_agriculture_income, "Agriculture Usage by Income Group", "Average Agriculture Usage", "IncomeGroup")
plot_usage(average_industry_income, "Industry Usage by Income Group", "Average Industry Usage", "IncomeGroup")

plot_usage(average_domestic_region, "Domestic Usage by Region", "Average Domestic Usage", "Region")
plot_usage(average_agriculture_region, "Agriculture Usage by Region", "Average Agriculture Usage", "Region")
plot_usage(average_industry_region, "Industry Usage by Region", "Average Industry Usage", "Region")
