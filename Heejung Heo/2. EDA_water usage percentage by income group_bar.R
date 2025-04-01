rm(list = ls())

# Load necessary packages
library(dplyr)
library(tidyr)
library(ggplot2)

# Load data
files <- c("agriculture", "domestic", "gdppc", "gdppc_from1993", "industry", "info", 
           "natural_disasters", "population", "private_investment", "water_productivity", 
           "water_stress", "withdrawals", "available_resources", "gdp_ppp", "gdp_usd")

for (file in files) {
  assign(file, readRDS(paste0(file, ".rds")))
}

# Function to reshape and merge data with income group information
reshape_and_merge <- function(data, category_name, info_data) {
  data %>%
    pivot_longer(cols = starts_with("X"), names_to = "Year", values_to = "Value") %>%
    mutate(Year = as.numeric(sub("X", "", Year)), Category = category_name) %>%
    left_join(info_data, by = "Country.Code") %>%
    mutate(IncomeGroup = replace_na(IncomeGroup, "Unknown"))
}

# Transform and merge datasets
data_categories <- list(Domestic = domestic, Agriculture = agriculture, Industry = industry)
combined_data <- bind_rows(lapply(names(data_categories), function(cat) 
  reshape_and_merge(data_categories[[cat]], cat, info)))

# Calculate average usage per IncomeGroup and Year
average_usage_income_group <- combined_data %>%
  group_by(IncomeGroup, Year, Category) %>%
  summarise(average_usage = mean(Value, na.rm = TRUE), .groups = "drop")

# Function to plot income group trends
plot_income_group_trend <- function(data, income_group_name) {
  income_data <- filter(data, IncomeGroup == income_group_name)
  if (nrow(income_data) == 0) {
    message(paste("No data found for IncomeGroup:", income_group_name))
    return(NULL)
  }
  ggplot(income_data, aes(x = Year, y = average_usage, fill = Category)) +
    geom_bar(stat = "identity", position = "stack", width = 0.5) +  
    scale_fill_manual(values = c("Domestic" = "#A6CEE3", "Agriculture" = "#1F77B4", "Industry" = "#004B8C")) + 
    scale_x_continuous(breaks = seq(min(income_data$Year), max(income_data$Year), by = 1)) +  
    theme_minimal() +
    labs(title = paste(income_group_name, "- Annual Average Usage: Domestic, Agriculture, Industry"),
         x = "Year", y = "Average Usage", fill = "Category") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5),
          legend.position = "bottom")
}

# Example Execution
plot_income_group_trend(average_usage_income_group, "Upper middle income")