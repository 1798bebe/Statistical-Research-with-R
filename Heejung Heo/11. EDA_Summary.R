rm(list = ls())

# Load datasets
data_list <- c("agriculture", "domestic", "gdppc", "gdppc_from1993", "industry", "info", "natural_disasters", 
               "population", "private_investment", "water_productivity", "water_stress", "withdrawals", 
               "available_resources", "gdp_ppp", "gdp_usd")
lapply(data_list, function(x) assign(x, readRDS(paste0(x, ".rds")), envir = .GlobalEnv))


# Function to process and summarize data
process_data <- function(data, data_name) {
  # Merge with IncomeGroup information
  data <- left_join(data, info, by = "Country.Code")
  
  # Convert wide to long format
  data_long <- data %>% 
    pivot_longer(cols = starts_with("X"), names_to = "Year", values_to = "Value")
  
  # Calculate mean per IncomeGroup
  summary_data <- data_long %>%
    group_by(Country.Code, IncomeGroup) %>%
    summarise(Mean_Value = mean(Value, na.rm = TRUE)) %>%
    ungroup()
  
  # Compute summary statistics
  summary_stats <- summary_data %>%
    group_by(IncomeGroup) %>%
    summarise(
      Mean = mean(Mean_Value, na.rm = TRUE),
      Max = max(Mean_Value, na.rm = TRUE),
      Min = min(Mean_Value, na.rm = TRUE),
      Median = median(Mean_Value, na.rm = TRUE)
    )
  
  # Print results
  print(paste("Summary statistics for", data_name))
  print(summary_stats)
}

# Apply function to all datasets
process_data(domestic, "Domestic Water Usage")
process_data(agriculture, "Agricultural Water Usage")
process_data(industry, "Industrial Water Usage")
process_data(population, "Population")
process_data(withdrawals, "Freshwater Withdrawals")
process_data(water_stress, "Water Stress Ratio")
process_data(gdppc, "GDP per Capita")
process_data(water_productivity, "Water Productivity")
