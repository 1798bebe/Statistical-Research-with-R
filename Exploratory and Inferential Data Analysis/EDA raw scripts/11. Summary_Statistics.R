# Clear environment
rm(list = ls())

# Load datasets
rds_files <- list.files(pattern = "\\.rds$")
for(file in rds_files){
  var_name <- tools::file_path_sans_ext(file)
  assign(var_name, readRDS(file))
}

#1. Function to process and summarize data
process_data <- function(data, data_name) {
  data <- left_join(data, info, by = "Country.Code") # Merge with IncomeGroup information
  
  data_long <- data %>% 
    pivot_longer(cols = starts_with("X"), names_to = "Year", values_to = "Value") # Convert wide to long format

    summary_data <- data_long %>%
    group_by(Country.Code, IncomeGroup) %>%
    summarise(Mean_Value = mean(Value, na.rm = TRUE)) %>%
    ungroup()  # Calculate mean per IncomeGroup
  
  summary_stats <- summary_data %>%
    group_by(IncomeGroup) %>%
    summarise(
      Mean = mean(Mean_Value, na.rm = TRUE),
      Max = max(Mean_Value, na.rm = TRUE),
      Min = min(Mean_Value, na.rm = TRUE),
      Median = median(Mean_Value, na.rm = TRUE)
    )  # Compute summary statistics
  
  print(paste("Summary statistics for", data_name))
  print(summary_stats)
}

#2. Apply function to all datasets
process_data(domestic, "Domestic Water Usage")
process_data(agriculture, "Agricultural Water Usage")
process_data(industry, "Industrial Water Usage")
process_data(population, "Population")
process_data(withdrawals, "Freshwater Withdrawals")
process_data(water_stress, "Water Stress Ratio")
process_data(gdppc, "GDP per Capita")
process_data(water_productivity, "Water Productivity")
