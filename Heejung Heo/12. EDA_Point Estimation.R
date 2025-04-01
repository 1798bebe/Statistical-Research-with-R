# Clear workspace
rm(list = ls())


# Load datasets
rds_files <- list.files(pattern = "\\.rds$")
for(file in rds_files){
  var_name <- tools::file_path_sans_ext(file)
  assign(var_name, readRDS(file))
}

library(dplyr)
library(tidyr)

# Load required libraries
library(dplyr)
library(tidyr)

# 1. Merge 'info' with other datasets (domestic, agriculture, industry, water_stress)
merge_with_info <- function(df) {
  df %>%
    left_join(info, by = "Country.Code")
}

domestic_with_info <- merge_with_info(domestic)
agriculture_with_info <- merge_with_info(agriculture)
industry_with_info <- merge_with_info(industry)
water_stress_with_info <- merge_with_info(water_stress)

# 2. Calculate average for each year by IncomeGroup (e.g., for domestic data)
calculate_avg <- function(df) {
  df %>%
    select(IncomeGroup, starts_with("X")) %>%
    gather(key = "Year", value = "Value", -IncomeGroup) %>%
    group_by(IncomeGroup, Year) %>%
    summarise(Year_Avg = mean(Value, na.rm = TRUE)) %>%
    ungroup()  # Remove grouping after calculation
}

# Apply the calculate_avg function to each dataset
avg_by_income_group <- calculate_avg(domestic_with_info)
avg_agriculture <- calculate_avg(agriculture_with_info)
avg_industry <- calculate_avg(industry_with_info)
avg_water_stress <- calculate_avg(water_stress_with_info)

# 3. Calculate the number of countries for each IncomeGroup
income_group_count <- domestic_with_info %>%
  group_by(IncomeGroup) %>%
  summarise(Count = n_distinct(Country.Code))

# 4. Calculate the overall average (weighted by the number of countries in each IncomeGroup)
avg_by_year <- avg_by_income_group %>%
  left_join(income_group_count, by = "IncomeGroup") %>%
  group_by(Year) %>%
  summarise(Overall_Avg = weighted.mean(Year_Avg, Count, na.rm = TRUE)) %>%
  ungroup()

# 5. Merge the IncomeGroup-level and overall averages into a final table
final_table <- avg_by_income_group %>%
  left_join(avg_by_year, by = "Year") %>%
  select(Year, IncomeGroup, Year_Avg, Overall_Avg) %>%
  arrange(Year, IncomeGroup)  # Ensure the table is sorted by Year and IncomeGroup

# 6. Print the result (Display up to 200 rows)
print(final_table, n = 200)  # Print the first 200 rows to the console