# 1. Merge the domestic, agriculture, and industry datasets with the population data

# Load required libraries
library(dplyr)
library(tidyr)

# Assuming the dataframes for domestic, agriculture, industry, and population are named accordingly
# Ensure that the 'Year' columns are converted to numeric and handled correctly
# Reshaping population data into long format and calculating country-specific average population
population_long <- population %>%
  pivot_longer(cols = starts_with("X"), names_to = "Year", values_to = "Population") %>%
  mutate(Year = as.numeric(gsub("X", "", Year))) %>%
  group_by(Country.Code) %>%
  summarise(average_population = mean(Population, na.rm = TRUE))

# Now, merge population data with the domestic, agriculture, and industry data
merged_domestic <- domestic %>%
  pivot_longer(cols = starts_with("X"), names_to = "Year", values_to = "Value") %>%
  mutate(Year = as.numeric(gsub("X", "", Year)))

merged_agriculture <- agriculture %>%
  pivot_longer(cols = starts_with("X"), names_to = "Year", values_to = "Value") %>%
  mutate(Year = as.numeric(gsub("X", "", Year)))

merged_industry <- industry %>%
  pivot_longer(cols = starts_with("X"), names_to = "Year", values_to = "Value") %>%
  mutate(Year = as.numeric(gsub("X", "", Year)))

# Merging population data with each dataset
merged_domestic_with_population <- merged_domestic %>%
  left_join(population_long, by = "Country.Code")

merged_agriculture_with_population <- merged_agriculture %>%
  left_join(population_long, by = "Country.Code")

merged_industry_with_population <- merged_industry %>%
  left_join(population_long, by = "Country.Code")

#2. Divide countries into 5 population groups

# Define population groups
population_groups <- c("Very Low", "Low", "Average", "High", "Very High")
population_cutoffs <- quantile(merged_domestic_with_population$average_population, probs = 0:5 / 5, na.rm = TRUE)

# Create population group variable
merged_domestic_with_population <- merged_domestic_with_population %>%
  mutate(PopulationGroup = cut(average_population, breaks = population_cutoffs, labels = population_groups, include.lowest = TRUE))

merged_agriculture_with_population <- merged_agriculture_with_population %>%
  mutate(PopulationGroup = cut(average_population, breaks = population_cutoffs, labels = population_groups, include.lowest = TRUE))

merged_industry_with_population <- merged_industry_with_population %>%
  mutate(PopulationGroup = cut(average_population, breaks = population_cutoffs, labels = population_groups, include.lowest = TRUE))

#3. Calculate the total average usage by population group and year

# Calculate total average usage by PopulationGroup and Year for each dataset
average_domestic_population_group <- merged_domestic_with_population %>%
  group_by(PopulationGroup, Year) %>%
  summarise(average_domestic_usage = mean(Value, na.rm = TRUE), .groups = "drop")

average_agriculture_population_group <- merged_agriculture_with_population %>%
  group_by(PopulationGroup, Year) %>%
  summarise(average_agriculture_usage = mean(Value, na.rm = TRUE), .groups = "drop")

average_industry_population_group <- merged_industry_with_population %>%
  group_by(PopulationGroup, Year) %>%
  summarise(average_industry_usage = mean(Value, na.rm = TRUE), .groups = "drop")

#4. Plot the graphs (IncomeGroup and Region)
# Load ggplot2
library(ggplot2)

# Plot for IncomeGroup - Domestic Usage
ggplot(average_domestic_population_group, aes(x = Year, y = average_domestic_usage, color = PopulationGroup)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(1994, 2021, by = 1)) +
  ggtitle("Domestic Usage by Population Group") +
  xlab("Year") +
  ylab("Average Domestic Usage") +
  theme_minimal() + 
  theme(
    legend.position = "bottom", 
    legend.justification = "center",
    plot.title = element_text(hjust = 0.5)  
  ) +
  guides(color = guide_legend(nrow = 1)) 

# Plot for IncomeGroup - Agriculture Usage
ggplot(average_agriculture_population_group, aes(x = Year, y = average_agriculture_usage, color = PopulationGroup)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(1994, 2021, by = 1)) +
  ggtitle("Agriculture Usage by Population Group") +
  xlab("Year") +
  ylab("Average Agriculture Usage") +
  theme_minimal() +  
  theme(
    legend.position = "bottom", 
    legend.justification = "center",
    plot.title = element_text(hjust = 0.5)  
  ) +
  guides(color = guide_legend(nrow = 1)) 

# Plot for IncomeGroup - Industry Usage
ggplot(average_industry_population_group, aes(x = Year, y = average_industry_usage, color = PopulationGroup)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(1994, 2021, by = 1)) +
  ggtitle("Industry Usage by Population Group") +
  xlab("Year") +
  ylab("Average Industry Usage") +
  theme_minimal() +  
  theme(
    legend.position = "bottom", 
    legend.justification = "center",
    plot.title = element_text(hjust = 0.5)  
  ) +
  guides(color = guide_legend(nrow = 1)) 
