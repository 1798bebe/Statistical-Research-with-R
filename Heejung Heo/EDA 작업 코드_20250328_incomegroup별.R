rm(list = ls())

agriculture <- readRDS("agriculture.rds")
domestic <- readRDS("domestic.rds")
gdppc <- readRDS("gdppc.rds")
gdppc_from1993 <- readRDS("gdppc_from1993.rds")
industry <- readRDS("industry.rds")
info <- readRDS("info.rds")
natural_disasters <- readRDS("natural_disasters.rds")
population <- readRDS("population.rds")
private_investment <- readRDS("private_investment.rds")
water_productivity <- readRDS("water_productivity.rds")
water_stress <- readRDS("water_stress.rds")
withdrawals <- readRDS("withdrawals.rds")
available_resources <- readRDS("available_resources.rds")
gdp_ppp <- readRDS("gdp_ppp.rds")
gdp_usd <- readRDS("gdp_usd.rds")

#1. Reshaping the Data
# Load necessary libraries
library(tidyr)
library(dplyr)

# Reshape the data to long format for each dataset (domestic, agriculture, industry)
merged_domestic_long <- merged_domestic %>%
  pivot_longer(cols = starts_with("X"), 
               names_to = "Year", 
               values_to = "Value") %>%
  mutate(Year = as.numeric(gsub("X", "", Year)))  # Remove the "X" and convert to numeric

merged_agriculture_long <- merged_agriculture %>%
  pivot_longer(cols = starts_with("X"), 
               names_to = "Year", 
               values_to = "Value") %>%
  mutate(Year = as.numeric(gsub("X", "", Year)))

merged_industry_long <- merged_industry %>%
  pivot_longer(cols = starts_with("X"), 
               names_to = "Year", 
               values_to = "Value") %>%
  mutate(Year = as.numeric(gsub("X", "", Year)))


#2.  Merging the domestic, agriculture, and industry 
#datasets with info to add IncomeGroup and Region by 
#Country.Code

# Assuming your data frames are named domestic, agriculture, industry, and info

# Load necessary libraries
library(dplyr)

# Merge data by Country.Code
merged_domestic <- domestic %>%
  left_join(info, by = "Country.Code")

merged_agriculture <- agriculture %>%
  left_join(info, by = "Country.Code")

merged_industry <- industry %>%
  left_join(info, by = "Country.Code")

# Check if there are any missing values in the "Region" and "IncomeGroup" columns
merged_domestic <- merged_domestic %>%
  mutate(Region = ifelse(is.na(Region), "Unknown", Region),
         IncomeGroup = ifelse(is.na(IncomeGroup), "Unknown", IncomeGroup))

merged_agriculture <- merged_agriculture %>%
  mutate(Region = ifelse(is.na(Region), "Unknown", Region),
         IncomeGroup = ifelse(is.na(IncomeGroup), "Unknown", IncomeGroup))

merged_industry <- merged_industry %>%
  mutate(Region = ifelse(is.na(Region), "Unknown", Region),
         IncomeGroup = ifelse(is.na(IncomeGroup), "Unknown", IncomeGroup))


#3 Calculating Average Usage by IncomeGroup for each Year

# Calculate average usage by IncomeGroup for each year
average_domestic_income_group <- merged_domestic_long %>%
  group_by(IncomeGroup, Year) %>%
  summarise(average_domestic_usage = mean(Value, na.rm = TRUE))

average_agriculture_income_group <- merged_agriculture_long %>%
  group_by(IncomeGroup, Year) %>%
  summarise(average_agriculture_usage = mean(Value, na.rm = TRUE))

average_industry_income_group <- merged_industry_long %>%
  group_by(IncomeGroup, Year) %>%
  summarise(average_industry_usage = mean(Value, na.rm = TRUE))

#4. Calculating Average Usage by Region for each Year

# Calculate average usage by Region for each year
average_domestic_region <- merged_domestic_long %>%
  group_by(Region, Year) %>%
  summarise(average_domestic_usage = mean(Value, na.rm = TRUE))

average_agriculture_region <- merged_agriculture_long %>%
  group_by(Region, Year) %>%
  summarise(average_agriculture_usage = mean(Value, na.rm = TRUE))

average_industry_region <- merged_industry_long %>%
  group_by(Region, Year) %>%
  summarise(average_industry_usage = mean(Value, na.rm = TRUE))


#5.  Plotting the Graphs

# Load ggplot2 for plotting
library(ggplot2)

# Plot by IncomeGroup
ggplot(average_domestic_income_group, aes(x = Year, y = average_domestic_usage, color = IncomeGroup)) +
  geom_line() +  
  geom_point() +  
  scale_x_continuous(breaks = seq(1994, 2021, by = 1)) +  
  ggtitle("Domestic Usage by Income Group") +
  xlab("Year") +
  ylab("Average Domestic Usage") +
  theme_minimal() +
  theme(
    legend.position = "bottom", 
    legend.justification = "center",
    plot.title = element_text(hjust = 0.5)  
  ) +
  guides(color = guide_legend(nrow = 1))

ggplot(average_agriculture_income_group, aes(x = Year, y = average_agriculture_usage, color = IncomeGroup)) +
  geom_line() +  
  geom_point() +  
  scale_x_continuous(breaks = seq(1994, 2021, by = 1)) +  
  ggtitle("Agriculture Usage by Income Group") +
  xlab("Year") +
  ylab("Average Agriculture Usage") +
  theme_minimal() +  
  theme(
    legend.position = "bottom", 
    legend.justification = "center",
    plot.title = element_text(hjust = 0.5)  
  ) +
  guides(color = guide_legend(nrow = 1))  


ggplot(average_industry_income_group, aes(x = Year, y = average_industry_usage, color = IncomeGroup)) +
  geom_line() +  # Line plot
  geom_point() +  # Add dots for each year
  scale_x_continuous(breaks = seq(1994, 2021, by = 1)) +  # Ensure all years are shown
  ggtitle("Industry Usage by Income Group") +
  xlab("Year") +
  ylab("Average Industry Usage") +
  theme_minimal() + 
  theme(
    legend.position = "bottom", 
    legend.justification = "center",
    plot.title = element_text(hjust = 0.5)  
  ) +
  guides(color = guide_legend(nrow = 1))  

# Plot by Region with dots and all years shown
ggplot(average_domestic_region, aes(x = Year, y = average_domestic_usage, color = Region)) +
  geom_line() +  # Line plot
  geom_point() +  # Add dots for each year
  scale_x_continuous(breaks = seq(1994, 2021, by = 1)) +  # Ensure all years are shown
  ggtitle("Domestic Usage by Region") +
  xlab("Year") +
  ylab("Average Domestic Usage") +
  theme_minimal() +  
  theme(
    legend.position = "bottom", 
    legend.justification = "center",
    plot.title = element_text(hjust = 0.5)  
  ) +
  guides(color = guide_legend(nrow = 1))  

ggplot(average_agriculture_region, aes(x = Year, y = average_agriculture_usage, color = Region)) +
  geom_line() +  # Line plot
  geom_point() +  # Add dots for each year
  scale_x_continuous(breaks = seq(1994, 2021, by = 1)) +  # Ensure all years are shown
  ggtitle("Agriculture Usage by Region") +
  xlab("Year") +
  ylab("Average Agriculture Usage") +
  theme_minimal() + 
  theme(
    legend.position = "bottom", 
    legend.justification = "center",
    plot.title = element_text(hjust = 0.5) 
  ) +
  guides(color = guide_legend(nrow = 1))  

ggplot(average_industry_region, aes(x = Year, y = average_industry_usage, color = Region)) +
  geom_line() +  # Line plot
  geom_point() +  # Add dots for each year
  scale_x_continuous(breaks = seq(1994, 2021, by = 1)) +  # Ensure all years are shown
  ggtitle("Industry Usage by Region") +
  xlab("Year") +
  ylab("Average Industry Usage") +
  theme_minimal() +  
  theme(
    legend.position = "bottom", 
    legend.justification = "center",
    plot.title = element_text(hjust = 0.5)  
  ) +
  guides(color = guide_legend(nrow = 1))  