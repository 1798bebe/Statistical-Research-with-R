# Clear workspace
rm(list = ls())

# Load required libraries
packages <- c("dplyr", "tidyr", "ggplot2", "ggcorrplot")
lapply(packages, function(pkg) if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg, dependencies = TRUE))
lapply(packages, library, character.only = TRUE)

# Load datasets
datasets <- c("agriculture", "domestic", "gdppc", "gdppc_from1993", "industry", "info", "natural_disasters", 
              "population", "private_investment", "water_productivity", "water_stress", "withdrawals", 
              "available_resources", "gdp_ppp", "gdp_usd")
lapply(datasets, function(name) assign(name, readRDS(paste0(name, ".rds")), envir = .GlobalEnv))

# Function to reshape data into long format
reshape_data <- function(df, var_name) {
  df %>% pivot_longer(cols = starts_with("X"), names_to = "Year", values_to = var_name) %>%
    mutate(Year = as.numeric(sub("X", "", Year)))
}

# Reshape relevant datasets
pop_long <- reshape_data(population, "Population")
withd_long <- reshape_data(withdrawals, "Withdrawals")
stress_long <- reshape_data(water_stress, "Water_Stress")
gdppc_long <- reshape_data(gdppc, "GDPpc")
prod_long <- reshape_data(water_productivity, "Water_Productivity")

# Merge datasets
merged_data <- list(pop_long, withd_long, stress_long, gdppc_long, prod_long) %>%
  Reduce(function(df1, df2) left_join(df1, df2, by = c("Country.Name", "Country.Code", "Year")), .) %>%
  left_join(info, by = "Country.Code")

# Compute mean values by income group
group_avg <- merged_data %>%
  group_by(IncomeGroup) %>%
  summarise(across(c(Population, Withdrawals, Water_Stress, GDPpc, Water_Productivity), mean, na.rm = TRUE))

# Compute correlation by income group
correlation_results <- merged_data %>%
  group_by(IncomeGroup) %>%
  summarise(
    cor_pop_stress = cor(Population, Water_Stress, use = "complete.obs"),
    cor_pop_withd = cor(Population, Withdrawals, use = "complete.obs"),
    cor_pop_prod = cor(Population, Water_Productivity, use = "complete.obs"),
    cor_gdp_stress = cor(GDPpc, Water_Stress, use = "complete.obs"),
    cor_gdp_withd = cor(GDPpc, Withdrawals, use = "complete.obs"),
    cor_gdp_prod = cor(GDPpc, Water_Productivity, use = "complete.obs")
  )

# Correlation matrix visualization for High-income group
high_income_data <- merged_data %>% filter(IncomeGroup == "High income") %>%
  select(Population, Withdrawals, Water_Stress, GDPpc, Water_Productivity)

ggcorrplot(cor(high_income_data, use = "complete.obs"), lab = TRUE, title = "Correlation Matrix for High-income Group")