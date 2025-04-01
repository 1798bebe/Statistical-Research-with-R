# Clear the workspace
rm(list = ls())

# Load datasets
rds_files <- list.files(pattern = "\\.rds$")
for(file in rds_files){
  var_name <- tools::file_path_sans_ext(file)
  assign(var_name, readRDS(file))
}


# Load necessary libraries
library(dplyr)
library(ggplot2)

# 1. Calculate average GDP per capita by Country.Code
gdppc_avg <- gdppc %>%
  group_by(Country.Code) %>%
  summarise(gdppc_avg = mean(c_across(starts_with("X")), na.rm = TRUE))

# 2. Merge gdppc average with water_stress data
water_stress_with_gdppc <- water_stress %>%
  left_join(gdppc_avg, by = "Country.Code")

# 3. Calculate average water stress value for each year
water_stress_with_gdppc <- water_stress_with_gdppc %>%
  mutate(water_stress_avg = rowMeans(select(., starts_with("X1994"):starts_with("X2021")), na.rm = TRUE))

# 4. Divide gdppc into 5 groups using quantiles
gdppc_quantiles <- quantile(water_stress_with_gdppc$gdppc_avg, probs = 0:5 / 5, na.rm = TRUE)

# 5. Assign countries to GDP per capita groups
water_stress_with_gdppc <- water_stress_with_gdppc %>%
  mutate(gdppc_group = cut(gdppc_avg, breaks = gdppc_quantiles, include.lowest = TRUE, 
                           labels = c("Low", "Lower-Mid", "Mid", "Upper-Mid", "High")))

# 6. Create a boxplot of water stress by GDP per capita group
ggplot(water_stress_with_gdppc, aes(x = gdppc_group, y = water_stress_avg)) +
  geom_boxplot(width = 0.6) +
  labs(
    title = "Water Stress by GDP per Capita Group (Log Scale)", 
    x = "GDP per Capita Group",
    y = "Average Water Stress (Log Scale)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(10, 20, 10, 10),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_y_log10()  # Log scale for y-axis