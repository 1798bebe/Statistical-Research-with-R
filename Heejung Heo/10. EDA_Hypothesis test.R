rm(list = ls())

# Load datasets
rds_files <- list.files(pattern = "\\.rds$")
for(file in rds_files){
  var_name <- tools::file_path_sans_ext(file)
  assign(var_name, readRDS(file))
}



# 1. Calculate the average of water_stress for each country (X1994 ~ X2021)
water_stress_avg <- water_stress %>%
  mutate(water_stress_avg = rowMeans(select(., X1994:X2021), na.rm = TRUE)) %>%
  select(Country.Code, water_stress_avg)  # Select only Country.Code and the average value

# 2. Extract Region information from the info dataset
# Extract only Country.Code and Region from the info dataset
info_region <- info %>%
  select(Country.Code, Region)

# 3. Merge the natural_disasters and info datasets to add Region
merged_data <- natural_disasters %>%
  inner_join(info_region, by = "Country.Code") %>%
  inner_join(water_stress_avg, by = "Country.Code") %>%
  filter(!is.na(X2000), !is.na(water_stress_avg))  # Exclude data where natural disaster or water stress average is missing

# 4. Perform correlation analysis
correlation_result <- cor.test(merged_data$X2000, merged_data$water_stress_avg)
print(correlation_result)

# 5. Data visualization (scatter plot)
# Install the scales package (if not installed)
# install.packages("scales")
library(scales)
library(ggplot2)

# Set number label format in ggplot graph
ggplot(merged_data, aes(x = X2000, y = water_stress_avg)) +
  geom_point(aes(color = Region), alpha = 0.7, size = 2) +  # Adjust point size and transparency
  geom_smooth(method = "lm", color = "red") +  # Add linear regression line
  scale_y_log10(labels = label_number(accuracy = 0.01)) +  # Apply log scale for y-axis with formatted labels
  labs(
    title = paste("Correlation between Natural Disasters and Water Stress (Log Scale for Water Stress)"),
    x = "Natural Disasters (X2000)",
    y = "Water Stress (Average) (Log Scale)",
    caption = paste("Correlation coefficient: ", round(correlation_result$estimate, 2),
                    "\nP-value: ", round(correlation_result$p.value, 3))
  ) + 
  theme_minimal()  # Use a minimal theme for cleaner visualization
