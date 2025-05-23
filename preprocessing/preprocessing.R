setwd("C:/Users/SEC/Desktop/2024WS/projectR/dataset")  # Please change this to your actual working directory. 
# setwd("U:/Eigene Dateien/research/dataset")
getwd

library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

population <- read.csv("population.csv", header = TRUE, skip = 4)
private_investment <- read.csv("private_investment.csv", header = TRUE, skip = 4)
annual_freshwater_withdrawals <- read.csv("annual_freshwater_withdrawals.csv", header = TRUE, skip = 4)
afwd_agriculture <- read.csv("annual_freshwater_withdrawals_agriculture.csv", header = TRUE, skip = 4)
afwd_domestic <- read.csv("annual_freshwater_withdrawals_domestic.csv", header = TRUE, skip = 4)
afwd_industry <- read.csv("annual_freshwater_withdrawals_industry.csv", header = TRUE, skip = 4)
water_stress <- read.csv("water_stress.csv", header = TRUE, skip = 4)
water_productivity <- read.csv("water_productivity.csv", header = TRUE, skip = 4)
natural_disasters <- read.csv("droughts_floods_extreme_temperatures.csv", header = TRUE, skip = 4)
gdp_per_capita <- read.csv("gdp_per_capita.csv", header = TRUE, skip = 4)
precipitation <- read.csv("precipitation.csv", header = TRUE, skip = 4)
additional_info <- read.csv("mc_gdp_per_capita.csv", header = TRUE) 
# IncomeGroup or Region from additional_info can be merged into other data frame based on matching Country.Code

# Function to calculate overall missing proportion 
calculate_missing <- function(data, meta_cols = 4) {
  sum(colSums(is.na(data[, (meta_cols + 1):(ncol(data) - 1)]))) / (nrow(data) * (ncol(data) - meta_cols - 1))
}

# Function to calculate missing proportion by column (year)
calculate_year_missing <- function(data, meta_cols = 4) {
  colSums(is.na(data[, (meta_cols + 1):(ncol(data) - 1)])) / nrow(data)
}

# Function to calculate missing proportion by row (country)
calculate_country_missing <- function(data, meta_cols = 4) {
  rowSums(is.na(data[, (meta_cols + 1):(ncol(data) - 1)])) / (ncol(data) - meta_cols - 1)
}

# list of all data except additional info 
alldata <- list(
  wp = water_productivity, ws = water_stress, fw = annual_freshwater_withdrawals,
  fwa = afwd_agriculture, fwd = afwd_domestic, fwi = afwd_industry,
  gdp = gdp_per_capita, pop = population, pri = private_investment, 
  natd = natural_disasters, prec = precipitation
)

missing_raw <- data.frame(
  Dataset = names(alldata),
  Missing_Proportion = sapply(alldata, function(df) round(calculate_missing(df) * 100, 2))
)

# Plot as horizontal bar chart
ggplot(missing_raw, aes(x = Missing_Proportion, y = reorder(Dataset, Missing_Proportion))) +
  geom_bar(stat = "identity", fill = "lightcoral") +
  geom_text(aes(label = paste0(Missing_Proportion, "%")), hjust = -0.2) +
  labs(title = "Missing Data Proportion Before Filtering",
       x = "Missing Data Proportion (%)",
       y = "alldata") +
  xlim(0, 100) +
  theme_minimal()
# Private Investment (97.62%) and Natural Disasters (99.01%) are almost entirely missing. They will be labeled 'problematic data'.

# Compute missing proportions per year 
missing_prop_year <- lapply(alldata, calculate_year_missing)

keys <- c("wp", "ws", "fw", "fwa", "fwd", "fwi", "gdp", "pop", "prec", "pri", "natd") # shorthand name for all datasets
years <- sub("^X", "", names(missing_prop_year[[1]]))  # all years

# Build the data frame dynamically
missing_per_year <- data.frame(
  Year = years,
  sapply(missing_prop_year[keys], function(x) x * 100)
)

rownames(missing_per_year) <- c(1:nrow(missing_per_year))

# Convert to long format for plotting
missing_per_year_long <- pivot_longer(missing_per_year, cols = -Year, names_to = "Dataset", values_to = "MissingPercent")

# Plot missing percentage by year for each dataset
ggplot(missing_per_year_long, aes(x = Year, y = MissingPercent, color = Dataset, group = Dataset)) +
  geom_line(size = 1) +
  labs(title = "Missing Data Percentage by Year Across All datasets",
       x = "Year", y = "Missing Data Percentage (%)") +
  theme_minimal()

# Compute missing percentages per country for all datasets
# Create the data frame dynamically
missing_prop_country <- lapply(alldata, calculate_country_missing)

missing_per_country <- data.frame(
  Country = water_productivity[ ,2],  # Country.Code of all countries
  sapply(missing_prop_country[keys], function(x) x * 100)
)

# Convert to long format for plotting
missing_per_country_long <- pivot_longer(missing_per_country, cols = -Country, names_to = "Dataset", values_to = "MissingPercent")

# Plot missing percentage by country for each dataset
ggplot(missing_per_country_long, aes(x = reorder(Country, -MissingPercent), y = MissingPercent, fill = Dataset)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Missing Data Percentage by Country Across All alldata",
       x = "Country", y = "Missing Data Percentage (%)") +
  theme_minimal()
# The missing values are concentrated on the earlier years and on certain countries.

# Define normal data whose overall missing rate weren't too high. 
normal_dataset <- list(
  wp = water_productivity, ws = water_stress, fw = annual_freshwater_withdrawals,
  fwa = afwd_agriculture, fwd = afwd_domestic, fwi = afwd_industry,
  gdp = gdp_per_capita, pop = population, prec = precipitation
)

# Define threshold for acceptable missing data per year 
year_threshold <- 0.55  # Allow years with up to 55% missing data

# Select valid years where the datasets meet the threshold
valid_years <- Reduce(intersect, lapply(
  missing_prop_year[names(normal_dataset)],
  function(x) names(x[x <= year_threshold])
))

# Define threshold for acceptable missing data per country 
country_threshold <- 0.57  # Allow countries with up to 57% missing data

# Select valid countries where the datasets meet the threshold
for (key in names(normal_dataset)) {
  names(missing_prop_country[[key]]) <- normal_dataset[[key]]$Country.Code
}

valid_countries <- Reduce(intersect, lapply(
  missing_prop_country[names(normal_dataset)],
  function(x) names(x[x <= country_threshold])
)) %>%
  intersect(water_productivity$Country.Code)

# Function to filter dataset based on valid years and countries
filter_dataset <- function(data, valid_countries, valid_years) {
  data %>%
    filter(Country.Code %in% valid_countries) %>%
    dplyr::select(1:4,all_of(valid_years))
}

# Apply filtering to the normal datasets
filtered_normal <- purrr::map(normal_dataset, filter_dataset, valid_countries = valid_countries, valid_years = valid_years)

# We use an alternative approach for the two problematic datasets.
# First, we label the private investment data. 
# For each country, test whether any year column is non-missing
# (i.e. has at least one observed value)
pri_df <- private_investment %>%
  mutate(
    has_data = if_else(
      rowSums(!is.na(select(., starts_with("X")))) > 0,
      1L,  # at least one non NA → label 1
      0L   # all NA → label 0
    )
  )

# Keep only the metadata + the new label, drop all X* year columns
pri_labelled <- pri_df %>%
  dplyr::select(Country.Name, Country.Code, Indicator.Name, Indicator.Code, has_data)

# Inspect results
table(pri_labelled$has_data)

# Filter pri_labelled to keep only countries in valid countries
pri_labelled_final <- pri_labelled %>%
  dplyr::filter(Country.Code %in% valid_countries)

# Verify the result
table(pri_labelled_final$has_data)

# Second, we filter the natural disaster data. 
# Natural disaster data has a pecularity. The average data of 1990-2009 is listed on the year 2009. 
# So we just extract the data of year 2009 
natural_disasters_filtered <- as.data.frame(natural_disasters[, c("Country.Name","Country.Code","Indicator.Name","Indicator.Code","X2009")]) 

# Rename the "2009" column to "2000" (midpoint year of 1990-2009)
colnames(natural_disasters_filtered)[5] <- "X2000"

natural_disasters_filtered_final <- natural_disasters_filtered %>%
  dplyr::filter(Country.Code %in% valid_countries)

# Imputation
natural_disasters_complete <- natural_disasters_filtered_final

nd <- natural_disasters_filtered_final %>%
  left_join(additional_info[ , c("Country.Code", "Region")], by = "Country.Code") %>%
  mutate(Region = as.factor(Region))

nd_model <- lm(X2000 ~ Region, data = nd, na.action = na.exclude)

nd$X2000_imputed <- ifelse(
  is.na(nd$X2000),
  predict(nd_model, newdata = nd),
  nd$X2000
)

natural_disasters_complete$X2000 <- nd$X2000_imputed

# Compute missingness of natural disasters
sum(is.na(natural_disasters_filtered$X2000)) / nrow(natural_disasters_filtered) * 100
sum(is.na(natural_disasters_filtered_final$X2000)) / nrow(natural_disasters_filtered_final) * 100
sum(is.na(natural_disasters_complete$X2000)) / nrow(natural_disasters_complete) * 100

filtered_problematic <- list(
  pri = pri_labelled_final,
  natd = natural_disasters_complete
)

# Function to compute overall missing proportion after filtering
calculate_missing_filtered <- function(data, meta_cols = 4) {
  100 * sum(is.na(data)) / (nrow(data) * (ncol(data) - meta_cols))
}

filtered_all <- c(filtered_normal, filtered_problematic)

missing_data <- data.frame(
  Dataset = names(filtered_all),
  Missing_Proportion = sapply(filtered_all, function(df) round(calculate_missing_filtered(df), 2))
)

print(missing_data)

# Fetch world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Filter to only valid countries
selected_countries <- world %>%
  filter(iso_a3 %in% valid_countries)

# Plot
ggplot(data = world) +
  geom_sf(fill = "gray90", color = "white") +
  geom_sf(data = selected_countries, fill = "steelblue", color = "black") +
  labs(
    title    = "Countries Retained After Missing-Data Filtering",
    subtitle = paste(length(valid_countries), "countries selected"),
    caption  = "Source: World Bank panel filtering"
  ) +
  theme_minimal()

# Count the percentage of retained data after missing data handling. 
# Helper to count total cells beyond the first 4 metadata columns and the last invalid column
count_cells_1 <- function(df) {
  nrow(df) * (ncol(df) - 5)
}

count_cells_2 <- function(df) {
  nrow(df) * (ncol(df) - 4)
}

# Cells before filtering 
cells_before <- sapply(alldata[order(names(alldata))], count_cells_1)

# Cells after filtering 
cells_after  <- sapply(filtered_all[order(names(filtered_all))], count_cells_2)

# Build a summary table
size_summary <- data.frame(
  Dataset        = names(cells_before),
  Cells_Before   = cells_before,
  Cells_After    = cells_after,
  Retained_Pct   = round(cells_after / cells_before * 100, 2)
)

print(size_summary)

# Helper to count non-NA cells beyond the first 4 columns
count_non_na <- function(df) {
  sum(!is.na(df[, 5:ncol(df)]))
}

# Non-NA counts before filtering (all “normal” + problematic inputs)
before_counts <- sapply(alldata[order(names(alldata))], count_non_na)

# Non-NA counts after filtering
after_counts <- sapply(filtered_all[order(names(filtered_all))], count_non_na)

# Combine into a summary
non_na_summary <- data.frame(
  Dataset       = names(before_counts),
  NonNA_Before  = before_counts,
  NonNA_After   = after_counts,
  Retained_Pct  = round(after_counts / before_counts * 100, 2)
)

print(non_na_summary)

# Time series plot of precipitation 
# Pivot precipitation to long format
prec_long <- filtered_normal$prec %>%
  select(Country.Name, Country.Code, all_of(valid_years)) %>%
  pivot_longer(
    cols      = starts_with("X"),
    names_to  = "Year",
    values_to = "Precipitation"
  ) %>%
  mutate(Year = as.integer(sub("X", "", Year)))

# Plot with one color per country
ggplot(prec_long, aes(x = Year, y = Precipitation, group = Country.Code, color = Country.Code)) +
  geom_line(alpha = 0.7) +
  guides(color = "none") +  # hide the legend if too large
  labs(
    title   = "Annual Precipitation for Filtered Countries",
    subtitle = "Each country in its own color",
    x       = "Year",
    y       = "Precipitation (mm)",
    caption = "Data: World Bank, filtered panel"
  ) +
  theme_minimal()

# Now the missing values of GDP per capita from 1993 will be filled using imputation by income group 
# because the global sea level data is available from 1993. 
# Extract columns where the year is ≥ 1993
year_cols <- gdp_per_capita %>%
  dplyr::select(starts_with("X")) %>%
  dplyr::select( which(as.numeric(sub("X", "", colnames(.))) >= 1993) )

# Combine with metadata columns
gdppc_from1993 <- bind_cols(gdp_per_capita[ ,1:4], year_cols)
100 * sum(is.na(gdppc_from1993))/(nrow(gdppc_from1993) * (ncol(gdppc_from1993) - 4)) # 8% missing rate. 

# Merge the income group value of additional information data into the gdp per capita from 1993 data. 
gdppc_from1993_merged <- merge(gdppc_from1993, additional_info[ ,c("Country.Code", "IncomeGroup")], by = "Country.Code")

# Identify GDP years (all columns starting with "X")
gdp_years <- colnames(gdppc_from1993_merged)[grepl("^X\\d{4}$", colnames(gdppc_from1993_merged))]

# Fill missing GDP values by income group
for (year in gdp_years) {
  gdppc_from1993_merged <- gdppc_from1993_merged %>%
    group_by(IncomeGroup) %>%
    mutate(!!year := ifelse(is.na(get(year)), mean(get(year), na.rm = TRUE), get(year)))
}

# Un-group data
gdppc_from1993_merged <- ungroup(gdppc_from1993_merged)
100 * sum(is.na(gdppc_from1993_merged[ ,-ncol(gdppc_from1993_merged)]))/(nrow(gdppc_from1993_merged) * (ncol(gdppc_from1993_merged) - 5)) # All data has been filled. 
# Imputing by income group is logical because countries within the same income group 
# tend to have similar economic structures, development levels, and spending patterns, making their GDP per capita values more comparable.
# Regression analysis of annual freshwater withdrawals by population will be done later. 

# Derived Feature Construction
# Reconstruct GDP in constant USD: GDP = water_productivity × withdrawals
gdp_usd <- data.frame(
  Country.Name   = filtered_normal$wp$Country.Name,
  Country.Code   = filtered_normal$wp$Country.Code,
  Indicator.Name = NA_character_,
  Indicator.Code = NA_character_,
  filtered_normal$wp[ , 5:ncol(filtered_normal$wp)] * filtered_normal$fw[ , 5:ncol(filtered_normal$fw)]
)

# Reconstruct available water resources: withdrawals / water_stress × 100
available_resources <- data.frame(
  Country.Name   = filtered_normal$fw$Country.Name,
  Country.Code   = filtered_normal$fw$Country.Code,
  Indicator.Name = NA_character_,
  Indicator.Code = NA_character_,
  (filtered_normal$fw[ , 5:ncol(filtered_normal$fw)] / filtered_normal$ws[ , 5:ncol(filtered_normal$ws)]) * 100
)

# Reconstruct GDP in PPP: gdppc × population
gdp_ppp <- data.frame(
  Country.Name   = filtered_normal$gdp$Country.Name,
  Country.Code   = filtered_normal$gdp$Country.Code,
  Indicator.Name = NA_character_,
  Indicator.Code = NA_character_,
  filtered_normal$gdp[ , 5:ncol(filtered_normal$gdp)] * filtered_normal$pop[ , 5:ncol(filtered_normal$pop)]
)

# Saving files 
# Custom filenames
save_targets <- list(
  gdp_usd = gdp_usd,
  gdp_ppp = gdp_ppp,
  available_resources = available_resources, 
  info = additional_info,
  gdppc_from1993 = gdppc_from1993_merged
)

# Combine all filtered datasets and additional files into one list
rds_outputs <- c(
  setNames(filtered_all, c(
    "water_productivity", "water_stress", "withdrawals",
    "agriculture", "domestic", "industry", "gdppc",  
    "population", "precipitation", "private_investment", "natural_disasters"
  )),
  save_targets
)

# Save all RDS files
invisible(lapply(names(rds_outputs), function(name) {
  saveRDS(rds_outputs[[name]], paste0(name, ".rds"))
}))
