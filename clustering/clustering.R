# ================================
# 0. Setup and Data Loading
# ================================

# Set working directory (update path as needed)
setwd("C:/Users/SEC/Desktop/2024WS/projectR/dataset/filtered_dataset_optimized")

# Load all .rds files in the directory
rds_files <- list.files(pattern = "\\.rds$")
for (file in rds_files) {
  var_name <- tools::file_path_sans_ext(file)
  assign(var_name, readRDS(file))
}

# Load required libraries
library(tidyverse)
library(cluster)
library(factoextra)
library(clusterSim)
library(rnaturalearth)
library(rnaturalearthdata)

# Define year range used across PCA and clustering
YEAR_RANGE <- paste0("X", 2012:2021)

# Safely assign rownames to all target datasets
datasets <- c("gdp_usd", "available_resources", "water_productivity", "agriculture",
              "domestic", "gdppc", "population")

for (name in datasets) {
  df <- get(name)
  rownames(df) <- df$Country.Code
  assign(name, df, envir = .GlobalEnv)
}

# ================================
# 1. PCA: All 7 Normal Variables
# ================================

# Select and average 7 full normal indicators over YEAR_RANGE
normal_features <- list(
  gdp_usd              = gdp_usd[ , YEAR_RANGE],
  available_resources  = available_resources[ , YEAR_RANGE],
  water_productivity   = water_productivity[ , YEAR_RANGE],
  agriculture          = agriculture[ , YEAR_RANGE],
  domestic             = domestic[ , YEAR_RANGE],
  gdppc                = gdppc[ , YEAR_RANGE],
  population           = population[ , YEAR_RANGE]
)

pca_averaged_features <- purrr::map(normal_features, ~ rowMeans(.x, na.rm = TRUE)) %>% bind_cols()
pca_dataset <- data.frame(
  Country.Name = water_productivity$Country.Name,
  Country.Code = water_productivity$Country.Code,
  pca_averaged_features
)
rownames(pca_dataset) <- pca_dataset$Country.Code
pca_data <- pca_dataset[ , 3:ncol(pca_dataset)]

# Run and visualize PCA
run_pca <- function(data, title = "PCA Contribution Plot") {
  result <- prcomp(data, scale. = TRUE)
  print(fviz_pca_var(result, col.var = "contrib") + labs(title = title))
  return(result)
}

pca_result <- run_pca(pca_data, "PCA: All 7 Normal Variables")

# ================================
# 2. Clustering Preparation
# ================================

# Choose final variables for clustering (can be modified)
clustering_features <- list(
  available_resources  = available_resources[ , YEAR_RANGE],
  agriculture          = agriculture[ , YEAR_RANGE],
  domestic             = domestic[ , YEAR_RANGE]
)

# Average selected features
averaged_features <- purrr::map(clustering_features, ~ rowMeans(.x, na.rm = TRUE)) %>% bind_cols()
clustering_dataset <- data.frame(
  Country.Name = water_productivity$Country.Name,
  Country.Code = water_productivity$Country.Code,
  averaged_features
)
rownames(clustering_dataset) <- clustering_dataset$Country.Code
clustering_data <- clustering_dataset[ , 3:ncol(clustering_dataset)]

# Visualize PCA on final clustering variables
pca_result <- run_pca(clustering_data, "PCA: Final Clustering Variables")

# ================================
# 3. K-means Clustering
# ================================

# Standardize feature data
clustering_scaled <- scale(clustering_data)

# Determine optimal k using Elbow method
fviz_nbclust(clustering_scaled, kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal k")

# Determine optimal k using Silhouette method
fviz_nbclust(clustering_scaled, kmeans, method = "silhouette") +
  labs(title = "Silhouette Method for Optimal k")

# Run final K-means clustering
set.seed(123)
k <- 3
k_result <- kmeans(clustering_scaled, centers = k, nstart = 25)

# Visualize clusters
fviz_cluster(k_result, data = clustering_scaled,
             geom = "point", ellipse.type = "norm",
             main = "K-means Clustering Result")

# Append cluster labels to dataset
clustering_dataset$Cluster <- as.factor(k_result$cluster)

# Calculate average silhouette score
sil <- silhouette(k_result$cluster, dist(clustering_scaled))
mean(sil[, 3])  # Print silhouette score

# Calculate Davies-Bouldin Index
db_index <- index.DB(clustering_scaled, k_result$cluster)$DB
print(db_index)

# Cluster profile plot
# Average feature values per cluster (long format)
cluster_profiles <- clustering_dataset %>%
  group_by(Cluster) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = -Cluster, names_to = "Feature", values_to = "Mean")

# Bar plot
ggplot(cluster_profiles, aes(x = Feature, y = Mean, fill = Cluster)) +
  geom_col(position = "dodge") +
  labs(title = "Cluster Profiles", y = "Average Value") +
  theme_minimal()

# Geographic Cluster Map
world <- ne_countries(scale = "medium", returnclass = "sf")

world_clusters <- world %>%
  left_join(clustering_dataset, by = c("iso_a3" = "Country.Code"))

ggplot(world_clusters) +
  geom_sf(aes(fill = Cluster)) +
  labs(title = "Countries by Cluster") +
  theme_void()

# Save the final dataset with cluster assignments
setwd("C:/Users/SEC/Desktop/2024WS/projectR/dataset/clustering")
write.csv(clustering_dataset, "clustering_result.csv", row.names = FALSE)

# Save the kmeans model result (for reproducibility or later analysis)
saveRDS(k_result, "kmeans_result_k3.rds")
