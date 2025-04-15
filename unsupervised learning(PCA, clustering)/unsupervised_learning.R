# ================================
# 0. Setup: Working Directory, Libraries, and Constants
# ================================
setwd("C:/Users/SEC/Desktop/2024WS/projectR/dataset/filtered_dataset_finalized")

# Libraries
library(tidyverse)
library(cluster)
library(factoextra)
library(clusterSim)
library(dbscan)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(stringr)
library(scales)
library(gridExtra)

# ================================
# 1. Load and Preprocess Data
# ================================
rds_files <- list.files(pattern = "\\.rds$")
for (file in rds_files) {
  assign(tools::file_path_sans_ext(file), readRDS(file))
}
YEAR_RANGE <- paste0("X", 2012:2021)

# Prepare for consistent indexing and merging.
raw_feature_sources <- c("available_resources", "water_productivity", "industry",
                         "agriculture", "domestic", "precipitation", 
                         "water_stress", "withdrawals", "natural_disasters")

for (name in raw_feature_sources) {
  temp <- get(name)
  rownames(temp) <- temp$Country.Code
  assign(name, temp, envir = .GlobalEnv)
}

aggregated_data <- data.frame(
  available_resources = rowMeans(available_resources[, YEAR_RANGE], na.rm = TRUE),
  water_productivity  = rowMeans(water_productivity[, YEAR_RANGE], na.rm = TRUE),
  industry            = rowMeans(industry[, YEAR_RANGE], na.rm = TRUE),
  agriculture         = rowMeans(agriculture[, YEAR_RANGE], na.rm = TRUE),
  domestic            = rowMeans(domestic[, YEAR_RANGE], na.rm = TRUE),
  precipitation       = rowMeans(precipitation[, YEAR_RANGE], na.rm = TRUE),
  water_stress        = rowMeans(water_stress[, YEAR_RANGE], na.rm = TRUE),
  withdrawals         = rowMeans(withdrawals[, YEAR_RANGE], na.rm = TRUE), 
  natural_disasters   = rowMeans(natural_disasters[, YEAR_RANGE], na.rm = TRUE)
)

# Visualize the features by countries 
plot_list <- lapply(raw_feature_sources, function(feat) {
  ggplot(aggregated_data, aes_string(x = feat)) +
    geom_histogram(bins = 30, fill = "skyblue", color = "black") +
    labs(title = feat, x = NULL, y = "Count") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 12))
})

do.call(grid.arrange, c(plot_list, ncol = 3))

# PCA 
pca_all <- prcomp(scale(aggregated_data), scale. = FALSE)
pca_summary <- summary(pca_all)
print(pca_summary)

# ================================
# 2. Generate Feature Combinations
# ================================
feature_sets <- list()
combo_id <- 1
for (k in 2:length(raw_feature_sources)) {
  combos <- combn(raw_feature_sources, k, simplify = FALSE)
  for (combo in combos) {r
    feature_sets[[paste0("Set_", combo_id)]] <- combo
    combo_id <- combo_id + 1
  }
}

# ================================
# 3. Clustering Grid Search
# ================================
num_pc_options <- 2:8
epsilon_values <- seq(0.0, 2.0, by = 0.2)
minpts_values <- 2:8
cluster_counts <- 2:4
results <- data.frame()

for (features in feature_sets) {
  scaled_data <- scale(aggregated_data[, features, drop = FALSE]) # Normalization 
  
  for (n_pc in num_pc_options) {                 # Apply PCA to the scaled data and extract the first n_pc principal components
    pca <- prcomp(scaled_data, scale. = FALSE)
    if (n_pc > ncol(pca$x)) next
    reduced_data <- pca$x[, 1:n_pc, drop = FALSE]
    
    # DBSCAN
    for (eps in epsilon_values) {
      for (minpts in minpts_values) {
        db_result <- dbscan(reduced_data, eps = eps, minPts = minpts)
        labels <- db_result$cluster
        valid <- labels != 0
        sil_score <- if (length(unique(labels[valid])) >= 2) mean(silhouette(labels, dist(reduced_data))[, 3]) else NA
        dbi_score <- if (!is.na(sil_score)) index.DB(reduced_data[valid, ], labels[valid])$DB else NA
        
        results <- rbind(results, data.frame(
          Method = "DBSCAN",
          Feature_Set = paste(features, collapse = ", "),
          PCs = n_pc,
          Param = paste0("eps=", round(eps, 3), ", minPts=", minpts),
          Num_Clusters = length(unique(labels[valid])), # Only measure valid clusters, not noise
          Silhouette = sil_score,
          DB_Index = dbi_score,
          Labels = I(list(labels))
        ))
      }
    }
    
    # KMeans
    for (k in cluster_counts) {
      labels <- kmeans(reduced_data, centers = k, nstart = 25)$cluster
      sil_score <- mean(silhouette(labels, dist(reduced_data))[, 3])
      dbi_score <- index.DB(reduced_data, labels)$DB
      
      results <- rbind(results, data.frame(
        Method = "KMeans",
        Feature_Set = paste(features, collapse = ", "),
        PCs = n_pc,
        Param = k,
        Num_Clusters = length(unique(labels)),
        Silhouette = sil_score,
        DB_Index = dbi_score,
        Labels = I(list(labels))
      ))
    }
    
    # Hierarchical
    dist_mat <- dist(reduced_data)
    hclust_model <- hclust(dist_mat, method = "ward.D2")
    for (k in cluster_counts) {
      labels <- cutree(hclust_model, k = k)
      sil_score <- mean(silhouette(labels, dist_mat)[, 3])
      dbi_score <- index.DB(reduced_data, labels)$DB
      
      results <- rbind(results, data.frame(
        Method = "Hierarchical",
        Feature_Set = paste(features, collapse = ", "),
        PCs = n_pc,
        Param = k,
        Num_Clusters = length(unique(labels)),
        Silhouette = sil_score,
        DB_Index = dbi_score,
        Labels = I(list(labels))
      ))
    }
  }
}

write.csv(results, "clustering_results.csv", row.names = FALSE)

# ================================
# 4. Evaluate and Visualize Best Clustering Result
# ================================
# Filter rows with valid Silhouette, DBI and Number of Clusters 
results_clean <- results %>%
  filter(
    is.finite(Silhouette),
    is.finite(DB_Index),
    !is.null(Labels),
    Num_Clusters >= 2, 
    Num_Clusters <= 4
  )

# Compute Max-Min Ratio for cluster size balance
results_clean$MaxMinRatio <- sapply(results_clean$Labels, function(lbls) {
  sizes <- table(lbls[lbls != 0])  # Exclude noise (label 0)
  if (length(sizes) <= 1) return(NA)
  max(sizes) / min(sizes)
})

# Compute proportion of missing or noise labels (label == 0)
results_clean$MissingRate <- sapply(results_clean$Labels, function(lbls) {
  mean(lbls == 0 | is.na(lbls))
})

# Normalize Silhouette and DBI, then compute composite score
results_clean$Silhouette_Norm <- rescale(results_clean$Silhouette, to = c(0, 1))
results_clean$DBI_Norm_Inv <- 1 - rescale(results_clean$DB_Index, to = c(0, 1))
results_clean$CompositeScore <- 0.5 * results_clean$Silhouette_Norm + 0.5 * results_clean$DBI_Norm_Inv

# Filter well-balanced results with missing rate 20% at most
balanced_results <- results_clean %>%
  filter(!is.na(MaxMinRatio), MaxMinRatio < 10, MissingRate <= 0.20)

# Select top configurations
sorted_results <- balanced_results %>%
  arrange(desc(CompositeScore))

print(sorted_results)

# Extract the best configuration
best_result <- sorted_results[1, ]
best_features <- str_split(best_result$Feature_Set, ",\\s*")[[1]]

# Print out the metrics
cat("Best Clustering Configuration:\n")
cat("Silhouette Score:", round(best_result$Silhouette, 4), "\n")
cat("DB Index:", round(best_result$DB_Index, 4), "\n")
cat("MaxMinRatio:", round(best_result$MaxMinRatio, 2), "\n")
cat("Missing Label Rate:", round(best_result$MissingRate * 100, 2), "%\n")

# Perform PCA on selected features
scaled_input <- scale(aggregated_data[, best_features, drop = FALSE])
pca_best <- prcomp(scaled_input, scale. = FALSE)
reduced_best <- pca_best$x[, 1:best_result$PCs, drop = FALSE]
cluster_labels <- unlist(best_result$Labels)

# Final 2D PCA Biplot with Clusters and Arrows
fviz_pca_biplot(
  pca_best,
  axes = c(1, 2),
  geom.ind = "point",
  col.ind = as.factor(cluster_labels),
  pointshape = 21,
  palette = "jco",
  addEllipses = TRUE,
  label = "var",        # show arrows only
  col.var = "black",    # arrow color
  repel = TRUE,
  legend.title = "Cluster"
) +
  labs(title = paste("PCA Biplot with Clusters -", best_result$Method))

# Prepare map data
country_clusters <- data.frame(
  Country.Code = rownames(aggregated_data),
  Cluster = as.factor(cluster_labels)
)

world <- ne_countries(scale = "medium", returnclass = "sf")

map_df <- left_join(world, country_clusters, by = c("iso_a3" = "Country.Code")) %>%
  mutate(
    Cluster = as.factor(Cluster)  # Don't convert 0 to NA
  )

# Define custom colors for clusters 
custom_colors <- c(
  "0" = "#90FE10",   
  "1" = "#FFD700",   
  "2" = "#1E90FF"    
)

# Plot world map of clusters with manual colors
ggplot(map_df) +
  geom_sf(aes(fill = Cluster), color = "gray80", size = 0.1) +
  scale_fill_manual(
    values = custom_colors,
    name = "Cluster"
  ) +
  labs(
    title = paste("World Map -", best_result$Method, "Clustering")
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

# ================================
# 5. Cluster Profile
# ================================
# Step 1: Compute cluster summary using original (non-log) values
cluster_data <- data.frame(
  Cluster = as.factor(cluster_labels),
  aggregated_data[rownames(aggregated_data) %in% rownames(scaled_input), best_features, drop = FALSE]
)

cluster_summary <- cluster_data %>%
  group_by(Cluster) %>%
  summarise(across(everything(), mean, na.rm = TRUE), .groups = "drop")

# Step 2: For plotting, apply log10 transformation (+1) to the values
log_cluster_summary <- cluster_summary %>%
  mutate(across(-Cluster, ~log10(.x + 1))) %>%
  pivot_longer(-Cluster, names_to = "Feature", values_to = "Mean")

# Step 3: Plot using log-transformed values
ggplot(log_cluster_summary, aes(x = Feature, y = Mean, fill = Cluster)) +
  geom_col(position = "dodge") +
  labs(title = "Cluster Profile (log10 transformed for visualization)", 
       y = "Log10(Mean + 1)", x = "Feature") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# ================================
# 6. Save files regarding the best result
# ================================
# Save cluster labels with country codes
write.csv(country_clusters, "best_cluster_labels.csv", row.names = FALSE)

# Save PCA object and cluster assignments
saveRDS(pca_best, "best_pca_model.rds")
saveRDS(best_result, "best_clustering_config.rds")

# Save clustered and reduced PCA data
clustered_data <- data.frame(reduced_best, Cluster = cluster_labels)
write.csv(clustered_data, "best_clustered_data.csv", row.names = TRUE)
