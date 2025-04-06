# ================================
# 0. Setup and Data Loading
# ================================
setwd("C:/Users/SEC/Desktop/2024WS/projectR/dataset/filtered_dataset_finalized")

rds_files <- list.files(pattern = "\\.rds$")
for (file in rds_files) {
  assign(tools::file_path_sans_ext(file), readRDS(file))
}

library(tidyverse)
library(cluster)
library(factoextra)
library(clusterSim)
library(dbscan)
library(rnaturalearth)
library(rnaturalearthdata)

YEAR_RANGE <- paste0("X", 2012:2021)

for (name in c("gdp_usd", "available_resources", "water_productivity",
               "agriculture", "domestic", "gdppc", "population", "precipitation")) {
  temp <- get(name, envir = .GlobalEnv)
  rownames(temp) <- temp$Country.Code
  assign(name, temp, envir = .GlobalEnv)
}

# ================================
# 1. PCA: All Normal Variables
# ================================
pca_data <- data.frame(
  GDP_USD             = rowMeans(gdp_usd[ , YEAR_RANGE], na.rm = TRUE),
  Available_Resources = rowMeans(available_resources[ , YEAR_RANGE], na.rm = TRUE),
  Water_Productivity  = rowMeans(water_productivity[ , YEAR_RANGE], na.rm = TRUE),
  Agriculture         = rowMeans(agriculture[ , YEAR_RANGE], na.rm = TRUE),
  Domestic            = rowMeans(domestic[ , YEAR_RANGE], na.rm = TRUE),
  GDP_Per_Capita      = rowMeans(gdppc[ , YEAR_RANGE], na.rm = TRUE),
  Population          = rowMeans(population[ , YEAR_RANGE], na.rm = TRUE),
  Precipitation       = rowMeans(precipitation[ , YEAR_RANGE], na.rm = TRUE)
)

pca_result <- prcomp(pca_data, scale. = TRUE)
fviz_pca_var(pca_result, col.var = "contrib") + labs(title = "PCA: All Normal Variables")
summary(pca_result)

# ================================
# 2. Clustering Preparation & PCA
# ================================
clustering_data <- data.frame(
  Available_Resources = rowMeans(available_resources[ , YEAR_RANGE], na.rm = TRUE),
  Agriculture         = rowMeans(agriculture[ , YEAR_RANGE], na.rm = TRUE),
  Domestic            = rowMeans(domestic[ , YEAR_RANGE], na.rm = TRUE)
)

pca_clustering <- prcomp(clustering_data, scale. = TRUE)
pca_clustering_data <- pca_clustering$x[, 1:2]

fviz_pca_ind(pca_clustering, geom.ind = "point", pointshape = 21, pointsize = 2,
             fill.ind = "skyblue", col.ind = "black", repel = TRUE) +
  labs(title = "PCA: Selected Features Only") + theme_minimal()

# ================================
# 3. Clustering: 3 Models
# ================================
set.seed(44)
kmeans_result <- kmeans(pca_clustering_data, centers = 3, nstart = 25)
dbscan_result <- dbscan(pca_clustering_data, eps = 1, minPts = 5)
hclust_result <- hclust(dist(pca_clustering_data), method = "ward.D2")

# ================================
# 4. Cluster Visualizations
# ================================
fviz_cluster(list(data = pca_clustering_data, cluster = kmeans_result$cluster),
             main = "K-means Clustering", ellipse.type = "t", geom = "point",
             show.clust.cent = FALSE)

fviz_cluster(list(data = pca_clustering_data, cluster = dbscan_result$cluster),
             main = "DBSCAN Clustering", ellipse.type = "t", geom = "point",
             show.clust.cent = FALSE)

fviz_cluster(list(data = pca_clustering_data, cluster = cutree(hclust_result, 3)),
             main = "Hierarchical Clustering", ellipse.type = "t", geom = "point",
             show.clust.cent = FALSE)

# ================================
# 5. Clustering Performance Evaluation
# ================================
sil_kmeans <- silhouette(kmeans_result$cluster, dist(pca_clustering_data))
sil_hier   <- silhouette(cutree(hclust_result, 3), dist(pca_clustering_data))
sil_dbscan <- silhouette(as.numeric(dbscan_result$cluster), dist(pca_clustering_data))

db_kmeans <- index.DB(pca_clustering_data, kmeans_result$cluster)$DB
db_hier   <- index.DB(pca_clustering_data, cutree(hclust_result, 3))$DB

valid <- dbscan_result$cluster != 0
db_dbscan <- if (length(unique(dbscan_result$cluster[valid])) >= 2)
  index.DB(pca_clustering_data[valid, ], dbscan_result$cluster[valid])$DB else NA

cat("Silhouette Means:\n")
cat("K-means     :", mean(sil_kmeans[, 3]), "\n")
cat("DBSCAN      :", mean(sil_dbscan[, 3]), "\n")
cat("Hierarchical:", mean(sil_hier[, 3]), "\n")

cat("Davies-Bouldin Index:\n")
cat("K-means     :", db_kmeans, "\n")
cat("DBSCAN      :", db_dbscan, "\n")
cat("Hierarchical:", db_hier, "\n")

# ================================
# 6. Silhouette Distribution Plot
# ================================
sil_all <- bind_rows(
  as.data.frame(sil_kmeans) %>% mutate(Method = "K-means"),
  as.data.frame(sil_hier)   %>% mutate(Method = "Hierarchical"),
  as.data.frame(sil_dbscan) %>% mutate(Method = "DBSCAN")
)

ggplot(sil_all, aes(x = Method, y = sil_width, fill = Method)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 0.5) +
  labs(title = "Silhouette Score Distribution", y = "Silhouette Width") +
  theme_minimal()

# ================================
# 7. K-means: Profile + Map + Save
# ================================
# Verify the optimal k using Elbow method
fviz_nbclust(pca_clustering_data, kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal k")

# Verify the optimal k using Silhouette method
fviz_nbclust(pca_clustering_data, kmeans, method = "silhouette") +
  labs(title = "Silhouette Method for Optimal k")

clustering_dataset <- data.frame(
  Country.Code = rownames(clustering_data),
  clustering_data,
  Cluster = as.factor(kmeans_result$cluster)
)

# Profile plot
cluster_profiles <- clustering_dataset %>%
  group_by(Cluster) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  pivot_longer(-Cluster, names_to = "Feature", values_to = "Mean")

ggplot(cluster_profiles, aes(x = Feature, y = Mean, fill = Cluster)) +
  geom_col(position = "dodge") +
  scale_y_log10() +
  labs(title = "Cluster Profiles (log scale)", y = "Average") +
  theme_minimal()

# World map
world <- ne_countries(scale = "medium", returnclass = "sf")
map_data <- left_join(world, clustering_dataset, by = c("iso_a3" = "Country.Code"))

ggplot(map_data) +
  geom_sf(aes(fill = Cluster)) +
  labs(title = "World Map: Clustered Countries") +
  theme_void()

# Save result
setwd("C:/Users/SEC/Desktop/2024WS/projectR/dataset/clustering")
write.csv(clustering_dataset, "clustering_result.csv", row.names = FALSE)
saveRDS(kmeans_result, "kmeans_result_k3.rds")
