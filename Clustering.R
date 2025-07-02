# clustering_cereal_analysis.R
# Title: Clustering Analysis on Cereal Brand Data
# Author: Surath Chakraborti
# Description: Performs various clustering methods (agglomerative, divisive, and k-means)
#              on cereal brand data and a precomputed dissimilarity matrix.

# --- Libraries ---
library(readxl)
library(cluster)
library(factoextra)

# -------------------------
# Part 1: Agglomerative Clustering on Dissimilarity Matrix
# -------------------------
# Read lower triangle of dissimilarity matrix
lower_triangle <- read_xlsx("C:/Users/Stats/AMA/AMA_Practical_1 Data (1).xlsx", 
                            sheet = 1, col_names = TRUE, range = "B3:W25")

# Perform hierarchical clustering
single_linkage   <- hclust(as.dist(lower_triangle), method = "single")
complete_linkage <- hclust(as.dist(lower_triangle), method = "complete")
group_average    <- hclust(as.dist(lower_triangle), method = "average")

# Plot dendrograms
par(mfrow = c(1, 3))
plot(as.dendrogram(single_linkage), main = "Single Linkage")
plot(as.dendrogram(complete_linkage), main = "Complete Linkage")
plot(as.dendrogram(group_average), main = "Group-Average Linkage")
par(mfrow = c(1, 1))

# -------------------------
# Part 2: Clustering on Cereal Brand Data
# -------------------------
# Load cereal brand data
cereal_data <- read_xlsx("C:/Users/DELL/Desktop/SXUK/Sem 3/Applied multivariate Analysis/Pracs/AMA_Practical_1 Data.xlsx", 
                         sheet = 2, col_names = TRUE, range = "A4:F16")

# Preprocessing: Remove brand column and handle missing values
numeric_data <- cereal_data[, -1]
numeric_data <- apply(numeric_data, 2, as.numeric)
numeric_data[is.na(numeric_data)] <- colMeans(numeric_data, na.rm = TRUE)
numeric_data <- na.omit(numeric_data)

# Compute Euclidean distance matrix
dist_mat <- dist(numeric_data)

# --- Agglomerative Clustering using Centroid Method ---
Hierar_centroid <- hclust(dist_mat, method = "centroid")
plot(Hierar_centroid, main = "Agglomerative Clustering (Centroid)")

# --- Divisive Clustering (Monothetic) ---
div_monothetic <- diana(cereal_data)  # includes non-numeric column
plot(div_monothetic, main = "Divisive Clustering (Monothetic)")

# --- Divisive Clustering (Polythetic) ---
div_polythetic <- diana(dist_mat, diss = TRUE)
plot(div_polythetic, main = "Divisive Clustering (Polythetic)")

# -------------------------
# Part 3: K-means Clustering and Elbow Method
# -------------------------
wcss <- numeric()  # Within-cluster sum of squares

for (k in 1:10) {
  km <- kmeans(numeric_data, centers = k, nstart = 25)
  wcss[k] <- km$tot.withinss
}

# Plot Elbow Curve
plot(1:10, wcss, type = "b", pch = 19, col = "blue",
     xlab = "Number of Clusters (k)", ylab = "WCSS",
     main = "Elbow Method for Optimal k")

# Choose elbow point (heuristic)
elbow_point <- which(diff(wcss) / diff(1:10) < 0.1)[1] + 1
cat("Optimal number of clusters (k) based on elbow point:", elbow_point, "\n")

# Final K-means Clustering
final_kmeans <- kmeans(numeric_data, centers = elbow_point, nstart = 25)
print(final_kmeans$cluster)
