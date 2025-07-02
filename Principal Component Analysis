# pca_analysis.R
# Author: Surath Chakraborti
# Description: Performs Principal Component Analysis using a given correlation matrix
#              and an Excel dataset using various visualization techniques.

# -------------------------------
# Part 1: PCA using Manual Correlation Matrix
# -------------------------------

# Create the correlation matrix (6x6)
R <- matrix(c(
  1, 0.4919, 0.2636, 0.4653, -0.2277, 0.0652,
  0.4919, 1, 0.3127, 0.3506, -0.1917, 0.2045,
  0.2636, 0.3127, 1, 0.4108, 0.0647, 0.2493,
  0.4653, 0.3506, 0.4108, 1, -0.2249, 0.2293,
  -0.2277, -0.1917, 0.0647, -0.2249, 1, -0.2144,
  0.0652, 0.2045, 0.2493, 0.2293, -0.2144, 1
), nrow = 6, ncol = 6, byrow = TRUE)

# Perform PCA
pca_full <- princomp(covmat = R)
PC_full <- pca_full$loadings
print("Principal Components from Full Correlation Matrix:")
print(PC_full)

# PCA on a subset of R (first 4 variables)
R_subset <- R[, 1:4]
pca_subset <- princomp(covmat = R_subset)
PC_subset <- pca_subset$loadings
print("Principal Components from Subset Correlation Matrix (First 4 Variables):")
print(PC_subset)

# -------------------------------
# Part 2: PCA using Excel Data
# -------------------------------

# --- Libraries ---
library(readxl)
library(factoextra)
library(ggplot2)
library(FactoMineR)
library(ggcorrplot)

# --- Load and preprocess data ---
df <- read_excel("C:/Users/User 26/Desktop/PCA.xlsx", sheet = "Sheet2")
num_df <- df[, 2:9]  # Select only numerical columns
num_df <- as.data.frame(lapply(num_df, as.numeric))  # Ensure numeric

# Normalize the data (Z-score)
norm_df <- scale(num_df)

# Correlation matrix
corr_matrix <- cor(norm_df)
ggcorrplot(corr_matrix, lab = TRUE, title = "Correlation Matrix")

# --- Perform PCA ---
data_pca <- princomp(covmat = corr_matrix)
summary(data_pca)
loadings(data_pca)

# PCA with FactoMineR and factoextra for visualization
pca_fm <- PCA(norm_df, graph = FALSE)

# Scree plot
fviz_eig(pca_fm, addlabels = TRUE, ylim = c(0, 60))

# Variable contributions
fviz_pca_var(pca_fm, col.var = "contrib") +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  ggtitle("PCA - Variable Contributions")

# Individual PCA scores
fviz_pca_ind(pca_fm, col.ind = "cos2", gradient.cols = c("blue", "green", "red")) +
  ggtitle("PCA - Individuals")

