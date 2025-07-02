# factor_analysis.R
# Author: Surath Chakraborti
# Description: Perform Factor Analysis using a CSV dataset and a given correlation matrix

# -------------------------------
# Part 1: Factor Analysis from CSV (Sales Data)
# -------------------------------

# Install & load required package
if (!require("psych")) install.packages("psych", dependencies = TRUE)
library(psych)

# Read data
sales_data <- read.csv("C:/Users/DELL/Desktop/SXUK/Sem 3/Applied multivariate Analysis/Pracs/AMA prac 3_1.csv")

# Standardize data (Z-score)
standardized_data <- scale(sales_data)
standardized_data <- na.omit(standardized_data)

# Compute covariance and correlation matrices
cov_matrix <- cov(standardized_data)
cor_matrix <- cor(standardized_data)

# Eigen decomposition of the correlation matrix
eigen_decomposition <- eigen(cor_matrix)
eigenvalues <- eigen_decomposition$values
eigenvectors <- eigen_decomposition$vectors

# Scree Plot
plot(1:length(eigenvalues), eigenvalues, type = 'b',
     main = 'Scree Plot',
     xlab = 'Factor Number',
     ylab = 'Eigenvalue')

# Choose number of factors (e.g., 2)
k <- 2
selected_eigenvectors <- eigenvectors[, 1:k]

# Calculate factor loadings
factor_loadings <- cor_matrix %*% selected_eigenvectors
print("Factor Loadings:")
print(factor_loadings)

# Compute factor scores for a new salesperson
salesperson_scores <- c(110, 98, 105, 15, 18, 12, 35)  # update based on your variable count
standardized_scores <- scale(salesperson_scores)  # optional
factor_score <- standardized_scores %*% factor_loadings
print("Factor Score for New Salesperson:")
print(factor_score)

# -------------------------------
# Part 2: Factor Analysis from Manual Correlation Matrix
# -------------------------------

manual_cor_matrix <- matrix(c(
  1.00, 0.02, 0.96, 0.42, 0.01,
  0.02, 1.00, 0.13, 0.71, 0.85,
  0.96, 0.13, 1.00, 0.50, 0.11,
  0.42, 0.71, 0.50, 1.00, 0.79,
  0.01, 0.85, 0.11, 0.79, 1.00
), nrow = 5, byrow = TRUE)

# Factor analysis using 'psych::principal'
manual_fa <- principal(r = manual_cor_matrix, nfactors = 5, rotate = "none")
print("Factor Analysis from Manual Correlation Matrix:")
print(manual_fa)
