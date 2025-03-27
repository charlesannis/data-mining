# 1
if (!require("readxl")) install.packages("readxl")
if (!require("cluster")) install.packages("cluster")
library(readxl)
library(cluster)

myData_z <- scale(myData[, 1:5])
d <- dist(myData_z, method = "euclidean")

aggl_single <- agnes(myData_z, method = "single")
par(mar = c(4, 4, 2, 1))
plot(aggl_single, main = "Dendrogram - Single Linkage")
h_single <- as.hclust(aggl_single)
clusters_single <- cutree(h_single, h = 5)
cat("Number of clusters (Single Linkage) at height 5:", length(unique(clusters_single)), "\n")
print(table(clusters_single))

aggl_complete <- agnes(myData_z, method = "complete")
par(mar = c(4, 4, 2, 1))
plot(aggl_complete, main = "Dendrogram - Complete Linkage")
h_complete <- as.hclust(aggl_complete)
clusters_complete <- cutree(h_complete, h = 5)
cat("Number of clusters (Complete Linkage) at height 5:", length(unique(clusters_complete)), "\n")
print(table(clusters_complete))

aggl_ward <- agnes(myData_z, method = "ward")
par(mar = c(4, 4, 2, 1))
plot(aggl_ward, main = "Dendrogram - Ward's Method")
h_ward <- as.hclust(aggl_ward)
clusters_ward <- cutree(h_ward, h = 5)
cat("Number of clusters (Ward's Method) at height 5:", length(unique(clusters_ward)), "\n")
print(table(clusters_ward))


# 2
if (!require("readxl")) install.packages("readxl")
if (!require("cluster")) install.packages("cluster")
library(readxl)
library(cluster)

collegeData <- read_excel("path/to/your_CollegeScorecard_data.xlsx")
data_full <- collegeData[, c("Earnings", "Cost", "Grad", "Debt")]
data_full_z <- scale(data_full)
d_full <- dist(data_full_z, method = "euclidean")
aggl_full <- agnes(data_full_z, method = "ward")
par(mar = c(4, 4, 2, 1))
plot(aggl_full, main = "Dendrogram - All Colleges (Ward's Method)")
h_full <- as.hclust(aggl_full)
clusters_full <- cutree(h_full, h = 15)
cat("Number of clusters (All Colleges) at height 15:", length(unique(clusters_full)), "\n")
print(table(clusters_full))

data_city <- collegeData[collegeData$City == 1, ]
data_city_subset <- data_city[, c("Earnings", "Cost", "Grad", "Debt")]
data_city_z <- scale(data_city_subset)
d_city <- dist(data_city_z, method = "euclidean")
aggl_city <- agnes(data_city_z, method = "ward")
par(mar = c(4, 4, 2, 1))
plot(aggl_city, main = "Dendrogram - Colleges in City (Ward's Method)")
h_city <- as.hclust(aggl_city)
clusters_city <- cutree(h_city, h = 10)
cat("Number of clusters (Colleges in City) at height 10:", length(unique(clusters_city)), "\n")
print(table(clusters_city))

data_non_city <- collegeData[collegeData$City == 0, ]
data_non_city_subset <- data_non_city[, c("Earnings", "Cost", "Grad", "Debt")]
data_non_city_z <- scale(data_non_city_subset)
d_non_city <- dist(data_non_city_z, method = "euclidean")
aggl_non_city <- agnes(data_non_city_z, method = "ward")
par(mar = c(4, 4, 2, 1))
plot(aggl_non_city, main = "Dendrogram - Colleges not in City (Ward's Method)")
h_non_city <- as.hclust(aggl_non_city)
clusters_non_city <- cutree(h_non_city, h = 10)
cat("Number of clusters (Colleges not in City) at height 10:", length(unique(clusters_non_city)), "\n")
print(table(clusters_non_city))


# 3
homes_std <- scale(homes[, c("Price", "Beds", "Baths", "Sqft")])
set.seed(1)
kResult3 <- kmeans(homes_std, centers = 3)
print("k = 3: Cluster assignments")
print(kResult3$cluster)
print("k = 3: Cluster centers (all variables)")
print(round(kResult3$centers, 3))
print("k = 3: Cluster sizes")
print(kResult3$size)
largest_cluster_index3 <- which.max(kResult3$size)
k3_price  <- round(kResult3$centers[largest_cluster_index3, "Price"], 1)
k3_sqft   <- round(kResult3$centers[largest_cluster_index3, "Sqft"], 1)
k3_size   <- kResult3$size[largest_cluster_index3]
cat("Results for k = 3:\n")
cat("Largest cluster size:", k3_size, "\n")
cat("Cluster center values (standardized):\n")
cat("  Price =", k3_price, "\n")
cat("  Sqft  =", k3_sqft, "\n\n")

set.seed(1)
kResult4 <- kmeans(homes_std, centers = 4)
print("k = 4: Cluster assignments")
print(kResult4$cluster)
print("k = 4: Cluster centers (all variables)")
print(round(kResult4$centers, 3))
print("k = 4: Cluster sizes")
print(kResult4$size)
largest_cluster_index4 <- which.max(kResult4$size)
k4_price <- round(kResult4$centers[largest_cluster_index4, "Price"], 1)
k4_beds  <- round(kResult4$centers[largest_cluster_index4, "Beds"], 1)
k4_sqft  <- round(kResult4$centers[largest_cluster_index4, "Sqft"], 1)
k4_size  <- kResult4$size[largest_cluster_index4]
cat("Results for k = 4:\n")
cat("Largest cluster size:", k4_size, "\n")
cat("Cluster center values (standardized):\n")
cat("  Price =", k4_price, "\n")
cat("  Beds  =", k4_beds, "\n")
cat("  Sqft  =", k4_sqft, "\n")


# 4
suppressWarnings(RNGversion("3.5.3"))
library(readxl)
library(cluster)
data_s <- scale(data[, 1:3])
set.seed(1)
kResult <- pam(data_s, k = 3)
summary(kResult)
