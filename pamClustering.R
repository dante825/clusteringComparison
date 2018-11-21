# Partitioning around medoids (PAM) clustering algorithm
# https://www.r-bloggers.com/clustering-mixed-data-types-in-r/

library(dplyr)
library(cluster)
library(Rtsne)
library(ggplot2)

# Loading the data
trainingPath <- file.path('./bigMartTrain.csv')
trainData <- read.csv(trainingPath)
dim(trainData)

# Select the continuous and nominal data for clustering
pamData <- trainData %>% select(Item_MRP, Item_Weight, Item_Visibility, Item_Fat_Content, Item_Type, Outlet_Type,
                                   Outlet_Size, Outlet_Location_Type)

### Mixed data so use gower distance instead of euclidean
# Calculate the gower distance
gower_dist <- daisy(pamData, metric = 'gower', type = list(logratio = 3))

# Check the data types: I-interval, N-nominal
summary(gower_dist)

# Sanity check
gower_mat <- as.matrix(gower_dist)
# Find the most similar pair
trainData[which(gower_mat==min(gower_mat[gower_mat != min(gower_mat)]), arr.ind = TRUE)[1, ], ]
# Find the most dissimilar pair
trainData[which(gower_mat==max(gower_mat[gower_mat != max(gower_mat)]), arr.ind = TRUE)[1, ], ]

### Select number of clusters
# Calculate the silhouette width for many k using PAM
sil_width <- c(NA)

for (i in 2:10) {
  pam_fit <- pam(gower_dist, diss = TRUE, k=i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}

# plot silhouette width (higher is better)
plot(1:10, sil_width, xlab = 'Number of clusters', ylab = 'Silhouette Width')
lines(1:10, sil_width)

# The highest value of the chart should be the number of clusters

### Cluster interpretation
pam_fit <- pam(gower_dist, diss = TRUE, k=3)

pam_result <- pamData %>% select()

# Incomplete, refer to the link