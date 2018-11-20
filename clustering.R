# Clustering
trainingPath <- file.path('./bigMartTrain.csv')
trainData <- read.csv(trainingPath)
dim(trainData)

# testingPath <- file.path('./bigMartTest.csv')
# testData <- read.csv(testingPath)
# dim(testData)

head(trainData)
str(trainData)

############### Data Preprocessing ###################s
# Just using the features with numeric value for clustering
library(dplyr)
# trainData <- trainData %>% select(Item_Visibility, Item_MRP, Item_Outlet_Sales)
trainData <- trainData %>% select(Item_MRP, Item_Outlet_Sales)

sum(is.na(trainData))
trainData[!complete.cases(trainData),]

###################### K-means ####################


# Elbow method to detect the best number of clusters for K-means
set.seed(123)
vec <- vector()
for (i in 1:10) {
  vec[i] = sum(kmeans(trainData, i)$withinss)
}

plot(x = 1:10, y = vec, type = 'b', main = 'The Elbow Method', xlab = 'Number of Clusters', ylab = 'WCSS')

# From the chart of the elbow method, the best number of cluster or k value is 4
# Fitting kmeans to the dataset
library(cluster)

set.seed(123)
kmeans <- kmeans(x = trainData, centers = 4)
ykmeans <- kmeans$cluster

# Visualizing the clusters
clusplot(trainData, ykmeans, lines = 0, shade = T, color = T, labels = 2, plotchar = F, span = T, 
         main = 'Clusters of Items', ylab = 'Item Outlet Sales')




################## Hierarchical clustering ##################
# Using the dendrogram to find the optimal number of clusters
dendrogram = hclust(d = dist(trainData, method = 'euclidean'), method = 'ward.D')
plot(dendrogram,
     main = paste('Dendrogram'),
     xlab = 'Items',
     ylab = 'Euclidean distances')

# Fitting Hierarchical Clustering to the dataset
hc = hclust(d = dist(trainData, method = 'euclidean'), method = 'ward.D')
y_hc = cutree(hc, 4)

# Visualising the clusters
library(cluster)
clusplot(trainData,
         y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels= 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of items'),
         xlab = 'Item Visibility',
         ylab = 'Item Outlet Sales')
