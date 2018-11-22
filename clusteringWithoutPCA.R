# Clustering Algorithm Comparison
# Loading the data
trainingPath <- file.path('./bigMartTrain.csv')
trainData <- read.csv(trainingPath)
dim(trainData)

testingPath <- file.path('./bigMartTest.csv')
testData <- read.csv(testingPath)
dim(testData)

# Add a column to the test data
testData$Item_Outlet_Sales <- 1

# Merge the train data and test data into 1
martData <- rbind(trainData, testData)
dim(martData)
head(martData)
str(martData)

############### Data Preprocessing ###################s
library(dplyr)

# Checking the NA values
sum(is.na(martData))
View(martData[!complete.cases(martData),])
sum(is.na(martData$Item_Weight))

# Impute the NAs with the median
martData$Item_Weight[is.na(martData$Item_Weight)] <- median(martData$Item_Weight, na.rm=T)

# No more NA after inferring the mean to the NAs
sum(is.na(martData))

# Some of the item visibility has the value of 0 which indicate some missing data
sum(martData$Item_Visibility == 0)

# Impute them with median
martData$Item_Visibility <- ifelse(martData$Item_Visibility == 0, median(martData$Item_Visibility),
                                   martData$Item_Visibility)

# Cleaning the levels of the factor
sum(martData$Item_Fat_Content=='LF')
martData$Item_Fat_Content <- ifelse(martData$Item_Fat_Content == 'LF', 'Low Fat', martData$Item_Fat_Content)
sum(martData$Item_Fat_Content=='low fat')
martData$Item_Fat_Content <- ifelse(martData$Item_Fat_Content == 'low fat', 'Low Fat', martData$Item_Fat_Content)
sum(martData$Item_Fat_Content=='reg')
martData$Item_Fat_Content <- ifelse(martData$Item_Fat_Content == 'reg', 'Regular', martData$Item_Fat_Content)
levels(martData$Item_Fat_Content) <- c('Low Fat', 'Regular')


# Clustering is unsupervised so remove the response variable
clusData <- martData %>% select(Item_Weight, Item_Fat_Content, Item_Type, Item_Visibility, Item_MRP, 
                                Outlet_Establishment_Year, Outlet_Size, Outlet_Type, Outlet_Location_Type)

colnames(clusData)
str(clusData)

###################### K-means ####################
# Just using the features with numeric value for clustering
kmeansData <- martData %>% select(Item_MRP, Item_Weight, Item_Visibility)

# Elbow method to detect the best number of clusters for K-means
set.seed(123)
vec <- vector()
for (i in 1:10) {
  vec[i] = sum(kmeans(kmeansData, i)$withinss)
}

plot(x = 1:10, y = vec, type = 'b', main = 'The Elbow Method', xlab = 'Number of Clusters', ylab = 'WCSS')

# From the chart of the elbow method, the best number of cluster or k value is 4
# Fitting kmeans to the dataset
library(cluster)

set.seed(123)
kmeans <- kmeans(x = kmeansData, centers = 4)
ykmeans <- kmeans$cluster

# Visualizing the clusters
clusplot(kmeansData, ykmeans, lines = 0, shade = T, color = T, labels = 2, plotchar = F, span = T, 
         main = 'Clusters of Items', xlab = 'X', ylab = 'Y')



################## Hierarchical clustering ##################
hierData <- martData %>% select(Item_MRP, Item_Weight, Item_Visibility, Item_Fat_Content, Item_Type, Outlet_Type,
                                 Outlet_Size)

gowerDist <- daisy(hierData, metric = 'gower')

# Divisive clustering
divisiveClus <- diana(as.matrix(gowerDist), diss = TRUE, keep.diss = TRUE)
plot(divisiveClus, main = "Divisive")

# Aggloromerative clustering
agglClus <- hclust(gowerDist, method = 'complete')
plot(agglClus, main = 'Agglomerative, complete linkage')

# Using the dendrogram to find the optimal number of clusters
# dendrogram = hclust(d = dist(hierData, method = 'euclidean'), method = 'ward.D')
# plot(dendrogram,
#      main = paste('Dendrogram'),
#      xlab = 'Items',
#      ylab = 'Euclidean distances')

# Fitting Hierarchical Clustering to the dataset
# hc = hclust(d = dist(hierData, method = 'euclidean'), method = 'ward.D')

y_hc = cutree(agglClus, 5)

# Visualising the clusters
clusplot(hierData,
         y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels= 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of items'),
         xlab = 'X',
         ylab = 'Y')