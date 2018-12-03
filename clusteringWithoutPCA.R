# Clustering Algorithm Comparison
# Loading the data
trainingPath <- file.path('./bigMartTrain.csv')
martData <- read.csv(trainingPath)
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
martData$Item_Fat_Content[martData$Item_Fat_Content=='LF'] <- 'Low Fat' 
sum(martData$Item_Fat_Content=='low fat')
martData$Item_Fat_Content[martData$Item_Fat_Content=='low fat'] <- 'Low Fat'
sum(martData$Item_Fat_Content=='reg')
martData$Item_Fat_Content[martData$Item_Fat_Content=='reg'] <- 'Regular'
martData$Item_Fat_Content <- factor(martData$Item_Fat_Content)
levels(martData$Item_Fat_Content)

# Cleaning the levels of the factor Outlet_Size
levels(martData$Outlet_Size) <- c('Unknown', 'High', 'Medium', 'Small')

################# Data Selection #######################
# Clustering is unsupervised so remove the response variable and the identifiers (which are not variables)
# Just keep the variables in numeric form
clusData <- martData %>% select(Item_Weight, Item_Visibility, Item_MRP)

colnames(clusData)
str(clusData)
dim(clusData)

###################### K-means ####################
# Elbow method to detect the best number of clusters for K-means
set.seed(123)
vec <- vector()
for (i in 1:10) {
  vec[i] = sum(kmeans(clusData, i)$withinss)
}

plot(x = 1:10, y = vec, type = 'b', main = 'The Elbow Method', xlab = 'Number of Clusters', ylab = 'WCSS')

library(ggplot2)
elbowDf <- data.frame(x = 1:10, y=vec)
ggplot(data=elbowDf, aes(x=x, y=y, group=1)) +
  geom_line() +
  ggtitle('The Elbow Method') +
  xlab('Number of clusters') +
  ylab('WCSS')

# From the chart of the elbow method, the best number of cluster or k value is 4
# Fitting kmeans to the dataset
library(cluster)

set.seed(123)
kmeans <- kmeans(x = clusData, centers = 4)
ykmeans <- kmeans$cluster

# Cluster membership
table(ykmeans)

# Visualizing the clusters
clusplot(clusData, ykmeans, lines = 0, shade = T, color = T, plotchar = F, span = T, 
         main = 'K-means clustering of Big Mart Sales data', xlab = 'X', ylab = 'Y')

################## Hierarchical clustering ##################
library(cluster)

# Using Euclidean distance on the data with numerical values
hc <- hclust(d = dist(clusData, method = 'euclidean'), method = 'ward.D')
plot(hc, main = 'Dendrogram with Euclidean distance', xlab = 'Items', ylab = 'Euclidean distances')

# Get the number of cluster based on the dendogram
rect.hclust(hc, k = 4, border = "red")
y_hc = cutree(hc, 4)

# Cluster membership
table(y_hc)

clusplot(clusData, y_hc, lines=0, shade=TRUE, color=TRUE, plotchar=FALSE, span=TRUE, 
         main='Hierarchical clustering of Big Mart Sales data', xlab='X', ylab='Y')


########### Hierarchical Clustering with Categorical Values ###########
# Includes some categorical values in the data
hierData <- martData %>% select(Item_Fat_Content, Item_Type, Outlet_Type, Outlet_Size)

# Use gower distance instead of euclidean to calculate the similarity
gowerDist <- daisy(hierData, metric = 'gower')

# Aggloromerative clustering Dendogram
hc <- hclust(gowerDist, method = 'complete')
plot(hc, main = 'Agglomerative, complete linkage, Gower distance', xlab = 'Items', ylab = 'Gower distance')

# Get the number of clusters based on the dendogram
rect.hclust(hc, k = 6, border = "red")
y_hc = cutree(hc, 6)

# Cluster membership
table(y_hc)

# Visualising the clusters
clusplot(hierData, y_hc, lines = 0, shade = TRUE, color = TRUE, plotchar = FALSE, span = TRUE, 
         main = "Hierarchical clustering with Gower's distance metric", xlab = 'X', ylab = 'Y')
