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

############ Principal Component Analysis (PCA) #############
## Convert the categorical variables into continuous variables
library(dummies)
dummyDf <- dummy.data.frame(clusData, names = c('Item_Fat_Content', 'Item_Type', 'Outlet_Establishmen_Year',
                                                'Outlet_Size', 'Outlet_Type', 'Outlet_Location_Type'))
# All the data is in numeric form
str(dummyDf)

# Divide the dataset into train and test, PCA should be perform on the train only
# pca.train <- dummyDf[1:nrow(trainData),]
# pca.test <- dummyDf[-(1:nrow(trainData))]

## Principal Component Analysis (PCA)
impFeatures <- prcomp(dummyDf, scale. = T)
names(impFeatures)

# Center and Scale = means and std of the variables
impFeatures$center
impFeatures$scale

# Rotation = principal component loading, most important features
impFeatures$rotation

head(impFeatures$rotation)
dim(impFeatures$rotation)

dim(impFeatures$x)
biplot(impFeatures, scale=0)

# Compute the standard deviation for each of the component
std_dev <- impFeatures$sdev

variance <- std_dev^2

# Check the variance for the first 10 components
head(variance, 10)

# Proportion of variance explained
# The higher the percentage the more important the feature
propVariance <- variance/sum(variance)
propVariance[1:20]

# How many of these feature to select?
# Scree plot
plot(propVariance, xlab = 'Principal Component', ylab = 'Proportion of Variance Explained', type = 'b')

# The graph shows around 28 or 29 component explain 98% variance of the dataset

# Confirmation check with a cumulative variance plot
plot(cumsum(propVariance), xlab = 'Principal Component', ylab = 'Cumulative Proportion of Variance Explained', 
     type = 'b')

train2 <- data.frame(Item_Outlet_Sales = martData$Item_Outlet_Sales, impFeatures$x)

train2 <- train2[,1:30]

###################### K-means ####################
# Elbow method to detect the best number of clusters for K-means
set.seed(123)
vec <- vector()
for (i in 1:10) {
  vec[i] = sum(kmeans(train2, i)$withinss)
}

plot(x = 1:10, y = vec, type = 'b', main = 'The Elbow Method', xlab = 'Number of Clusters', ylab = 'WCSS')

# From the chart of the elbow method, the best number of cluster or k value is 4
# Fitting kmeans to the dataset
library(cluster)

set.seed(123)
kmeans <- kmeans(x = train2, centers = 4)
ykmeans <- kmeans$cluster

# Visualizing the clusters
clusplot(train2, ykmeans, lines = 0, shade = T, color = T, labels = 2, plotchar = F, span = T, 
         main = 'Clusters of Items', xlab = 'X', ylab = 'Y')



################## Hierarchical clustering ##################
# Using the dendrogram to find the optimal number of clusters
hc = hclust(d = dist(train2, method = 'euclidean'), method = 'ward.D')
plot(hc,
     main = paste('Dendrogram'),
     xlab = 'Items',
     ylab = 'Euclidean distances')

# Fitting Hierarchical Clustering to the dataset
y_hc = cutree(hc, 3)

# Visualising the clusters
clusplot(train2,
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