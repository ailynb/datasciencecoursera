# - Set Working directory
setwd("C:/Users/ABC/Cursos/Data Scientists - Johns Hopkins University/WorkingDirectory_Curso/datasciencecoursera_repository/Course 9/Project_1")


############################## Wine Quality ############################## 
# - Reproduceability
set.seed(1234)


# Read wine data
while(!exists("wine_red"))
    wine_red <- read.csv2(url("http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"), dec = ".")
wine_red$color <- "red"
while(!exists("wine_white"))
    wine_white <- read.csv2(url("http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"), dec = ".")
wine_white$color <- "white"

# - Built only a dataframe taking some variables
wine_red <- wine_red[,c("alcohol","pH","density","volatile.acidity","quality","color")]
wine_white <- wine_white[,c("alcohol","pH","density","volatile.acidity","quality","color")]
wine <- rbind(wine_red,wine_white)

# - Quality Histogram
library(plotly)
plot_ly(data = wine, x =~quality, type = "histogram")

# -- Mean Absolute Error (MAE)
MAE <- function(actual, predicted){
    mean(abs(actual - predicted))
}


###### - Predit "taste" whit randomForest
wine$taste <- ifelse(wine$quality < 6, 'bad', 'good')
wine$taste[wine$quality == 6] <- 'normal'
wine$taste <- as.factor(wine$taste)
wine$color <- as.factor(wine$color )
summary(wine)
# - Create a series of testing/training partitions 
inTrain <- createDataPartition(wine$taste, p=0.7, list=FALSE)
training <- wine[inTrain,]
testing <- wine[-inTrain,]
# - Built a Model and Predict on the testing set
library(randomForest)
model <- randomForest(taste ~ .- quality, data = training)
pred <- predict(model, newdata = testing)
c_matrix <- table(pred, testing$taste)
(c_matrix[1,1] + c_matrix[2,2] + c_matrix[3,3]) / nrow(testing) # accuracy



###### - Predit "quality" with Regression Tree
# - Create a series of testing/training partitions 
inTrain <- createDataPartition(wine$quality, p=0.7, list=FALSE)
training <- wine[inTrain,-7]
testing <- wine[-inTrain,]
# - Built a Model and Predict on the testing set
library(rpart) #recursive and partitioning trees
model <- rpart(quality ~. , data = training)
pred <- predict(model, newdata = testing)
MAE(testing$quality, pred)


