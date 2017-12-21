# - Set Working directory
setwd("C:/Users/ABC/Cursos/Data Scientists - Johns Hopkins University/WorkingDirectory_Curso/datasciencecoursera_repository/Course 8/")

# - Load libraries
library(ISLR)
library(caret)
library(ggplot2)

# - Load data
data(Wage)
Wage <- subset(Wage, select = -c(logwage))

# - Create training and test sets
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training); dim(testing)

# - Fit the model
modFit <- train( wage ~ ., method="gbm", data=training, verbose=FALSE)
print(modFit)

# - Plot the results
qplot(predict(modFit, testing), wage, data=testing)