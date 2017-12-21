# - Set Working directory
setwd("C:/Users/ABC/Cursos/Data Scientists - Johns Hopkins University/WorkingDirectory_Curso/datasciencecoursera_repository/Course 8/")

# - Load libraries
library(ggplot2)
library(caret)

# - Load Data
data("iris")
names(iris)
table(iris$Species)

# - Create training and test sets
inTrain <- createDataPartition( y=iris$Species, p=.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training); dim(testing)

# - Built predictions
modlda <- train(Species ~ ., data=training, method="lda")
modnb <- train(Species ~ ., data=training, method="nb")
plda <- predict(modlda, testing)
pnb <- predict(modnb, testing)
table(plda,pnb)

# -- Comparison of results
equalPredictions <- (plda==pnb)
qplot(Petal.Width, Sepal.Width, colour=equalPredictions, data=testing)