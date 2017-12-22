# - Set Working directory
setwd("C:/Users/ABC/Cursos/Data Scientists - Johns Hopkins University/WorkingDirectory_Curso/datasciencecoursera_repository/Course 8/")

# - Load libraries
library(ggplot2)
library(caret)

################################################### EXAMPLE 1 ################################################### 
# - Load data
data("iris")
names(iris)
table(iris$Species)

# - Create training and test sets
inTrain <- createDataPartition(y=iris$Species, p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training); dim(testing)

# Petal.Width vs Sepal.Width
qplot(Petal.Width, Sepal.Width, colour=Species, data=training)


# - Create a Model
modFit <- train(Species ~ ., method="rpart", data=training)
print(modFit$finalModel)
# Plot tree
plot(modFit$finalModel, uniform = TRUE, main="Classification Tree")
text(modFit$finalModel, use.n = TRUE, all=TRUE, cex=.8)
# -- other type of plot
library(rattle)
fancyRpartPlot(modFit$finalModel)

# - Predict new values
predict(modFit, newdata = testing)



################################################### EXAMPLE 2 ################################################### 
# - Load data
data(ozone, package = "ElemStatLearn")
ozone <- ozone[order(ozone$ozone),]
head(ozone)

# - Bagged loess
ll <- matrix(NA, nrow = 10, ncol=155)
for(i in 1:10){
    ss <- sample(1:dim(ozone)[1], replace = T)
    ozone0 <- ozone[ss,]; ozone0 <- ozone0[order(ozone0$ozone),]
    loess0 <- loess(temperature ~ ozone, data=ozone0, span=.2)
    ll[i,] <- predict(loess0, newdata=data.frame(ozone=1:155))
}
plot(ozone$ozone, ozone$temperature, pch=19, cex=.5)
for(i in 1:10){
    lines(1:155, ll[i,], col="grey", lwd=2)
}
lines(1:155, apply(ll,2,mean), col="red", lwd=2)


# - Baggin in caret
predictors <- data.frame(ozone=ozone$ozone)
temperature <- ozone$temperature
treebag <- bag(predictors, temperature, B=10, 
               bagControl=bagControl(fit=ctreeBag$fit, predict=ctreeBag$pred, agrregate=ctreeBag$aggrefate))
plot(ozone$ozone, temperature, col="lightgrey",pch=19)
points(ozone$ozone, predict(treebag$fits[[1]]$fit, predictors),pch=19,col="red")
points(ozone$ozone, predict(treebag, predictors),pch=19,col="blue")





################################################### EXAMPLE 3 ################################################### 
# - Load data
data("iris")
names(iris)
table(iris$Species)

# - Create training and test sets
inTrain <- createDataPartition(y=iris$Species, p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training); dim(testing)

# - Create a Model: Random Forest
library(randomForest)
modFit <- train(Species ~ ., method="rf", prox=TRUE, data=training)

# - Get a single tree
getTree(modFit$finalModel, k=2)

# - Class "centers"
irisP <- classCenter(training[,c(3,4)], training$Species, modFit$finalModel$prox)
irisP <- as.data.frame(irisP); irisP$Species <- rownames(irisP)
qplot(Petal.Width, Petal.Length, col=Species, data=training) +
    geom_point(aes(x=Petal.Width, y=Petal.Length, col=Species), size=5, shape=4, data=irisP)

# - Predicting new values
pred <- predict(modFit, testing)
testing$predRight <- pred==testing$Species
table(pred, testing$Species)
qplot(Petal.Width, Petal.Length, col=predRight, data=testing, main="Newdata Predictions") +
    geom_point(aes(x=Petal.Width, y=Petal.Length, col=Species), size=5, shape=4, data=irisP)












