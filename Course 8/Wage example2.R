
# - Set Working directory
setwd("C:/Users/ABC/Cursos/Data Scientists - Johns Hopkins University/WorkingDirectory_Curso/datasciencecoursera_repository/Course 8/")

# - Load libraries
library(ISLR)
library(ggplot2)
library(caret)

# - Load data
data(Wage)
Wage <- subset(Wage, select=-c(logwage))
summary(Wage)

# - Create a series of test/training partitions
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
# -- Split into two dataframes
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training);dim(testing)

# - Feature Plot
featurePlot(x=training[,c("age","education","jobclass")], y=training$wage, plot="pairs")

# - Age versus wage color by jobclass
qplot(age,wage,colour=jobclass,data=training)

# - Age versus wage color by education
qplot(age,wage,colour=education,data=training)


############################################################################################
###################################     TRAY TO PREDICT  ###################################
#                       with regression model - Use Multiple Covariates
############################################################################################

# - Built a Model Without preprocessing
modelFit <- train(wage ~ age + jobclass + education, data=training, method="lm")
finalModel <- modelFit$finalModel

# - Diagnostics
plot(finalModel, 1, pch=19, cex=0.5, col="#00000010")
# -- Color by variables not used in the model
qplot(finalModel$fitted, finalModel$residuals, colour=race, data=training)
# -- By index
plot(finalModel$residuals, pch=19)


# - Predicted versus truth in test set
pred <- predict(modelFit, testing)
qplot(wage, pred, colour=year, data=testing)


############################################################################################
###################################     TRAY TO PREDICT  ###################################
#                       with regression model - Use All Covariates
############################################################################################

# - Built a Model Without preprocessing
modelFit <- train(wage ~ ., data=training, method="lm")

# - Predicted versus truth in test set
pred <- predict(modelFit, testing)
qplot(wage, pred, colour=year, data=testing)


