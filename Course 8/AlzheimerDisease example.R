
# - Set Working directory
setwd("C:/Users/ABC/Cursos/Data Scientists - Johns Hopkins University/WorkingDirectory_Curso/datasciencecoursera_repository/Course 8/")

# - Load libraries
library(AppliedPredictiveModeling)
library(ggplot2)
library(caret)

# - Load data
data(AlzheimerDisease)


################################# PART I ################################# 

# Create non-overapping training and test sets with about 50% of the observations assigned to each
adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]


# - Built a Model Without preprocessing
modelFit1 <- train(diagnosis ~., data=training, method="glm")
modelFit1
modelFit1$finalModel

# -- Prediction
predictions <- predict(modelFit1, newdata=testing)

# -- Confusion Matrix
confusionMatrix(predictions, testing$diagnosis)



################################# PART II ################################# 

# - Create a series of test/training partitions
set.seed(3433)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

# Find all the predictor variables in the training set that begin with IL. 
# Calculate the number of principal components needed to capture 80% of the variance. 
## - First we'll get the predictors with grep, then we'll use the preProcess function in caret to estimate the number of
IL_str <- grep("^IL", colnames(training), value = TRUE)
preProc <- preProcess(training[, IL_str], method = "pca", thresh = 0.8)
preProc$rotation
## -- and we can see that there are 7 components required to achieve 80% of the variance



# Create a training data set consisting of only the predictors with variable names beginning with IL and the diagnosis. 
# Build two predictive models, one using the predictors as they are and one using PCA with principal components 
# explaining 80% of the variance in the predictors. Use method="glm" in the train function.
set.seed(3433)
## grep the predictors starting with 'IL'
IL_str <- grep("^IL", colnames(training), value = TRUE)
## make a subset of these predictors
predictors_IL <- predictors[, IL_str]
df <- data.frame(diagnosis, predictors_IL)
inTrain = createDataPartition(df$diagnosis, p = 3/4)[[1]]
training = df[inTrain, ]
testing = df[-inTrain, ]

## train the data using the first method
modelFit <- train(diagnosis ~ ., method = "glm", data = training)
predictions <- predict(modelFit, newdata = testing)
## get the confustion matrix for the first method
C1 <- confusionMatrix(predictions, testing$diagnosis)
print(C1)
A1 <- C1$overall[1]

## do similar steps with the caret package
modelFit <- train(training$diagnosis ~ ., method = "glm", preProcess = "pca", 
                  data = training, trControl = trainControl(preProcOptions = list(thresh = 0.8)))
C2 <- confusionMatrix(testing$diagnosis, predict(modelFit, testing))
print(C2)
A2 <- C2$overall[1]








