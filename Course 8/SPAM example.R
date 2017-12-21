
# - Set Working directory
setwd("C:/Users/ABC/Cursos/Data Scientists - Johns Hopkins University/WorkingDirectory_Curso/datasciencecoursera_repository/Course 8/")

# - Load libraries
library(caret)
library(kernlab)
library(RANN)

# - Load data
data(spam)


############################################################################################
################################### TRAY TO PREDICT TYPE ###################################
############################################################################################

set.seed(32343)

################################# First steps ################################# 

# - Create a series of test/training partitions
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)
# -- Split into two dataframes
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training);dim(testing)



################################# Data slicing ################################# 

# K-fold
folds <- createFolds(y=spam$type, k=10, list = TRUE, returnTrain = TRUE)
sapply(folds, length)
folds[[1]][1:10]

# Return test
folds <- createFolds(y=spam$type, k=10, list = TRUE, returnTrain = FALSE)
sapply(folds, length)
folds[[1]][1:10]

# Resampling
folds <- createResample(y=spam$type, times=10, list = TRUE)
sapply(folds, length)
folds[[1]][1:10]

# Times Slices
folds <- createTimeSlices(y=1:1000, initialWindow =20, horizon =10)
names(folds)
folds$train[[1]]
folds$test[[1]]


################################# PREPROCESSING ################################# 

# Histogram
hist(training$capitalAve, main="", xlab="ave. capital run length")

# Mean and SD
mean(training$capitalAve)
sd(training$capitalAve)

# Standardizing
trainCapAve <- training$capitalAve
trainCapAveS <- (trainCapAve - mean(trainCapAve))/sd(trainCapAve)
mean(trainCapAveS)
sd(trainCapAveS)


# Standardizing - PreProcess function: method=c("center","scale")
preObj <- preProcess(training[,-58], method=c("center","scale"))

trainCapAveS <- predict(preObj, training[,-58])$capitalAve
mean(trainCapAveS)
sd(trainCapAveS)

testCapAveS <- predict(preObj, testing[,-58])$capitalAve
mean(testCapAveS)
sd(testCapAveS)


# Standardizing - PreProcess function: method=c("BoxCox")
preObj <- preProcess(training[,-58], method=c("BoxCox"))

trainCapAveS <- predict(preObj, training[,-58])$capitalAve
mean(trainCapAveS)
sd(trainCapAveS)

par(mfrow=c(1,2)); hist(trainCapAveS); qqnorm(trainCapAveS)



################################# PREDITIONS ################################# 

# - Built a Model Without preprocessing
modelFit1 <- train(type ~., data=training, method="glm")
modelFit1
modelFit1$finalModel

# -- Prediction
predictions <- predict(modelFit1, newdata=testing)

# -- Confusion Matrix
confusionMatrix(predictions, testing$type)


# - Built a Model With preprocessing
modelFit3 <- train(type ~., data=training, preProcess=c("BoxCox"), method="glm")
modelFit3
modelFit3$finalModel


# - Built a Model With preprocessing
modelFit2 <- train(type ~., data=training, preProcess=c("center","scale"), method="glm")
modelFit2
modelFit2$finalModel



################################# Inputing data ################################# 
set.seed(13343)

# Make some values NA
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1], size=1, prob=0.05)==1
training$capAve[selectNA] <- NA

# Inpute and standarize
preObj <- preProcess(training[,-58],method="knnImpute")
capAve <- predict(preObj, training[,-58])$capAve

# Standarize true values
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth - mean(capAveTruth))/sd(capAveTruth)

quantile(capAve -capAveTruth)
quantile((capAve -capAveTruth)[selectNA])
quantile((capAve -capAveTruth)[!selectNA])


########################### Transforming tidy covariates ########################### 
spam$capitalAveSq <- spam$capitalAve^2





