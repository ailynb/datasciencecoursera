
# - Set Working directory
setwd("C:/Users/ABC/Cursos/Data Scientists - Johns Hopkins University/WorkingDirectory_Curso/datasciencecoursera_repository/Course 8/")

# - Load libraries
library(caret)

# - Load data
data("faithful")

set.seed(333)

# - Create a series of test/training partitions
inTrain <- createDataPartition(y=faithful$waiting, p=0.5, list=FALSE)
# -- Split into two dataframes
training <- faithful[inTrain,]
testing <- faithful[-inTrain,]
dim(training);dim(testing)


# Plot: eruption duration versus waiting time
p <- plot(training$waiting, training$eruptions, pch=19, col="blue", xlab="Waiting", ylab="Duration")


############################################################################################
###################################     TRAY TO PREDICT  ###################################
#                                    with regression model
############################################################################################

# Fit a linear model
lm1 <- lm(eruptions ~ waiting, data=training)
summary(lm1)

lines(training$waiting, lm1$fitted, lwd=2)


# - PREDICT A NEW VALUE OF ERUPTION FOR A WAITING
coef(lm1)[1] + coef(lm1)[2]*80
newdata <- data.frame(waiting=80)
predict(lm1, newdata)


# - PLOT PREDICTIONS: training and test
par(mfrow=c(1,2))
plot(training$waiting, training$eruptions, pch=19, col="blue", xlab="Waiting", ylab="Duration", main="Training")
lines(training$waiting, predict(lm1),lwd=2)
plot(testing$waiting, testing$eruptions, pch=19, col="blue", xlab="Waiting", ylab="Duration", main="Testing")
lines(testing$waiting, predict(lm1, newdata=testing),lwd=2)

# - Get training/test set errors
# -- Calculate RMSE on training
sqrt(sum((lm1$fitted - training$eruptions)^2))
# -- Calculate RMSE on testing
sqrt(sum((predict(lm1, newdata=testing) - testing$eruptions)^2))


# - PREDICTION INTERVALS
dev.off()
pred1 <- predict(lm1, newdata=testing, interval="prediction")
ord <- order(testing$waiting)
plot(testing$waiting, testing$eruptions, pch=19, col="blue", xlab="Waiting", ylab="Duration", main="Testing")
matlines(testing$waiting[ord],pred1[ord,], type="l", col=c(1,2,2), lty=c(1,1,1), lwd=2)



############################################################################################
###################################     TRAY TO PREDICT  ###################################
#                                    with caret package
############################################################################################

# - Built a Model Without preprocessing
modelFit <- train(eruptions ~ waiting, data=training, method="lm")
summary(modelFit)







