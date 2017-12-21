
# - Set Working directory
setwd("C:/Users/ABC/Cursos/Data Scientists - Johns Hopkins University/WorkingDirectory_Curso/datasciencecoursera_repository/Course 8/")

# - Load libraries
library(ISLR)
library(ggplot2)
library(caret)
library(Hmisc)
library(gridExtra)

# - Load data
data(Wage)
# summary(Wage)

# - Create a series of test/training partitions
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
# -- Split into two dataframes
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training);dim(testing)

# - Feature Plot
featurePlot(x=training[,c("age","education","jobclass")], y=training$wage, plot="pairs")

# - Age versus wage
qplot(age,wage,colour=jobclass,data=training)

# -- Add regression smoothers
qplot(age,wage,colour=education,data=training) +
    geom_smooth(method = "lm", formula=y~x)

# - Cut in 3 groups
cutWage <- cut2(training$wage, g=3)
table(cutWage)
# -- Plot each group
p1 <- qplot(cutWage, age, data=training, fill=cutWage, geom=c("boxplot"))
# --- Add points overlayed
p2 <- qplot(cutWage, age, data=training, fill=cutWage, geom=c("boxplot","jitter"))
grid.arrange(p1,p2, ncol=2)


# - Compare with tables
t1 <- table(cutWage, training$jobclass)
t1
prop.table(t1,1)


# - Density plots
qplot(wage, colour=education, data=training, geom="density")


########################### Covariate creation ########################### 
### Basic idea: convert factor variables to "indicator variables"

table(training$jobclass)

# - Dummy variables
dummies <- dummyVars(wage ~ jobclass, data=training)
head(predict(dummies, newdata=training))

# - Removing zero covariates
nsv <- nearZeroVar(training, saveMetrics = TRUE)
nsv

## - Spline basis
# library(splines)
# bsBasis <- bs(training$age, df=3)
# bsBasis
## -- Fitting curves with splines
# lm1 <- lm(wage ~bsBasis, data=training)
# plot(training$age, training$wage, pch=19, cex=0.5)
# points(training$age, predict(lm1, newdata=training), col="red", pch=19, cex=0.5)
## -- Splines on the test set
# predict(bsBasis, age=testing$age)









