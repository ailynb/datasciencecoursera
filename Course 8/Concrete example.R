
# - Set Working directory
setwd("C:/Users/ABC/Cursos/Data Scientists - Johns Hopkins University/WorkingDirectory_Curso/datasciencecoursera_repository/Course 8/")

# - Load libraries
library(AppliedPredictiveModeling)
library(ggplot2)
library(caret)
library(Hmisc)

# - Load data
data(concrete)

set.seed(1000)

# - Create a series of test/training partitions
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]



# Plot: CompressiveStrength versus versus the index of the samples
names <- colnames(concrete)
names <- names[-length(names)]
featurePlot(x = training[, names], y = training$CompressiveStrength, plot = "pairs")
## - It is clear from this plot that there is no relation between the outcome and any of the other variables int he data set
## - Now we'll make a plot of the outcome as a function of the index
index <- seq_along(1:nrow(training))
ggplot(data = training, aes(x = index, y = CompressiveStrength)) + geom_point() + theme_bw()
## - From this plot we should probably cut the outcome in 4 categories
cutCS <- cut2(training$CompressiveStrength, g = 4)
summary(cutCS)
## - Make a plot of the categorized outcome outcome
ggplot(data = training, aes(y = index, x = cutCS)) + geom_boxplot() + geom_jitter(col = "blue") + 
    theme_bw()
# - Now the step is better seen in the above plot. As we can see this plot the step-like pattern is more clear now.
# - Now we'll make a plot of the categorized income as function of the rest of the variables
featurePlot(x = training[, names], y = cutCS, plot = "box")
## -- Again, none of the variables in the data can explaing the step-like behaviour in the outcome.



# Plot: Make a histogram and confirm the SuperPlasticizer variable is skewed.
ggplot(data = training, aes(x = Superplasticizer)) + geom_histogram() + theme_bw()
## -- It is clear that there are plenty of zeros in this parameter so taking the log base 10 would yield infinities.









