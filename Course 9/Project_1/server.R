#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# - Load libraries
library(ggplot2)
library(caret)
library(randomForest)
library(shiny)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    # - Reproduceability
    set.seed(1234)
    
    # - Load Data
    if(!exists("input_training")){
        input_training <- read.csv(url("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"),
                                   na.strings=c("NA","#DIV/0!",""))
        input_testing <- read.csv(url("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"),
                                  na.strings=c("NA","#DIV/0!",""))
    }
    
    #table(input_training$classe)
    
    #summary(input_training)
    
    # - Exclude variables that cannot be used for prediction: identifier, timestamp, and window data
    input_training <- input_training[, - grep("name|timestamp|window|X", colnames(input_training), value=F) ]
    
    # - Remove variables with at least 60% of NA
    cs <- colSums(sapply(input_training, is.na))
    input_training <- input_training[, (cs / nrow(input_training) < 0.6)]
    
    # - Removing zero covariates
    nzv <- nearZeroVar(input_training, saveMetrics = TRUE)
    input_training <- input_training[, !nzv$nzv ]
    
    # - Create a series of testing/training partitions
    inTrain <- createDataPartition(input_training$classe, p=0.7, list=FALSE)
    training <- input_training[inTrain,]
    testing <- input_training[-inTrain,]
    
    # - Built a Model and Predict on the testing set
    modFit <- randomForest(classe ~. , data=training)
    prediction <- predict(modFit, testing)
    cMatrix <- confusionMatrix(prediction, testing$classe)
    
    prediction <- predict(modFit, input_testing)
    input_testing$classe <- prediction
    
    output$classe <- renderText({    
        input_testing[input$test_case,]$classe
    })
    
})
