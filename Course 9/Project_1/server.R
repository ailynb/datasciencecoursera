#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#





############################## Shiny Server ############################## 

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    # - Reproduceability
    set.seed(1234)
    
    
    # Read wine data
    while(!exists("wine_red"))
        wine_red <- read.csv2(url("http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"),
                              dec = ".")
    wine_red$color <- "red"
    
    while(!exists("wine_white"))
        wine_white <- read.csv2(url("http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"),
                                dec = ".")
    wine_white$color <- "white"
    
    # - Built only a dataframe taking some variables
    wine_red <- wine_red[,c("alcohol","pH","density","volatile.acidity","quality","color")]
    wine_white <- wine_white[,c("alcohol","pH","density","volatile.acidity","quality","color")]
    wine <- rbind(wine_red,wine_white)
    
    
    ###### - Predit "quality" with Regression Tree
    # - Create a series of testing/training partitions 
    library(caret)
    inTrain <- createDataPartition(wine$quality, p=0.7, list=FALSE)
    training <- wine[inTrain,]
    testing <- wine[-inTrain,]
    
    # - Built a Model and Predict on the testing set
    library(rpart) #recursive and partitioning trees
    model <- rpart(quality ~. , data = training)
    
    modelpred <- reactive({
        test <- data.frame("alcohol"=input$alcohol , "pH"=input$pH , "density"=input$density , 
                           "volatile.acidity"=input$volatile_acidity , "quality"=NA , 
                           "color"=input$color , "taste" =NA  )
        predict(model, newdata = test)
    })
    
    pred_MAE <- reactive({
        mean(abs(testing$quality - modelpred()))
    })
    
    output$pred1 <- renderText({
        paste(modelpred(),"(",ifelse(modelpred() < 6, 'bad', ifelse(modelpred() == 6, 'normal', 'good')),")")
    })
    
    output$pred_MAE <- renderText({
        paste(pred_MAE()*100,"%")
    })
    
})
