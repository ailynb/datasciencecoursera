#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

############################## Wine Quality ############################## 
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





############################## UI ############################## 
# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Predict Wine Quality"),
    sidebarLayout(
        sidebarPanel( 
            sliderInput("volatile_acidity", "volatile acidity:", 0, max(wine$volatile.acidity), value= mean(wine$volatile.acidity)),
            sliderInput("density", "density:", 0,  max(wine$density), value= mean(wine$density)),
            sliderInput("pH", "pH:", 0, max(wine$pH), value = mean(wine$pH)),
            sliderInput("alcohol", "alcohol:", 0, max(wine$alcohol), value = mean(wine$alcohol)),
            radioButtons(inputId = "color",
                         label = "Wine type: ",
                         choices = c("Red"="red", "White"="white"),
                         inline = TRUE),
            submitButton("Submit")
        ),
        mainPanel( 
            h5("Predicted quality (score between 0 and 10):"),
            textOutput("pred1"),
            h5("Mean Absolute Error (MAE):"),
            textOutput("pred_MAE")
        )
    )
))
