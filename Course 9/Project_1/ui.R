#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Predict the manner in which they did the exercise"),
    
    sidebarLayout(
        sidebarPanel( 
            sliderInput("test_case", "Select a test case", 1, 20, value = 10),
            submitButton("Submit")
        ),
        mainPanel( 
            h3("Classe of the test case (1-A, 2-B, 3-C, 4-D, 5-E):"),
            textOutput("classe")
        )
    )
))
