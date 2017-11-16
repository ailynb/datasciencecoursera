library(shiny)

shinyUI(fluidPage(
    titlePanel("First Shiny App"),
    sidebarLayout(
        sidebarPanel( 
            h1("H1 Text"),
            h6("H6 Text"),
            em("Emphesized Text")
        ),
        mainPanel( 
            h5("Main Panel Text"),
            code("Some Code!")
        )
    ),
    sidebarLayout(
        sidebarPanel( 
            h1("Move the Slider!"),
            sliderInput("slider2","Slide Me!",0,100,0)
        ),
        mainPanel( 
            h3("Add 10 to slider Value:"),
            textOutput("text1")
        )
    ),
    titlePanel("Plot Random Numbers"),
    sidebarLayout(
        sidebarPanel( 
            numericInput("numeric","How many Random Numbers should be Plotted?", value=1000, min=1, max=1000, step=1),
            sliderInput("sliderX", "Pick minimum and maximum X Values", -100, 100, value = c(-50,50)),
            sliderInput("sliderY", "Pick minimum and maximum Y Values", -100, 100, value = c(-50,50)),
            checkboxInput("show_xlab", "Show/Hide X Axis Label", value=TRUE),
            checkboxInput("show_ylab", "Show/Hide Y Axis Label", value=TRUE),
            checkboxInput("show_title", "Show/Hide Title")
        ),
        mainPanel( 
            h5("Graph of Random Points"),
            plotOutput("plot1")
        )
    ),
    titlePanel("Predict Horsepower from MPG"),
    sidebarLayout(
        sidebarPanel( 
            sliderInput("sliderMPG", "What is the MPG of the car?", 10, 35, value = 20),
            checkboxInput("showModel1", "Show/Hide Model 1", value=TRUE),
            checkboxInput("ShowModel2", "Show/Hide Model 2", value=TRUE),
            submitButton("Submit")
        ),
        mainPanel( 
            plotOutput("plot2"),
            h5("Predicted Horsepower from Model 1:"),
            textOutput("pred1"),
            h5("Predicted Horsepower from Model 2:"),
            textOutput("pred2")
        )
    ),
    titlePanel("Tabs!"),
    sidebarLayout(
        sidebarPanel( 
            textInput("box1","Enter Tab 1 Text:", value="Tab 1!"),
            textInput("box2","Enter Tab 2 Text:", value="Tab 2!"),
            textInput("box3","Enter Tab 3 Text:", value="Tab 3!"),
            submitButton("Submit")
        ),
        mainPanel( 
            tabsetPanel(type="tabs",
                        tabPanel("Tab 1", br(), textOutput("out1")),
                        tabPanel("Tab 2", br(), textOutput("out2")),
                        tabPanel("Tab 3", br(), textOutput("out3"))
            )
        )
    ),
    titlePanel("Visualize many models!"),
    sidebarLayout(
        sidebarPanel( 
            h3("slope"),
            textOutput("slopeOut"),
            h3("Intercept"),
            textOutput("intOut")
        ),
        mainPanel( 
            plotOutput("plot3", brush=brushOpts(id="brush1"))
        )
    )
))

