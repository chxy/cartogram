library(shiny)

shinyUI(pageWithSidebar(
    
    headerPanel("Dorling Cartogram"),
    
    sidebarPanel(

        sliderInput("iteration", 
                    "The number of iterations running with one click :", 
                    min = 1, 
                    max = 201, 
                    value = 11,
                    step = 10),
        
        sliderInput("resolution",
                    "The number of bins by row and column:",
                    min = 10,
                    max = 200,
                    value = 50,
                    step = 10),
        
        submitButton(text = "Update View")
    ),
    
    mainPanel(
        
        plotOutput("distPlot",width="100%", height="430px"),
        
        plotOutput("origPlot",width="100%", height="525px")        
    )
))
