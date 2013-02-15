library(shiny)

shinyUI(pageWithSidebar(
    
    headerPanel("Dorling Cartogram"),
    
    sidebarPanel(
        sliderInput("distratio", 
                    "The attract force happens when the distance is greater than ___ times the sum of radii :", 
                    min = 1, 
                    max = 5, 
                    value = 2,
                    step = 0.2),
        
        sliderInput("iteration", 
                    "The number of iterations running with one click :", 
                    min = 1, 
                    max = 201, 
                    value = 51,
                    step = 10),
        
        # submitButton(text = "Update View")
    ),
    
    mainPanel(
        
        plotOutput("distPlot",width="100%", height="525px"),
        
        plotOutput("origPlot",width="100%", height="525px")        
    )
))
