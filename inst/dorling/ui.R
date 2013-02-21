library(shiny)

shinyUI(pageWithSidebar(
    
    headerPanel("Dorling Cartogram"),
    
    sidebarPanel(
        sliderInput("distratio", 
                    "The attract force happens when the distance is greater than ___ times the sum of radii :", 
                    min = 1, 
                    max = 3, 
                    value = 1.2,
                    step = 0.1),
        
        sliderInput("iteration", 
                    "The number of iterations running with one click :", 
                    min = 1, 
                    max = 201, 
                    value = 11,
                    step = 10),
        
        radioButtons("shape",
                     "The shape of polygons :",
                     c("Circle"=100, "Triangle"=3, "Square"=1,
                       "Diamond"=4, "Hexagon"=6, "Octagon"=8)),
        
        submitButton(text = "Update View")
    ),
    
    mainPanel(
        
        plotOutput("distPlot",width="100%", height="430px"),
        
        plotOutput("origPlot",width="100%", height="525px")        
    )
))
