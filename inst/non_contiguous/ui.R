library(shiny)

shinyUI(pageWithSidebar(
    
    headerPanel("Non-contiguous Cartogram"),
    
    sidebarPanel(
        sliderInput("ratio", 
                    "Inverse scale of the area:", 
                    min = 0.5, 
                    max = 1, 
                    value = 0.85,
                    step = 0.01),
        
        radioButtons("adjust", 
                    "Adjust the overlapped regions?", 
                    c("Yes" = 1, 
                    "No" = 0)),
        
        radioButtons("ref", 
                     "Use the dorling cartogram as a reference?", 
                     c("Yes" = 1, 
                       "No" = 0))
        
        #submitButton(text = "Update View")
    ),
    
    mainPanel(
        
        plotOutput("distPlot",width="100%", height="345px")
        
    )
))
