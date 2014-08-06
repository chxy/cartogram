library(shiny)

shinyUI(pageWithSidebar(
    
    headerPanel("Non-contiguous Cartogram"),
    
    sidebarPanel(
        sliderInput("ratio", 
                    "Scale of the area:", 
                    min = 0.05, 
                    max = 1, 
                    value = 1,
                    step = 0.05)#,
        
        #submitButton(text = "Update View")
    ),
    
    mainPanel(
        
        plotOutput("distPlot",width="100%", height="345px")
        
    )
))
