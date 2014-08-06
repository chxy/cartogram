library(shiny)

shinyUI(pageWithSidebar(
    
    headerPanel("Diffusion-based Cartogram"),
    
    sidebarPanel(
        sliderInput("nrows", 
                    "Grid resolution:", 
                    min = 10, 
                    max = 300, 
                    value = 50,
                    step = 10),
        
        sliderInput("diffuse", 
                    "Diffusing/shrinking rate:", 
                    min = 0.01, 
                    max = 10, 
                    value = 2,
                    step = 0.01),
        
        sliderInput("blank.init",
                    "Weight to fill in the blank of the grid:",
                    min = 0.1, 
                    max = 1, 
                    value = 0.8,
                    step = 0.01),
        
        sliderInput("interpolation",
                    "Interpolation between the cartogram and original map:",
                    min = 0, 
                    max = 1, 
                    #animate=TRUE,
                    value = 1,
                    step = 0.01)#,
        
        #submitButton(text = "Update View")
    ),
    
    mainPanel(
        
        plotOutput("distPlot",width="100%", height="430px"),
        
        textOutput('shapediff'),
        
        textOutput('sizediff')
       
    )
))
