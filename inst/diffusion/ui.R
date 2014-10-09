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
        
        sliderInput("blank.init", 
                    "Weight to fill in the blank of the grid:", 
                    min = 0, 
                    max = 1, 
                    value = 0.2,
                    step = 0.01),
        
        sliderInput("sea.init",
                    "Weight to fill in the sea:",
                    min = 0, 
                    max = 1, 
                    value = 0.07,
                    step = 0.01),
        
        sliderInput("sea.width",
                    "Width of the sea:",
                    min = 0.1, 
                    max = 3, 
                    value = 0.6,
                    step = 0.1),
        
        sliderInput("blur",
                    "Gaussian smoothing parameter for the diffusion:",
                    min = 0, 
                    max = 3, 
                    value = 0,
                    step = 0.1),
        
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
