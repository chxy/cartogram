library(shiny)

shinyUI(navbarPage("carto",
                   tabPanel("Diffusion Cartograms",
                            sidebarLayout(
                              sidebarPanel(
                                sliderInput("nrows", 
                                            "Grid resolution (row):", 
                                            min = 10, 
                                            max = 200, 
                                            value = 100,
                                            step = 10),
                                
                                sliderInput("ncols", 
                                            "Grid resolution (column):", 
                                            min = 10, 
                                            max = 200, 
                                            value = 60,
                                            step = 10),
                                
                                sliderInput("blank.init", 
                                            "Weight to fill in the blank of the grid:", 
                                            min = -0.1, 
                                            max = 1, 
                                            value = 0.1,
                                            step = 0.05),
                                
                                sliderInput("sea.init",
                                            "Weight to fill in the sea:",
                                            min = -0.1, 
                                            max = 1, 
                                            value = 0.1,
                                            step = 0.05),
                                
                                sliderInput("sea.width",
                                            "Width of the sea:",
                                            min = 0.1, 
                                            max = 5, 
                                            value = 2.1,
                                            step = 0.5),
                                
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
                                textOutput('sizediff'),
                                textOutput('totaldiff'),
                                plotOutput("distScatter",width="100%", height="350px")
                                #dataTableOutput("diffTable")
                              )
                            )
                   ),
                   tabPanel("Evaluation",
                            sidebarLayout(
                              sidebarPanel(
                                selectInput('selectedState', 'State',
                                            c(Choose='', unique(state$abbr)),
                                            selectize=FALSE),
                                plotOutput("diffPlot",width="100%", height="550px")
                              ),
                              mainPanel(
                                plotOutput("polygons",width="100%", height="250px"),
                                fluidRow(
                                  column(4,
                                         textOutput('shapediff1'),
                                         textOutput('sizediff1'),
                                         textOutput('size1'),
                                         textOutput('size0')
                                  ),
                                  column(4,
                                         textOutput('shapediff2'),
                                         textOutput('sizediff2'),
                                         textOutput('size2')
                                  ),
                                  column(4,
                                         textOutput('shapediff3'),
                                         textOutput('sizediff3'),
                                         textOutput('size3')
                                  )
                                ),
                                hr(),
                                plotOutput("angledge",width="100%", height="250px")
                              )
                            )
                   ),
                   tabPanel("Vertices",
                            verbatimTextOutput("vertexSummary"),
                            dataTableOutput("vertex")
                   )
))
