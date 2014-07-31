library(shiny)
library(maps)

dat=merge(usCapitals,election2012,by.x='Abbr',by.y='state')[-c(1,12),c(1,2,6,11:12)]
dat$State = tolower(dat$State)
ratio=dat$electors
vote=dat$result

shinyServer(function(input, output) {
  output$distPlot <- renderPlot({
    res = Rcartogram(state$x, state$y, state$poly, state$abbr, ratio, color=vote, diffuse=input$diffuse, nrows=input$nrows, blank.init=input$blank.init, interpolate=input$interpolation)
  })
})
