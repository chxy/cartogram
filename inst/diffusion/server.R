library(cartogram)
library(shiny)
library(maps)

dat=merge(usCapitals,election2012,by.x='Abbr',by.y='state')[-c(1,12),c(1,2,6,11:12)]
dat$State = tolower(dat$State)
ratio=dat$electors
vote=dat$result

shinyServer(function(input, output) {
  
  f1 = reactive(Rcartogram(state$x, state$y, state$poly, state$abbr, ratio, diffuse=input$diffuse, nrows=input$nrows, blank.init=input$blank.init))
  
  f2 = reactive(interpolate(f1(),state,input$interpolation))
  
  output$distPlot = renderPlot({
    par(mar=c(0,0,0,0))
    plotmap(f2(), color=vote)
  })
  
  output$shapediff = renderText({
    tmp = sum(shape_diff(f2(),state,state$polygon))
    paste("Shape difference:",round(tmp,3))
  })
  
  output$sizediff = renderText({
    crt = polyarea(f2(),state$polygon,state$abbr)
    crt = tapply(crt[,3],crt[,2],sum)
    tmp = size_diff(crt,ratio)
    paste("Size difference:",round(tmp,3))
  })
})
