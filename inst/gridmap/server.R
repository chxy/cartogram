library(cartogram)
library(shiny)
library(maps)

data(usGeoInfo)
data(crimes)
palette(sample(c(rainbow(24),colors()[c(1,4:11,13:26,28:29,76:87)*5+3]),48,rep=FALSE))

unipoly=!duplicated(state$polygon)
statelabel=state$state[unipoly]
names(statelabel)=state$polygon[unipoly]

murder=crimes[,5]/crimes$population
names(murder)=crimes$state

shinyServer(function(input, output) {
 
    output$origPlot <- renderPlot({
        
        gridmap=checkerboard(state$x,state$y,state$polygon,statelabel,nbins=input$resolution,plot=FALSE)
        plot(y~x,data=gridmap,pch=15,col=gridmap$label)
        map("state",add=TRUE)
        text(usCapitals$Longitude,usCapitals$Latitude,usCapitals$Abbr,cex=0.8)
        
    })
    
    output$distPlot <- renderPlot({
        
        gridmap=checkerboard(state$x,state$y,state$polygon,statelabel,nbins=input$resolution,plot=FALSE)
        newgrid=grid_cart(gridmap,murder,iteration=input$iteration,animation=FALSE)
        
    })
})
