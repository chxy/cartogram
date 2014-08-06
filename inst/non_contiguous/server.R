library(cartogram)
library(shiny)
library(maps)

dat=merge(usCapitals,election2012,by.x='Abbr',by.y='state')[-c(1,12),c(1,6,11:12)]
ratio=dat$electors/dat$TotalSqMi*2000
#ratio=dat$electors
vote=dat$result
names(ratio)=names(vote)=dat$Abbr

shinyServer(function(input, output) {
    
    output$distPlot <- renderPlot({
      par(mar=c(0,0,0,0))
      res=map_scaling(state[,c(5,4,1,2)],ratio*input$ratio,vote,'white',FALSE,FALSE)
      map('state',add=T,col='grey70')
      
    })
})
