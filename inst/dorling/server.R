library(cartogram)
library(shiny)
library(maps)

dat=merge(usCapitals,election2012,by.x='Abbr',by.y='state')[-c(1,12),c(1,6,9:12)]
state_nbrs=nbrlist(state$abbr,state$x,state$y,corner=FALSE)
nbrs=state_nbrs[names(state_nbrs) %in% dat$Abbr]
nbrs=lapply(nbrs,function(xv){xv[xv %in% dat$Abbr]})

shinyServer(function(input, output) {

    output$origPlot <- renderPlot({
        par(mar=c(0,0,0,0))
        plot(y~x,state,type='n',xlab="",ylab="",frame=FALSE,xaxt='n',yaxt='n')
        map("state",add=TRUE)
        points(centroidy~centroidx,dat,type='p',col=2, pch=20)
        if (input$shape==1){
            circle(dat$centroidx,dat$centroidy,sqrt(dat$electors)/2,square=TRUE,col=dat$result)
        }else{
            circle(dat$centroidx,dat$centroidy,sqrt(dat$electors)/2,vertex=as.numeric(input$shape),col=dat$result)
        }
        text(dat$centroidx,dat$centroidy,dat$Abbr,cex=0.8)
    })
    
    output$distPlot <- renderPlot({
      par(mar=c(0,0,0,0))
        if (input$shape==1) {
            dorling(dat$Abbr,dat$centroidx,dat$centroidy,sqrt(dat$electors),nbrs,dist.ratio=input$distratio, iteration=input$iteration, animation=FALSE, col=dat$result, square=TRUE, frame=FALSE, xlab='', ylab='')
        } else {
            dorling(dat$Abbr,dat$centroidx,dat$centroidy,sqrt(dat$electors),nbrs,dist.ratio=input$distratio, iteration=input$iteration, polygon.vertex=as.numeric(input$shape), animation=FALSE, col=dat$result, frame=FALSE,  xlab='', ylab='')
        }             
    })
})
