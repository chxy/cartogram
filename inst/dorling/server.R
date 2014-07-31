library(shiny)
library(maps)

data(usGeoInfo)
data(crimes)
dat=merge(usCapitals,crimes,by.x='Abbr',by.y='abbr')[,c(1,2,4,5,6,9,10,12)]
dat=dat[-which(dat$Abbr %in% c('AK','HI')),]
nbrs=statenbrs[names(statenbrs) %in% dat$Abbr]
nbrs=lapply(nbrs,function(xv){xv[xv %in% dat$Abbr]})
dat$density=sqrt(dat$population/dat$TotalSqMi)
origindist=as.matrix(dist(dat[,3:4]))
dat$density=dat$density/max(dat$density)*mean(origindist)/5

shinyServer(function(input, output) {

    output$origPlot <- renderPlot({

        plot(y~x,state,type='n',xlab="",ylab="",frame=FALSE,xaxt='n',yaxt='n')
        map("state",add=TRUE)
        points(Latitude~Longitude,dat,type='p',col=2, pch=20)
        if (input$shape==1){
            circle(dat$Longitude,dat$Latitude,dat$density,square=TRUE,col='pink')
        }else{
            circle(dat$Longitude,dat$Latitude,dat$density,vertex=as.numeric(input$shape),col='pink')
        }
        text(dat$Longitude,dat$Latitude,dat$Abbr,cex=0.8)
    })
    
    output$distPlot <- renderPlot({
        
        if (input$shape==1) {
            dorling(dat$Abbr,dat$Longitude,dat$Latitude,dat$density,nbrs,dist.ratio=input$distratio, iteration=input$iteration, animation=FALSE, col='pink', square=TRUE)
        } else {
            dorling(dat$Abbr,dat$Longitude,dat$Latitude,dat$density,nbrs,dist.ratio=input$distratio, iteration=input$iteration, polygon.vertex=as.numeric(input$shape), animation=FALSE, col='pink')
        }             
    })
})
