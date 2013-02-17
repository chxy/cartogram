library(shiny)
library(maps)

load('../../data/usGeoInfo.rda')
load('../../data/crimes.rda')
dat=merge(usCapitals,crimes,by.x='Abbr',by.y='abbr')[,c(1,2,4,5,6,9,10,12)]
dat=dat[-which(dat$Abbr %in% c('AK','HI')),]
nbrs=statenbrs[names(statenbrs) %in% dat$Abbr]
nbrs=lapply(nbrs,function(xv){xv[xv %in% dat$Abbr]})
dat$density=sqrt(dat$population/dat$TotalSqMi)
origindist=as.matrix(dist(dat[,3:4]))
dat$density=dat$density/max(dat$density)*mean(origindist)/5

shinyServer(function(input, output) {

    output$origPlot <- reactivePlot(function() {

        plot(y~x,state,type='n',xlab="",ylab="",frame=FALSE,xaxt='n',yaxt='n')
        map("state",add=TRUE)
        points(Latitude~Longitude,dat,type='p',col=2,pch=20)
        circle(dat$Longitude,dat$Latitude,dat$density,col='pink')
        text(dat$Longitude,dat$Latitude,dat$Abbr,cex=0.8)
    })
    
    output$distPlot <- reactivePlot(function() {
               
        dorling(dat$Abbr,dat$Longitude,dat$Latitude,dat$density,nbrs,dist.ratio=input$distratio, iteration=input$iteration, animation=FALSE)
                
    })
})
