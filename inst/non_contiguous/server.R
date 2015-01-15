library(cartogram)
library(shiny)
library(maps)

dat=merge(usCapitals,election2012,by.x='Abbr',by.y='state')[-c(1,12),c(1,6,9:12)]
ratio=dat$electors/dat$TotalSqMi
vote=dat$result
names(ratio)=names(vote)=dat$Abbr
state_nbrs=nbrlist(state$abbr,state$x,state$y,corner=FALSE)
nbrs=state_nbrs[names(state_nbrs) %in% dat$Abbr]
nbrs=lapply(nbrs,function(xv){xv[xv %in% dat$Abbr]})
resd=dorling(dat$Abbr,dat$centroidx,dat$centroidy,sqrt(dat$electors),nbrs,
             iteration=110,name.text=TRUE,dist.ratio=1.2,frame=FALSE,
             col=dat$result, xlab='', ylab='',animation=FALSE)

shinyServer(function(input, output) {
    
  dataInput <- reactive({
    map_scaling(state[,c(5,4,1,2)],ratio=ratio,
                anchor=input$ratio,adjust=(input$adjust=="1"),
                nbr=state_nbrs,
                refloc=if(input$ref=="1"){resd}else{NULL})
  })
  
  output$distPlot <- renderPlot({
    par(mar=c(0,0,0,0))
    plotmap(dataInput(),color=vote,border='white',name=TRUE)
    map('state',add=TRUE,col='grey70')
    
  })
  
})
