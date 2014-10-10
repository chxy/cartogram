library(cartogram)
library(shiny)
library(maps)
library(ggplot2)
library(plyr)

dat=merge(usCapitals,election2012,by.x='Abbr',by.y='state')[-c(1,12),c(1,2,6,11:12)]
dat$State = tolower(dat$State)
ratio=dat$electors
vote=dat$result

shinyServer(function(input, output) {
  
  f1 = reactive(Rcartogram(state$x, state$y, state$poly, state$abbr, ratio, nrows=input$nrows, blank.init=input$blank.init, sea.init=input$sea.init, sea.width=input$sea.width, blur=input$blur))
  
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
    tmp = sum(size_diff(crt,ratio))
    paste("Size difference:",round(tmp,3))
  })
  
  output$diffPlot = renderPlot({
    m=20
    p=length(unique(state$abbr))
    res = data.frame(matrix(NA,nrow=(m+1)*p,ncol=4))
    colnames(res) = c('weight','shape','size','state')
    for (i in (0:m)/m) {
      newloc = interpolate(f1(),state,wt=i)
      shape = shape_diff(newloc,state,state$polygon)
      crt = polyarea(newloc,state$polygon,state$abbr)
      crt = cbind(crt,shape)
      dif = cbind(tapply(crt[,3],crt[,2],sum),tapply(crt[,4],crt[,2],sum))
      dif = cbind(dif,size_diff(dif[,1],ratio,TRUE))
      dif[,1] = i
      res[(1:p)+i*m*p,1:3] = dif
      res[(1:p)+i*m*p,4] = rownames(dif)
    }
    res=reshape2::melt(res,c(1,4))
    colnames(res)[3:4]=c('type','difference')
    r=ddply(res,c('weight','type'),summarise,difference=mean(difference))
    qplot(weight,difference,data=res,geom='line',group=state,facets=~type,alpha=I(0.2),xlab='interpolation weight') + geom_line(aes(group=NULL),color=I(7),size=I(2),data=r)
    
  })
})
