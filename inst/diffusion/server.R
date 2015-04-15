library(cartogram)
library(shiny)
library(maps)
library(ggplot2)
library(plyr)

dat=merge(usCapitals,election2012,by.x='Abbr',by.y='state')[-c(1,12),c(1,2,6,11:12)]
dat$State = tolower(dat$State)
ratio=dat$electors
#ratio=sample(dat$electors)
vote=dat$result
names(ratio)=names(vote)=dat$Abbr
perim = perimeter(state$abbr,state$polygon,state$x,state$y)

shinyServer(function(input, output) {
  
  f1 = reactive(Rcartogram(state$x, state$y, state$poly, state$abbr, ratio, nrows=input$nrows, ncols=input$ncols, blank.init=input$blank.init, sea.init=input$sea.init, sea.width=input$sea.width, blur=input$blur))
  
  f2 = reactive(interpolate(f1(),state,input$interpolation))
  
  tmp1 = reactive(state[state$abbr %in% input$selectedState,])
  tmp2 = reactive(f2()[f2()$abbr %in% input$selectedState,])
  tmp3 = reactive(f1()[f1()$abbr %in% input$selectedState,])
  
  f3 = reactive({
    m=20
    p=length(unique(state$abbr))
    intpltn = data.frame(matrix(NA,nrow=(m+1)*p,ncol=5))
    colnames(intpltn) = c('weight','shape','area','state','sizepct')
    for (i in (0:m)/m) {
      newloc = interpolate(f1(),state,wt=i)
      shape = shape_diff(newloc,state,state$polygon)
      crt = polyarea(newloc,state$polygon,state$abbr)
      crt = cbind(crt,shape)
      dif = cbind(tapply(crt[,3],crt[,2],sum),tapply(crt[,4],crt[,2],sum))
      dif = cbind(dif,size_diff(dif[,1],ratio,mean(perim))*2,dif[,1]/sum(dif[,1]))
      dif[,1] = i
      intpltn[(1:p)+i*m*p,c(1:3,5)] = dif
      intpltn[(1:p)+i*m*p,4] = rownames(dif)
    }
    intpltn
  })
  
  f4 = reactive({
    co = state[state$abbr==input$selectedState,]
    co1 = f1()[f1()$abbr==input$selectedState,]
    unipoly = unique(co$polygon)
    p = data.frame()
    for (i in 1:length(unipoly)){
      idx = co$polygon %in% unipoly[i]
      d=check2poly(co$x[idx],co$y[idx],co1$x[idx],co1$y[idx])
      p = rbind(p,attr(d,'part'))
    }
    p$wad = p$ad*(p$l1f+p$l2f)/2
    p$wed = p$ed*(p$l1f+p$l2f)/2
    p
  })
  
  f5 = reactive({
    tmp = f3()
    tmp[,5] = tmp[,2]+tmp[,3]
    names(tmp)[5] = 'total'
    res=reshape2::melt(tmp,c(1,4))
    colnames(res)[3:4]=c('type','difference')
    res
  })
  
  output$distPlot = renderPlot({
    par(mar=c(0,0,0,0))
    plotmap(f2(), color=vote)
  })
  
  output$shapediff = renderText({
    tmp = sum(shape_diff(f2(),state,state$polygon))
    paste("Shape error:",round(tmp,3))
  })
  
  output$sizediff = renderText({
    crt = polyarea(f2(),state$polygon,state$abbr)
    crt = tapply(crt[,3],crt[,2],sum)
    tmp = sum(size_diff(crt,ratio,mean(perim))*2)
    paste("Area error:",round(tmp,3))
  })
  
  output$totaldiff = renderText({
    crt = polyarea(f2(),state$polygon,state$abbr)
    crt = tapply(crt[,3],crt[,2],sum)
    tmp1 = sum(size_diff(crt,ratio,mean(perim))*2)
    tmp2 = sum(shape_diff(f2(),state,state$polygon))
    paste("Total error:",round(tmp1+tmp2,3))
  })
  
  output$distScatter = renderPlot({
    res = f3()[which(abs(f3()$weight-input$interpolation)<0.025),1:4]
    names(res) = c('interpolation','shape error','area error','state')
    r=ddply(f5()[f5()$type=='total',c(1,4)],'weight',summarise,difference=sum(difference))
    par(mfrow=c(1,2))
    plot(res[,2:3],type='n')
    text(res[,2:3],labels=res[,4])
    plot(r[,1:2],type='l',xlab='interpolation weight',ylab='total error')
    abline(v=input$interpolation,col=3)
  })
  
  output$diffTable = renderDataTable({
    res = f3()[which(abs(f3()$weight-input$interpolation)<0.025),1:4]
    names(res) = c('interpolation','shape error','area error','state')
    res
  }, options=list(pageLength=50))
  
  output$shapediff1 = renderText({
    tmp = f3()[f3()$state %in% input$selectedState & f3()$weight==0,]
    paste("Shape error:",round(tmp[2],3))
  })
  
  output$sizediff1 = renderText({
    tmp = f3()[f3()$state %in% input$selectedState & f3()$weight==0,]
    paste("Area error:",round(tmp[3],3))
  })
  
  output$size1 = renderText({
    tmp = f3()[f3()$state %in% input$selectedState & f3()$weight==0,]
    paste("Proportional area:",round(tmp[5],3))
  })
  
  output$size0 = renderText({
    tmp = ratio[input$selectedState]/sum(ratio)
    paste("Desired prop area:",round(tmp,3))
  })
  
  output$shapediff2 = renderText({
    tmp = f3()[f3()$state %in% input$selectedState & abs(f3()$weight-input$interpolation)<0.025,]
    paste("Shape error:",round(tmp[2],3))
  })
  
  output$sizediff2 = renderText({
    tmp = f3()[f3()$state %in% input$selectedState & abs(f3()$weight-input$interpolation)<0.025,]
    paste("Area error:",round(tmp[3],3))
  })
  
  output$size2 = renderText({
    tmp = f3()[f3()$state %in% input$selectedState & abs(f3()$weight-input$interpolation)<0.025,]
    paste("Proportional area:",round(tmp[5],3))
  })
  
  output$shapediff3 = renderText({
    tmp = f3()[f3()$state %in% input$selectedState & f3()$weight==1,]
    paste("Shape error:",round(tmp[2],3))
  })
  
  output$sizediff3 = renderText({
    tmp = f3()[f3()$state %in% input$selectedState & f3()$weight==1,]
    paste("Area error:",round(tmp[3],3))
  })
  
  output$size3 = renderText({
    tmp = f3()[f3()$state %in% input$selectedState & f3()$weight==1,]
    paste("Proportional area:",round(tmp[5],3))
  })
  
  output$diffPlot = renderPlot({
    res = f5()
    r=ddply(res,c('weight','type'),summarise,difference=mean(difference))
    sl=res[res$state %in% input$selectedState,]
    qplot(weight,difference,data=res,geom='line',group=state,alpha=I(0.2),xlab='interpolation weight',ylab='error') + geom_line(aes(group=NULL),color=I(1),size=I(1.5),data=r) + geom_vline(xintercept = input$interpolation) + geom_line(color=I(7),size=I(2),data=sl) + facet_grid(facets=type~.,scales="free_y")
  })
  
  output$polygons = renderPlot({
    if (input$selectedState == '') return()
    par(mfrow=c(1,3))
    plotmap(tmp1(),color=0,border=1,main='Original map')
    plotmap(tmp2(),color=0,border=1,main='Interpolation')
    plotmap(tmp3(),color=0,border=1,main='Cartogram')
  })
  
  output$angledge = renderPlot({
    if (input$selectedState == '') return()
    co = state[state$abbr==input$selectedState,]
    co1 = f1()[f1()$abbr==input$selectedState,]
    p = f4()
    thres = 0.97
    wadthres = quantile(p$wad,thres)
    wedthres = quantile(p$wed,thres)
    par(mfrow=c(1,3))
    plot(co[,1:2],pch=19,cex=0.3,frame=FALSE,xaxt='n',yaxt='n',xlab='',ylab='',main='Original Map')
    points(co[p$wad>wadthres & p$wed<=wedthres,1:2],col=4,pch=15,cex=1.5)
    points(co[p$wad<=wadthres & p$wed>wedthres,1:2],col=3,pch=17,cex=1.5)
    points(co[p$wad>wadthres & p$wed>wedthres,1:2],col=2,pch=19,cex=1.5)
    plot(p[,c('wad','wed')],pch=19,cex=0.3,xlab='Angle difference',ylab='Edge difference',main=paste0('Color threshold: top ',100*(1-thres),'%'))
    points(p[p$wad>wadthres & p$wed<=wedthres,c('wad','wed')],col=4,pch=15,cex=1.5)
    points(p[p$wad<=wadthres & p$wed>wedthres,c('wad','wed')],col=3,pch=17,cex=1.5)
    points(p[p$wad>wadthres & p$wed>wedthres,c('wad','wed')],col=2,pch=19,cex=1.5)
    plot(co1[,1:2],pch=19,cex=0.3,frame=FALSE,xaxt='n',yaxt='n',xlab='',ylab='',main='Cartogram')
    points(co1[p$wad>wadthres & p$wed<=wedthres,1:2],col=4,pch=15,cex=1.5)
    points(co1[p$wad<=wadthres & p$wed>wedthres,1:2],col=3,pch=17,cex=1.5)
    points(co1[p$wad>wadthres & p$wed>wedthres,1:2],col=2,pch=19,cex=1.5)
  })
  
  output$vertexSummary = renderPrint({
    res = f4()[,c(7,1:3,5)]
    res[,1] = 1:nrow(res)
    names(res) = c('order','angle diff','edge diff','orig edge length','trans edge length')
    summary(res)
  })
  
  output$vertex = renderDataTable({
    res = round(f4()[,c(7,1:3,5)],5)
    res[,1] = 1:nrow(res)
    colnames(res) = c('order','angle diff','edge diff','orig edge length','trans edge length')
    res
  }, options=list(pageLength=15))
})
