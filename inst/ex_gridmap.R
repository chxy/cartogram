data(usGeoInfo)
data(crimes)
palette(sample(c(rainbow(24),colors()[c(1,4:11,13:26,28:29,76:87)*5+3]),48,rep=FALSE))

unipoly=!duplicated(state$polygon)
statelabel=state$state[unipoly]
names(statelabel)=state$polygon[unipoly]
gridmap=checkerboard(state$x,state$y,state$polygon,statelabel,nbins=100,plot=FALSE)
state_nbrs=nbrlist(state$state,state$x,state$y,corner=FALSE)
color=fct(state_nbrs)
plot(y~x,data=gridmap,pch=15,col=1+color[as.character(gridmap$label)])
library(ggplot2)
qplot(x,y,data=gridmap,geom='tile',fill=label) + theme_bw() + theme(legend.position="none")
library(maps)
map("state",add=TRUE)

library(randomForest)
mygrid=gridmap[!is.na(gridmap$label),c(1,2,4)]
mygrid$label=as.factor(as.character(mygrid$label))
mytree=randomForest(label~x+y+(x+y)+(x-y),data=mygrid)
plot(y~x,data=mygrid,pch=15,col=predict(mytree,type = "class"))

murder=crimes[,5]/crimes$population
names(murder)=crimes$state
newgrid=grid_cart(gridmap,murder,iteration=200)
newgrid=grid_cart(gridmap,murder,iteration=100,animation=TRUE)
newgrid=grid_cart(gridmap,murder,iteration=50,animation=TRUE,preserve.sea=FALSE)
text(usCapitals$centroidx,usCapitals$centroidy,usCapitals$Abbr,cex=0.8)
plot(y~x,data=newgrid$grids,pch=15,col=factor(newgrid$grids$label,levels=levels(gridmap$label)))
plot(newgrid$error$SSE,type='l')
plot(newgrid$error$AE,type='l')