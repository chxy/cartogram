data(usGeoInfo)
data(crimes)
pal=palette(sample(c(rainbow(24),colors()[c(1,4:11,13:26,28:29,76:87)*5+3]),48,rep=FALSE))

unipoly=!duplicated(state$polygon)
statelabel=state$state[unipoly]
names(statelabel)=state$polygon[unipoly]

gridmap=checkerboard(state$x,state$y,state$polygon,statelabel,nbins=50,plot=TRUE)

state_nbrs=nbrlist(state$state,state$x,state$y,corner=FALSE)
color=fct(state_nbrs)
tmplabel=as.integer(as.character(factor(gridmap$label,levels=statelabel,labels=color[statelabel])))
image(unique(gridmap$x),unique(gridmap$y),matrix(tmplabel,nrow=length(unique(gridmap$x)),ncol=length(unique(gridmap$y))),col=rainbow(5),xlab='',ylab='',xaxt='n',yaxt='n',frame=F)
plot(y~x,data=gridmap,pch=15,col=1+color[as.character(gridmap$label)],cex=1.7)

library(maps)
map("state",add=TRUE,col='black')

library(randomForest)
mygrid=gridmap[!is.na(gridmap$label),c(1,2,4)]
mygrid$label=as.factor(as.character(mygrid$label))
mytree=randomForest(label~x+y+(x+y)+(x-y),data=mygrid)
mygrid$predict=predict(mytree,type = "class")
rfimage=complete.image(mygrid[,c(1,2,4)])
image(rfimage$matrix,col=pal,xlab='',ylab='',xaxt='n',yaxt='n',frame=F)

murder=crimes[,5]/crimes$population
names(murder)=crimes$state
newgrid=grid_cart(gridmap,murder,iteration=200)
newgrid=grid_cart(gridmap,murder,iteration=100,animation=TRUE)
newgrid=grid_cart(gridmap,murder,iteration=50,animation=TRUE,preserve.sea=FALSE)
text(usCapitals$centroidx,usCapitals$centroidy,usCapitals$Abbr,cex=0.8)
plot(y~x,data=newgrid$grids,pch=15,col=factor(newgrid$grids$label,levels=levels(gridmap$label)))
plot(newgrid$error$SSE,type='l')
plot(newgrid$error$AE,type='l')

newgrid2=pan_cart(gridmap,murder)
tmplabel=as.integer(as.character(factor(newgrid2$grids$label,levels=statelabel,labels=color[statelabel])))
image(unique(newgrid2$grids$x),unique(newgrid2$grids$y),matrix(tmplabel,nrow=length(unique(newgrid2$grids$x)),ncol=length(unique(newgrid2$grids$y))),col=rainbow(5),xlab='',ylab='',xaxt='n',yaxt='n',frame=F)

################################################################################

s=grid.sim(16,12,10)
d1=s[!duplicated(s[,4:5]),4:5]
d=d1$d
names(d)=d1$label

grid1=grid_cart(s[,1:4],d,iteration=20,animation=TRUE,sleep.time=1)
grid1$count
plot(grid1$error$SSE,type='l')
plot(grid1$error$AE,type='l')

grid2=pan_cart(s[,c(1,2,4,4)],d)
grid2$count