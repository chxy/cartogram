data(usGeoInfo)
data(crimes)
palette(sample(c(rainbow(24),colors()[c(1,4:11,13:26,28:29,76:87)*5+3]),48,rep=FALSE))

unipoly=!duplicated(state$polygon)
statelabel=state$state[unipoly]
names(statelabel)=state$polygon[unipoly]

state_nbrs=nbrlist(state$state,state$x,state$y,corner=FALSE)
color=fct(state_nbrs)

gridmap=checkerboard(state$x,state$y,state$polygon,statelabel,nbins=50,plot=TRUE,pal=color[statelabel])
plot(y~x,data=gridmap,pch=15,col=1+color[as.character(gridmap$label)],cex=1.7)
tmplabel=as.integer(as.character(factor(gridmap$label,levels=statelabel,labels=color[statelabel])))
image(unique(gridmap$x),unique(gridmap$y),matrix(tmplabel,nrow=length(unique(gridmap$x)),ncol=length(unique(gridmap$y))),col=rainbow(5),xlab='',ylab='',xaxt='n',yaxt='n',frame=F)

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

newgrid2=pan_cart(gridmap,murder)
tmplabel=as.integer(as.character(factor(newgrid2$grids$label,levels=statelabel,labels=color[statelabel])))
image(unique(newgrid2$grids$x),unique(newgrid2$grids$y),matrix(tmplabel,nrow=length(unique(newgrid2$grids$x)),ncol=length(unique(newgrid2$grids$y))),col=rainbow(5),xlab='',ylab='',xaxt='n',yaxt='n',frame=F)

################################################################################

grid.sim = function(nr=15,nc=15,g=12,seed=100){
    set.seed(seed)
    p=palette(sample(c(rainbow(24),colors()[c(1,4:11,13:26,28:29,76:87)*5+3]),g,rep=FALSE))
    a=matrix(rpois(nr*nc,2),nrow=nr,ncol=nc)
    b=cbind(expand.grid(1:nr,1:nc),as.vector(a))
    colnames(b)=c('x','y','v')
    xmean=mean(b$x); xstd=sd(b$x)
    ymean=mean(b$y); ystd=sd(b$y)
    b$w=((b$x-xmean)/xstd)^2+((b$y-ymean)/ystd)^2
    b$m=kmeans(b[,c('x','y','v','x','y','w')],g,iter.max=100)$cluster
    b$d=round(5*sin(exp(b$m))+10,3)
    b$label=as.factor(LETTERS[b$m])
    r=matrix(b$m,nrow=nr,ncol=nc)
    image(1:nr,1:nc,r,col=p,xlab='',ylab='',xaxt='n',yaxt='n',frame=F,asp=nc/nr)
    b[,c('x','y','m','label','d')]
}

s=grid.sim(16,12,10)

d1=s[!duplicated(s[,4:5]),4:5]
d=d1$d
names(d)=d1$label

grid1=grid_cart(s[,c(1,2,4,4)],d,iteration=30,animation=TRUE)
grid2=pan_cart(s[,c(1,2,4,4)],d)
