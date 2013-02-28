data(usGeoInfo)

library(maps)
library(plyr)
library(sp)

polyrange=ddply(state,"polygon",summarize,xmin=min(x),xmax=max(x),ymin=min(y),ymax=max(y))
xgrid=sort(unique(c(polyrange$xmin,polyrange$xmax)),decreasing=FALSE)
ygrid=sort(unique(c(polyrange$ymin,polyrange$ymax)),decreasing=FALSE)

plot(range(xgrid),range(ygrid),type='n')
abline(v=xgrid,lty=3)
abline(h=ygrid,lty=3)
map("state",add=TRUE)

xgrid1=seq(min(xgrid)+0.2,max(xgrid),by=0.5)
ygrid1=seq(min(ygrid)+0.1,max(ygrid),by=0.25)
gridstate=data.frame(x=rep(xgrid1,each=length(ygrid1)),y=rep(ygrid1,times=length(xgrid1)))
query=apply(gridstate,1,function(x){
    a=which(x[1]>=polyrange$xmin & x[1]<=polyrange$xmax & 
            x[2]>=polyrange$ymin & x[2]<=polyrange$ymax)
    b=rep(NA,4)
    if (length(a)>0) {
        b[1:length(a)]=polyrange$polygon[a]
    }
    return(c(length(a),b))
})
query=t(query)
gridstate[,3:4]=query[,1:2]
colnames(gridstate)[3:4]=c('col','polygon')

idx = which(gridstate$col>1)
for (k in 1:length(idx)){
    i=idx[k]
    pol=na.omit(query[i,2:5])
    ptx=gridstate[i,1]
    pty=gridstate[i,2]
    whichstate=rep(0,length(pol))
    for (j in 1:length(pol)){
        polxy=state[state$polygon==pol[j],1:2]
        whichstate[j]=point.in.polygon(ptx,pty,polxy[,1],polxy[,2])
    }
    if (any(whichstate>0)) {gridstate[i,4]=pol[whichstate>0][1]}
    if (k%%100==0) print(k)
}

idx = which(gridstate$col==1)
for (k in 1:length(idx)){
    i=idx[k]
    pol=query[i,2]
    ptx=gridstate[i,1]
    pty=gridstate[i,2]
    polxy=state[state$polygon==pol,1:2]
    whichstate=point.in.polygon(ptx,pty,polxy[,1],polxy[,2])
    if (whichstate==0) {gridstate[i,4]=NA}
    if (k%%100==0) print(k)
}

gridstate1=gridstate[!is.na(gridstate$polygon),]
plot(y~x,data=gridstate1,pch=15,col=factor(gsub(":.+$","",gridstate1$polygon)))
map("state",add=TRUE)