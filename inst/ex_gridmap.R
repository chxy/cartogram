data(usGeoInfo)
data(crimes)
#mypal=palette(colors()[c(1:30,71:88)*5+3])
palette(sample(c(rainbow(24),colors()[c(1,4:11,13:26,28:29,76:87)*5+3]),48,rep=FALSE))

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
gridstate1$state=gsub(":.+$","",gridstate1$polygon)
gridstate1$state=factor(gridstate1$state,levels=names(sort(table(gridstate1$state),decreasing=TRUE)))
plot(y~x,data=gridstate1,pch=15,col=factor(gridstate1$state))
map("state",add=TRUE)

a=crimes[,c(1,3,5)]
a=a[-c(2,11),]
a$murder=a[,3]/a[,2]
a=a[,-(2:3)]
a$area=table(gridstate1$state)[a$state]
rownames(a)=a$state
a$goal=round(a$murder*mean(a$area)/mean(a$murder))

gridstate$state=gsub(":.+$","",gridstate$polygon)
gridstate2=gridstate[,c(1,2,5)]
ydist=length(ygrid1)
ncell=nrow(gridstate2)
a$crtarea=table(gridstate2$state)[a$state]

plot(y~x,data=gridstate2,pch=15,col=factor(gridstate2$state,levels=a$state))
text(usCapitals$centroidx,usCapitals$centroidy,usCapitals$Abbr,cex=0.8)
for (k in 1:150){
gridstate3=gridstate2
ord=order(a$goal-a$crtarea)[1:sum((a$goal-a$crtarea)<0)]
for (j in ord){
    idxj = which(gridstate2$state==a$state[j])
    for (i in idxj){
        bottom = i-1
        top    = i+1
        left   = i-ydist
        right  = i+ydist
        if (i %% ydist == 1)  bottom = NA
        if (i %% ydist == 0)  top    = NA
        if (i <= ydist)       left   = NA
        if (i >  ncell-ydist) right  = NA
        fournbrs=c(bottom,top,left,right)
        cell=gridstate3[i,3]
        cellnbrs=gridstate3[fournbrs,3]
        cond1 = all(cellnbrs[1:2]==cell,na.rm=TRUE) & all(cellnbrs[3:4]!=cell,na.rm=TRUE)
        cond2 = all(cellnbrs[1:2]!=cell,na.rm=TRUE) & all(cellnbrs[3:4]==cell,na.rm=TRUE)
        if (any(cellnbrs != cell, na.rm=TRUE) && (!cond1) && (!cond2)) {
            mynbrs=na.omit(cellnbrs[cellnbrs != cell])
            goal=a[mynbrs,'goal']
            candidate=mynbrs[which.max(goal)]
            if ((!candidate %in% a$state[ord[1:j]]) && a[cell,'crtarea']>3) {
                gridstate2[i,3]=candidate
                a$crtarea=table(gridstate2$state)[a$state]
            }
        }
    }
}
cat(k," - ",sum((a$goal-a$crtarea)^2),"\n")
}

library(randomForest)
mytree=randomForest(state~x+y+(x+y)+(x-y),gridstate1)
plot(y~x,data=gridstate1,pch=15,col=predict(mytree,type = "class"))