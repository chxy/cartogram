data(usGeoInfo)
data(crimes)
palette(sample(c(rainbow(24),colors()[c(1,4:11,13:26,28:29,76:87)*5+3]),48,rep=FALSE))

unipoly=!duplicated(state$polygon)
statelabel=state$state[unipoly]
names(statelabel)=state$polygon[unipoly]
gridmap=checkerboard(state$x,state$y,state$polygon,statelabel)
library(maps)
map("state",add=TRUE)

library(randomForest)
mygrid=gridmap[!is.na(gridmap$label),c(1,2,4)]
mygrid$label=as.factor(as.character(mygrid$label))
mytree=randomForest(label~x+y+(x+y)+(x-y),data=mygrid)
plot(y~x,data=mygrid,pch=15,col=predict(mytree,type = "class"))

a=crimes[,c(1,3,5)]
a=a[-c(2,11),]
a$murder=a[,3]/a[,2]
a=a[,-(2:3)]
a$area=table(gridmap$label)[a$state]
rownames(a)=a$state
a$goal=round(a$murder*mean(a$area)/mean(a$murder))

ydist=sum(gridmap$x==gridmap$x[1])
ncell=nrow(mygrid)
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
