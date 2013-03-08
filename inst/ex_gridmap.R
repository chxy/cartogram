data(usGeoInfo)
data(crimes)
palette(sample(c(rainbow(24),colors()[c(1,4:11,13:26,28:29,76:87)*5+3]),48,rep=FALSE))

library(maps)
gridstate1=grids[!is.na(grids$poly1),]
gridstate1$state=gsub(":.+$","",gridstate1$poly1)
gridstate1$state=factor(gridstate1$state,levels=names(sort(table(gridstate1$state),decreasing=TRUE)))
plot(y~x,data=gridstate1,pch=15,col=factor(gridstate1$state))
map("state",add=TRUE)

library(randomForest)
mytree=randomForest(state~x+y+(x+y)+(x-y),gridstate1)
plot(y~x,data=gridstate1,pch=15,col=predict(mytree,type = "class"))

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
