##### Example 1 ##### Presidential Election 2012 #####

dat=merge(usCapitals,election2012,by.x='Abbr',by.y='state')[-c(1,12),c(1,6,9:12)]
ratio=dat$electors/dat$TotalSqMi
vote=dat$result
names(ratio)=names(vote)=dat$Abbr
state_nbrs=nbrlist(state$abbr,state$x,state$y,corner=FALSE)
nbrs=state_nbrs[names(state_nbrs) %in% dat$Abbr]
nbrs=lapply(nbrs,function(xv){xv[xv %in% dat$Abbr]})

res1=map_scaling(state[,c(5,4,1,2)],ratio,0.85)
plotmap(res1,color=vote,border='white',name=TRUE)
map('state',add=T,col='grey70')

res2=map_scaling(state[,c(5,4,1,2)],ratio,0.9,TRUE,state_nbrs)
plotmap(res2,color=vote,border='white',name=TRUE)
map('state',add=T,col='grey70')

resd=dorling(dat$Abbr,dat$centroidx,dat$centroidy,sqrt(dat$electors),nbrs,
             iteration=110,name.text=TRUE,dist.ratio=1.2,frame=FALSE,
             col=dat$result, xlab='', ylab='')
res3=map_scaling(state[,c(5,4,1,2)],ratio,0.87,TRUE,state_nbrs,resd)
plotmap(res3,color=vote,border='white',name=TRUE)
map('state',add=T,col='grey70')


##### Example 2 ##### Water area proportion #####

r=usCapitals[,c(1,2,6,8)]
r$WaterRatio=r$WaterSqMi/r$TotalSqMi
vote = r$WaterRatio
names(vote)=r$Abbr
res=map_scaling(state[,c(5,4,1,2)],vote,1,FALSE)
plotmap(res,color='lightblue',border='white',name=FALSE)

res$state=gsub(":.*$","",res$poly)
statel=unique(res[,c('poly','state','abbr')])
statelabel=statel$state
names(statelabel)=statel$poly
bin=100
gridmap=checkerboard(res$x,res$y,res$poly,statelabel,nbins=bin,plot=TRUE)

grid1=gridmap[order(gridmap$y,gridmap$x),]
grid1$x=rep(1:bin,bin)
grid1$y=rep(1:bin,each=bin)
grid1=grid1[!is.na(grid1$poly),c(1,2,4)]
usC=usCapitals[,1:2]
rownames(usC)=tolower(usC$State)
grid1$label=usC[as.character(grid1$label),'Abbr']
grid1$label=factor(grid1$label,levels=sort(unique(as.character(grid1$label))))
xcenter=tapply(grid1$x,grid1$label,mean)
ycenter=tapply(grid1$y,grid1$label,mean)

library(plyr)
rad=ddply(grid1,'label',summarise,r=sqrt((diff(range(x))/2)^2+(diff(range(y))/2)^2))
rownames(rad)=rad$label
circleDist=outer(rad$r,rad$r,"+")
diag(circleDist)=0
colnames(circleDist)=rownames(circleDist)=rad$label

crtloc=frc=data.frame(x=xcenter,y=ycenter)
origindist=as.matrix(dist(crtloc))
nbr=statenbrs[levels(grid1$label)]
dist.ratio=2
shared.border=NULL
grid2=grid1

pal=palette(sample(c(rainbow(24),colors()[c(1,4:11,13:26,28:29,76:87)*5+3]),48,rep=FALSE))
for (s in 1:15){
    grid2=grid2[!is.na(grid2$label),]
    crtDist=as.matrix(dist(crtloc))
    frc$xforce=frc$yforce=frc$xattract=frc$yattract=frc$xrepel=frc$yrepel=0.00000
    
    # Calculate the repel force
    idx = circleDist > crtDist
    idx = idx & lower.tri(idx)
    err = circleDist-crtDist
    for (i in which(rowSums(idx)>0)){
        for (j in which(idx[i,1:(i-1)])){
            ratio=(circleDist[i,j]-crtDist[i,j])/2/crtDist[i,j]
            frc$xrepel[i]=frc$xrepel[i]+ratio*(crtloc$x[i]-crtloc$x[j])
            frc$xrepel[j]=frc$xrepel[j]+ratio*(crtloc$x[j]-crtloc$x[i])
            frc$yrepel[i]=frc$yrepel[i]+ratio*(crtloc$y[i]-crtloc$y[j])
            frc$yrepel[j]=frc$yrepel[j]+ratio*(crtloc$y[j]-crtloc$y[i])
        }
    }
    # Calculate the attract force
    for (i in 1:length(nbr)){
        for (j in 1:length(nbr[[i]])){
            crtstate=names(nbr)[i]
            crtnbr=which(rownames(crtDist)==nbr[[i]][j])
            if (length(crtnbr)>0) {
                distratio=crtDist[crtstate,crtnbr]/circleDist[crtstate,crtnbr]
                if (distratio > dist.ratio & crtDist[crtstate,crtnbr]>origindist[crtstate,crtnbr]){
                    border_ratio=ifelse(is.null(shared.border),1/(round(s/10)+8),shared.border[crtstate,nbr[[i]][j]]/shared.border[crtstate,crtstate])
                    ratio=(crtDist[crtstate,crtnbr]-circleDist[crtstate,crtnbr])/crtDist[crtstate,crtnbr]*border_ratio
                    frc$xattract[i]=frc$xattract[i]-ratio*(crtloc$x[i]-crtloc$x[crtnbr])
                    frc$xattract[crtnbr]=frc$xattract[crtnbr]-ratio*(crtloc$x[crtnbr]-crtloc$x[i])
                    frc$yattract[i]=frc$yattract[i]-ratio*(crtloc$y[i]-crtloc$y[crtnbr])
                    frc$yattract[crtnbr]=frc$yattract[crtnbr]-ratio*(crtloc$y[crtnbr]-crtloc$y[i])
                }
            }
        }
    }
    
    frc$xforce=frc$xrepel+frc$xattract
    frc$yforce=frc$yrepel+frc$yattract
    crtloc=crtloc+frc[,8:7]
    
    grid2$x=grid2$x+round(frc[grid2$label,'xforce'])
    grid2$y=grid2$y+round(frc[grid2$label,'yforce'])
    image2=complete.image(grid2)
    grid2=image2$df
    image(image2$matrix,col=pal,xlab='',ylab='',xaxt='n',yaxt='n',frame=F)
    points(crtloc$x/bin,crtloc$y/bin,pch=21,cex=rad$r)
    Sys.sleep(0.2)
}
