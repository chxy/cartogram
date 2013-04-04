data(usGeoInfo)

r=usCapitals[,c(1,2,6,8)]
r$WaterRatio=r$WaterSqMi/r$TotalSqMi*5
res=map_scaling(state[,c(4,1,2)],r[,c(1,4)],TRUE)

r$State=tolower(r$State)
polyname=state[!duplicated(state[,4:5]),3:5]
r=merge(polyname,r,by.x=1,by.y=1)
res=map_scaling(state[,c(5,1,2)],r[,c(3,7)],TRUE)

res1=res
idx=setdiff(1:nrow(res1),rownames(res1))
res1[(1:length(idx))+nrow(res1),]=NA
rownames(res1)[nrow(res1)+1-(1:length(idx))]=idx
res1=res1[order(as.integer(rownames(res1))),]
plot(res1[,2],res1[,3],type='l',xlab='longitude',ylab='latitude')

res$state=gsub(":.*$","",res$polygon)
unipoly=!duplicated(res$polygon)
statelabel=res$state[unipoly]
names(statelabel)=res$polygon[unipoly]
gridmap=checkerboard(res$x,res$y,res$polygon,statelabel,nbins=100,plot=TRUE)

grid1=gridmap[!is.na(gridmap$poly),c(1,2,4)]
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

for (s in 1:100){
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
    
    grid2$x=grid2$x+frc[grid2$label,'xforce']
    grid2$y=grid2$y+frc[grid2$label,'yforce']
    grid2=complete(grid2)
    image(unique(grid2$x),unique(grid2$y),matrix(as.integer(factor(grid2$label,levels=levels(grid1$label))),nrow=length(unique(grid2$x)),ncol=length(unique(grid2$y)),byrow=T),col=pal,xlab='',ylab='',xaxt='n',yaxt='n',frame=F)
    Sys.sleep(0.2)
}
