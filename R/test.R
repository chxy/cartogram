circle = function(xvec,yvec,rvec,border=1,col=NULL){
    # This function is used to compute the locations of the circle border 
    # and draw multiple circles.
    # Borrowing the code from plotrix::draw.circle
    n=length(xvec)
    stopifnot(length(yvec)==n && n==length(rvec))
    if (length(border) < n)  border = rep(border, length.out = n)
    if (!is.null(col) && length(col) < n) col = rep(col, length.out = n)
    # xylim = par("usr")
    # plotdim = par("pin")
    # ymult = (xylim[4] - xylim[3])/(xylim[2] - xylim[1]) * plotdim[1]/plotdim[2]
    angle.inc = 2 * pi / 100
    angles = seq(0, 2 * pi - angle.inc, by = angle.inc)
    for (i in 1:n){
        xv <- cos(angles) * rvec[i] + xvec[i]
        yv <- sin(angles) * rvec[i] + yvec[i]
        polygon(xv, yv, border = border[i], col = col)
    }
}

nnbr = function(distmtrx,k=3){
    # Find the nearest k neighbors
    stopifnot(k < nrow(distmtrx))
    res=apply(distmtrx,1,function(xv){order(xv)[1:(k+1)]})
    res=t(res)[,2:(k+1)]
    if (k>1) colnames(res)=paste('N',1:k,sep='')
    return(res)
}

nbrlist = function(region,x,y,digit=7){
    # Find all the neighbors for the polygons by checking duplicated coordinates.
    stopifnot(length(x)==length(region) && length(y)==length(region))
    dat=data.frame(r=region,x=x,y=y)
    dat$r=as.character(dat$r)
    dat$p=paste(round(dat$x,digit),round(dat$y,digit))
    dat=dat[!duplicated(dat),]
    censordat=dat[duplicated(dat$p)|duplicated(dat$p,fromLast=TRUE),]
    uniregion=sort(unique(region))
    k=length(uniregion)
    res1=list()
    res2=data.frame(region=uniregion,x=NA,y=NA)
    res2$region=as.character(res2$region)
    for (i in 1:k){
        ins=censordat[censordat$r %in% uniregion[i],]
        out=censordat[! censordat$r %in% uniregion[i],]
        res1[[i]]=unique(out[out$p %in% ins$p,1])
        res2[i,2:3]=centroid.polygon(dat[dat$r==uniregion[i],2:3])
    }
    names(res1)=uniregion
    return(list(nbr=res1,centroid=res2))
}

load('../data/usGeoInfo.rda')
load('../data/crimes.rda')
# dat=merge(usCapitals,crimes,by.x='Abbr',by.y='abbr')[,c(1,2,4,5,6,9,10,12)]
dat=merge(usCapitals,chsiALE,by.x='Abbr',by.y='CHSI_State_Abbr')[,c(1,2,4,5,6,9,10,11,12)]
dat=dat[-which(dat$Abbr %in% c('AK','HI')),]
rownames(dat)=dat$Abbr
usdist=as.matrix(dist(dat[,3:4]))
nbrs=statenbrs[names(statenbrs) %in% rownames(dat)]
nbrs=lapply(nbrs,function(xv){xv[xv %in% rownames(dat)]})
#dat$density=sqrt(dat$population/dat$TotalSqMi)
dat$density=dat$DeathPct
#dat$density=dat$ALE-min(dat$ALE)+2
dat$density=dat$density/max(dat$density)*mean(usdist)/5


circleDist=outer(dat$density,dat$density,"+")
diag(circleDist)=0
colnames(circleDist)=rownames(circleDist)=rownames(usdist)

crtloc=frc=dat[,3:4]
crtloc$Latitude=frc$Latitude=dat$centroidy
crtloc$Longitude=frc$Longitude=dat$centroidx
crtDist=usdist
s=0
err=circleDist-crtDist

while (sum(sapply(err,max,0))>0.1) {
    s=s+1
    if (s%%10==1) print(s)
    plot(Latitude~Longitude,crtloc,type='p',col=2,pch=20)
    circle(crtloc$Longitude,crtloc$Latitude,dat$density)
    text(crtloc$Longitude,crtloc$Latitude,dat$Abbr,cex=0.8)
    Sys.sleep(0.3)
    
    frc$xforce=frc$yforce=frc$xattract=frc$yattract=frc$xrepel=frc$yrepel=0.00000
    idx= circleDist > crtDist
    idx= idx & lower.tri(idx)
    err= circleDist-crtDist
    for (i in which(rowSums(idx)>0)){
        for (j in which(idx[i,1:(i-1)])){
            ratio=(circleDist[i,j]-crtDist[i,j])/2/crtDist[i,j]
            frc$xrepel[i]=frc$xrepel[i]+ratio*(crtloc$Longitude[i]-crtloc$Longitude[j])
            frc$xrepel[j]=frc$xrepel[j]+ratio*(crtloc$Longitude[j]-crtloc$Longitude[i])
            frc$yrepel[i]=frc$yrepel[i]+ratio*(crtloc$Latitude[i]-crtloc$Latitude[j])
            frc$yrepel[j]=frc$yrepel[j]+ratio*(crtloc$Latitude[j]-crtloc$Latitude[i])
        }
    }
    for (i in 1:length(nbrs)){
        for (j in 1:length(nbrs[[i]])){
            crtstate=names(nbrs)[i]
            crtnbr=which(rownames(crtDist)==nbrs[[i]][j])
            distratio=crtDist[crtstate,crtnbr]/circleDist[crtstate,crtnbr]
            if (distratio > 2){
                ratio=(crtDist[crtstate,crtnbr]-circleDist[crtstate,crtnbr])/(round(s/10)+8)/crtDist[crtstate,crtnbr]
                frc$xattract[i]=frc$xattract[i]-ratio*(crtloc$Longitude[i]-crtloc$Longitude[crtnbr])
                frc$xattract[crtnbr]=frc$xattract[crtnbr]-ratio*(crtloc$Longitude[crtnbr]-crtloc$Longitude[i])
                frc$yattract[i]=frc$yattract[i]-ratio*(crtloc$Latitude[i]-crtloc$Latitude[crtnbr])
                frc$yattract[crtnbr]=frc$yattract[crtnbr]-ratio*(crtloc$Latitude[crtnbr]-crtloc$Latitude[i])
            }
        }
    }
    frc$xforce=frc$xrepel+frc$xattract
    frc$yforce=frc$yrepel+frc$yattract
    closest=data.frame(cbind(rownames(crtDist),rownames(crtDist)[nnbr(crtDist,k=1)]))
    closest$dist=apply(closest,1,function(xv){crtDist[xv[1],xv[2]]})
    closest$force=sqrt(frc$xforce^2+frc$yforce^2)
    closest$idx=closest$force>closest$dist
    frc$xforce[closest$idx]=frc$xforce[closest$idx]*closest$dist[closest$idx]/closest$force[closest$idx]
    frc$yforce[closest$idx]=frc$yforce[closest$idx]*closest$dist[closest$idx]/closest$force[closest$idx]
    
    crtloc=crtloc+frc[,7:8]
    crtDist=as.matrix(dist(crtloc))
}
