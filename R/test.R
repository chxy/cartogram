library(plotrix)

circle = function(xvec,yvec,rvec,col=NULL,border=1){
    n=length(xvec)
    stopifnot(length(yvec)==n && n==length(rvec))
    if (length(border) < n)  border = rep(border, length.out = n)
    xylim <- par("usr")
    plotdim <- par("pin")
    ymult <- (xylim[4] - xylim[3])/(xylim[2] - xylim[1]) * plotdim[1]/plotdim[2]
    angle.inc <- 2 * pi / 100
    angles <- seq(0, 2 * pi - angle.inc, by = angle.inc)
    for (i in 1:n){
        xv <- cos(angles) * rvec[i] + xvec[i]
        yv <- sin(angles) * rvec[i] + yvec[i]
        polygon(xv, yv, border = border[i], col = col)
    }
}

load('../data/usCapitals.rda')
load('../data/crimes.rda')
dat=merge(usCapitals,crimes,by.x='Abbr',by.y='abbr')[,c(1,2,4,5,6,10)]
dat=dat[-which(dat$Abbr %in% c('AK','HI')),]
rownames(dat)=dat$Abbr
usdist=as.matrix(dist(dat[,3:4]))
dat$density=sqrt(dat$population/dat$TotalSqMi)
dat$density=dat$density/max(dat$density)*mean(usdist)/5

plot(dat$Longitude,dat$Latitude,type='p',col=2,pch=20)
for (i in 1:nrow(dat)) draw.circle(dat[i,4],dat[i,3],dat[i,'density'])
text(dat$Longitude,dat$Latitude,dat$Abbr)

circleDist=outer(dat$density,dat$density,"+")
diag(circleDist)=0
colnames(circleDist)=rownames(circleDist)=rownames(usdist)

crtloc=frc=dat[,3:4]

frc$xforce=frc$yforce=0.00000
for (i in 2:nrow(dat)){
    for (j in 1:(i-1)){
        if (circleDist[i,j]>usdist[i,j]){
            ratio=(circleDist[i,j]-usdist[i,j])/2/usdist[i,j]
            frc$xforce[i]=frc$xforce[i]+ratio*(frc$Longitude[i]-frc$Longitude[j])
            frc$xforce[j]=frc$xforce[j]+ratio*(frc$Longitude[j]-frc$Longitude[i])
            frc$yforce[i]=frc$yforce[i]+ratio*(frc$Latitude[i]-frc$Latitude[j])
            frc$yforce[j]=frc$yforce[j]+ratio*(frc$Latitude[j]-frc$Latitude[i])
        }
    }
}
crtloc=crtloc+frc[,3:4]
usdist=as.matrix(dist(crtloc))
plot(crtloc$Longitude,crtloc$Latitude,type='p',col=2,pch=20)
circle(crtloc[,2],crtloc[,1],dat[,'density'])
#for (i in 1:nrow(crtloc)) draw.circle(crtloc[i,2],crtloc[i,1],dat[i,'density'])
text(crtloc$Longitude,crtloc$Latitude,dat$Abbr)