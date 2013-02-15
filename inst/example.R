##### Example 1 ##### crime:population #####

load('../data/usGeoInfo.rda')
load('../data/crimes.rda')
dat=merge(usCapitals,crimes,by.x='Abbr',by.y='abbr')[,c(1,2,4,5,6,9,10,12)]
dat=dat[-which(dat$Abbr %in% c('AK','HI')),]
rownames(dat)=dat$Abbr
nbrs=statenbrs[names(statenbrs) %in% rownames(dat)]
nbrs=lapply(nbrs,function(xv){xv[xv %in% rownames(dat)]})
dat$density=sqrt(dat$population/dat$TotalSqMi)

##### Example 2 ##### CHSI:deathpct/ALE #####

load('../data/usGeoInfo.rda')
load('../data/CHSI_2009.rda')
library(plyr)
chsiALE=ddply(chsi,"CHSI_State_Abbr",summarize,ALE=sum(ALE*Population_Size,na.rm=T)/sum(Population_Size),DeathPct=sum(Total_Deaths,na.rm=T)/sum(Population_Size)*100)

dat=merge(usCapitals,chsiALE,by.x='Abbr',by.y='CHSI_State_Abbr')[,c(1,2,4,5,6,9,10,11,12)]
dat=dat[-which(dat$Abbr %in% c('AK','HI')),]
rownames(dat)=dat$Abbr
nbrs=statenbrs[names(statenbrs) %in% rownames(dat)]
nbrs=lapply(nbrs,function(xv){xv[xv %in% rownames(dat)]})
dat$density=dat$DeathPct
#dat$density=dat$ALE-min(dat$ALE)+2

################################################################################

library(maps)
county=map("county",plot=FALSE,fill=TRUE)
a=county$names
b=tolower(paste(chsi[,4],chsi[,3],sep=','))
b=gsub(",st\\. ",",st ",b)
a=gsub(",de ",",de",a)
b=gsub("'","",b)
setdiff(a,b)
setdiff(b,a)

################################################################################
library(shiny)
runApp("dorling")