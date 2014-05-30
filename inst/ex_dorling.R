##### Example 1 ##### Presidential Election 2012 #####

dat=merge(usCapitals,election2012,by.x='Abbr',by.y='state')[-c(1,12),c(1,6,9:12)]
state_nbrs=nbrlist(state$abbr,state$x,state$y,corner=FALSE)
nbrs=state_nbrs[names(state_nbrs) %in% dat$Abbr]
nbrs=lapply(nbrs,function(xv){xv[xv %in% dat$Abbr]})
# state_border=border_summary_length(state$abbr,state$polygon,state$x,state$y)
res=dorling(dat$Abbr,dat$centroidx,dat$centroidy,sqrt(dat$electors),nbrs,iteration=100,name.text=TRUE,dist.ratio=1.2,frame=FALSE, col=dat$result)

##### Example 2 ##### crime:population #####

data(usGeoInfo)
data(crimes)
dat=merge(usCapitals,crimes,by.x='Abbr',by.y='abbr')[,c(1,2,4,5,6,9,10,12)]
dat=dat[-which(dat$Abbr %in% c('AK','HI')),]
state_nbrs=nbrlist(state$abbr,state$x,state$y,corner=FALSE)
nbrs=state_nbrs[names(state_nbrs) %in% dat$Abbr]
nbrs=lapply(nbrs,function(xv){xv[xv %in% dat$Abbr]})
dat$density=sqrt(dat$population/dat$TotalSqMi)

dorling(dat$Abbr,dat$Longitude,dat$Latitude,dat$density,nbrs,iteration=100)
dorling(dat$Abbr,dat$centroidx,dat$centroidy,dat$density,nbrs,iteration=50,animation=FALSE)
dorling(dat$Abbr,dat$Longitude,dat$Latitude,dat$density,nbrs,iteration=10,square=TRUE)

##### Example 3 ##### CHSI:deathpct/ALE #####

data(usGeoInfo)
data(CHSI_2009)
library(plyr)
chsiALE=ddply(chsi,"CHSI_State_Abbr",summarize,ALE=sum(ALE*Population_Size,na.rm=T)/sum(Population_Size),DeathPct=sum(Total_Deaths,na.rm=T)/sum(Population_Size)*100)

dat=merge(usCapitals,chsiALE,by.x='Abbr',by.y='CHSI_State_Abbr')[,c(1,2,4,5,6,9,10,11,12)]
dat=dat[-which(dat$Abbr %in% c('AK','HI')),]
nbrs=statenbrs[names(statenbrs) %in% dat$Abbr]
nbrs=lapply(nbrs,function(xv){xv[xv %in% dat$Abbr]})

dorling(dat$Abbr,dat$Longitude,dat$Latitude,dat$DeathPct,nbrs)
dorling(dat$Abbr,dat$Longitude,dat$Latitude,dat$ALE-min(dat$ALE)+2,nbrs)

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
