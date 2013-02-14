library(maps)
a=map("state",plot=FALSE,fill=TRUE)
state=data.frame(x=a$x,y=a$y)
state$state=cumsum(is.na(a$x))+1
state$state=a$names[state$state]
state=state[-which(is.na(a$x)),]
state$state=gsub(":.*$","",state$state)
state$abbr=usCapitals[state$state,2]

################################################################################

statenbrs=nbrlist(state$abbr,state$x,state$y)$nbr
statecentroid=nbrlist(state$abbr,state$x,state$y)$centroid

rownames(usCapitals)=usCapitals$Abbr
usCapitals$centroidx=usCapitals$centroidy=NA
usCapitals[statecentroid$region,c('centroidx','centroidy')]=statecentroid[,2:3]

################################################################################

chsi=read.csv('../data/DEMOGRAPHICS.csv')
chsiALE=read.csv('../data/SUMMARYMEASURESOFHEALTH.csv')
chsiBD=read.csv('../data/MEASURESOFBIRTHANDDEATH.csv')
chsi$CHSI_County_Name=as.character(chsi$CHSI_County_Name)
chsi$CHSI_State_Name=as.character(chsi$CHSI_State_Name)
chsi$CHSI_State_Abbr=as.character(chsi$CHSI_State_Abbr)
chsi=cbind(chsi,chsiALE[,7:28],chsiBD[,7:141])
name=colnames(chsi)
chsi=chsi[,-unique(c(grep("Min_",name),grep("Max_",name),grep("_Ind",name)))]
chsi=chsi[,colnames(chsi)!="MOBD_Time_Span"]
for (j in 8:ncol(chsi)) chsi[chsi[,j]<0,j]=NA
# save(chsi,file='../data/CHSI_2009.rda')

################################################################################

library(plyr)
chsiALE=ddply(chsi,"CHSI_State_Abbr",summarize,ALE=sum(ALE*Population_Size,na.rm=T)/sum(Population_Size),DeathPct=sum(Total_Deaths,na.rm=T)/sum(Population_Size)*100)


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