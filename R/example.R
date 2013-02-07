library(maps)
a=map("state",plot=FALSE,fill=TRUE)
state=data.frame(x=a$x,y=a$y)
state$state=cumsum(is.na(a$x))+1
state$state=a$names[state$state]
state=state[-which(is.na(a$x)),]
state$state=gsub(":.*$","",state$state)
state$abbr=usCapitals[state$state,2]
statenbrs=nbrlist(state$abbr,state$x,state$y)$nbr

statecentroid=nbrlist(state$abbr,state$x,state$y)$centroid
statecentroid$region=as.character(statecentroid$region)
rownames(usCapitals)=usCapitals$Abbr
usCapitals$centroidx=usCapitals$centroidy=NA
usCapitals[statecentroid$region,c('centroidx','centroidy')]=statecentroid[,2:3]