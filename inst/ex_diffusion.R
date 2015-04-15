##### Example 1 ##### Presidential Election 2012 #####

dat=merge(usCapitals,election2012,by.x='Abbr',by.y='state')[-c(1,12),c(1,2,6,11:12)]
dat$State = tolower(dat$State)
#ratio=dat$electors/dat$TotalSqMi*2000
ratio=dat$electors
vote=dat$result
names(ratio)=names(vote)=dat$Abbr
res1 = Rcartogram(state$x, state$y, state$poly, state$abbr, ratio)
res2 = Rcartogram(state$x, state$y, state$poly, state$abbr, ratio, nrows=100)
res3 = Rcartogram(state$x, state$y, state$poly, state$abbr, ratio, blank.init=0.2)
res4 = Rcartogram(state$x, state$y, state$poly, state$abbr, ratio, nrows=100, blank.init=0.2)
res5 = Rcartogram(state$x, state$y, state$poly, state$abbr, ratio, blank.init=0.2, sea.init=0.1, sea.width=0.5)
res6 = Rcartogram(state$x, state$y, state$poly, state$abbr, ratio, nrows=100, ncols=60, blank.init=0.2, sea.init=0.4, sea.width=2)

plotmap(res1, color=vote)
newloc = interpolate(res1,state,wt=0.5)
plotmap(newloc, color=vote)

m=20
res = data.frame(matrix(NA,nrow=(m+1)*49,ncol=4))
colnames(res) = c('wt','shape','size','state')
perim = perimeter(state$abbr,state$polygon,state$x,state$y)
for (i in (0:m)/m) {
  newloc = interpolate(res1,state,wt=i)
  shape = shape_diff(newloc,state,state$polygon)
  crt = polyarea(newloc,state$polygon,state$abbr)
  crt = cbind(crt,shape)
  dif = cbind(tapply(crt[,3],crt[,2],sum),tapply(crt[,4],crt[,2],sum))
  dif = cbind(dif,size_diff(dif[,1],ratio,perim))
  dif[,1] = i
  res[(1:49)+i*m*49,1:3] = dif
  res[(1:49)+i*m*49,4] = rownames(dif)
}
res=reshape2::melt(res,c(1,4))
colnames(res)=c('weight','state','type','difference')
library(plyr)
r=ddply(res,c('weight','type'),summarise,difference=mean(difference))

library(ggplot2)
qplot(weight,difference,data=res,geom='line',group=state,facets=~type,alpha=I(0.2)) + geom_line(aes(group=NULL),color=I(7),size=I(2),data=r)

##### Example 1 ##### Shiny App #####
library(shiny)
runApp('diffusion')
