##### Example 1 ##### Presidential Election 2012 #####

dat=merge(usCapitals,election2012,by.x='Abbr',by.y='state')[-c(1,12),c(1,2,6,11:12)]
dat$State = tolower(dat$State)
#ratio=dat$electors/dat$TotalSqMi*2000
ratio=dat$electors
vote=dat$result
names(ratio)=names(vote)=dat$Abbr
res1 = Rcartogram(state$x, state$y, state$poly, state$abbr, ratio)
res2 = Rcartogram(state$x, state$y, state$poly, state$abbr, ratio, diffuse=5)
res3 = Rcartogram(state$x, state$y, state$poly, state$abbr, ratio, nrows=100)
res4 = Rcartogram(state$x, state$y, state$poly, state$abbr, ratio, diffuse=10, nrows=100)

plotmap(res1, color=vote)
newloc = interpolate(res1,state,wt=0.5)
plotmap(newloc, color=vote)
