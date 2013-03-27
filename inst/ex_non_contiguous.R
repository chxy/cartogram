data(usGeoInfo)

r=usCapitals[,c(1,2,6,8)]
r$WaterRatio=r$WaterSqMi/r$TotalSqMi*5
res=map_scaling(state[,c(4,1,2)],r[,c(1,4)],TRUE)

r$State=tolower(r$State)
polyname=state[!duplicated(state[,4:5]),3:5]
r=merge(polyname,r,by.x=1,by.y=1)
res=map_scaling(state[,c(5,1,2)],r[,c(3,7)],TRUE)

idx=setdiff(1:nrow(res),rownames(res))
res[(1:length(idx))+nrow(res),]=NA
rownames(res)[nrow(res)+1-(1:length(idx))]=idx
res=res[order(as.integer(rownames(res))),]
plot(res[,2],res[,3],type='l',xlab='longitude',ylab='latitude')