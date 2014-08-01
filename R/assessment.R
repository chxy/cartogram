##' Compare the shape of two polygons which have the same number of vertexes
##' @param x1,y1 generated polygons
##' @param x2,y2 original polygons
##' @examples
##' x1 = c(0,0,1,1,0)
##' y1 = c(1,0,0,1,1)
##' x2 = c(3,3,5,6,3)
##' y2 = c(3,1,1,3,3)
##' 
check2poly = function(x1,y1,x2,y2){
  n = length(x1)
  stopifnot(n==length(y1),n==length(x2),n==length(y2))
  tmp = data.frame(x1=x1,y1=y1,x2=x2,y2=y2,
                   s1front=NA,s1rear=NA,s2front=NA,s2rear=NA,
                   l1front=NA,l1rear=NA,l2front=NA,l2rear=NA,
                   a1=NA,a2=NA,r1=NA,r2=NA)
  n=n-1
  tmp = rbind(tmp[n,,drop=FALSE],tmp)
  rownames(tmp) = c(0:(n+1))
  tmp$s1front[1:n+1] = slope(tmp$x1[1:n],tmp$x1[1:n+1],tmp$y1[1:n],tmp$y1[1:n+1])
  tmp$s1rear[1:n+1] = slope(tmp$x1[1:n+2],tmp$x1[1:n+1],tmp$y1[1:n+2],tmp$y1[1:n+1])
  tmp$s2front[1:n+1] = slope(tmp$x2[1:n],tmp$x2[1:n+1],tmp$y2[1:n],tmp$y2[1:n+1])
  tmp$s2rear[1:n+1] = slope(tmp$x2[1:n+2],tmp$x2[1:n+1],tmp$y2[1:n+2],tmp$y2[1:n+1])
  tmp$a1 = atan(tmp$s1front)+atan(tmp$s1rear)
  tmp$a2 = atan(tmp$s2front)+atan(tmp$s2rear)
  tmp$l1front[1:n+1] = sqrt((tmp$x1[1:n]-tmp$x1[1:n+1])^2+(tmp$y1[1:n]-tmp$y1[1:n+1])^2)
  tmp$l1rear[1:n+1] = sqrt((tmp$x1[1:n+2]-tmp$x1[1:n+1])^2+(tmp$y1[1:n+2]-tmp$y1[1:n+1])^2)
  tmp$l2front[1:n+1] = sqrt((tmp$x2[1:n]-tmp$x2[1:n+1])^2+(tmp$y2[1:n]-tmp$y2[1:n+1])^2)
  tmp$l2rear[1:n+1] = sqrt((tmp$x2[1:n+2]-tmp$x2[1:n+1])^2+(tmp$y2[1:n+2]-tmp$y2[1:n+1])^2)
  tmp$r1 = tmp$l1front/tmp$l1rear
  tmp$r2 = tmp$l2front/tmp$l2rear
  res = tmp[1:n+1,]
  d = sum((abs(res$a1-res$a2)+atan(pmax(res$r1/res$r2,res$r2/res$r1)-1))*
          (res$l1front+res$l1rear)/2)
  return(d)
}


##' Get the slope from two points
##' @param x1,y1 coordinates of the first point
##' @param x2,y2 coordinates of the second point
##' 
slope = function(x1,x2,y1,y2){
  a = (y1-y2)/(x1-x2)
}


##' Compare the shape of the contiguous cartogram with that of the original map
##' @param map1 coordinates of the cartogram
##' @param map2 coordinates of the map
##' @parame poly a vector of the polygon names. Must be of the same length with \code{map1} and \code{map2}, and unique for each polygon.
##' @examples
##' dat=merge(usCapitals,election2012,by.x='Abbr',by.y='state')[-c(1,12),c(1,2,6,11:12)]
##' dat$State = tolower(dat$State)
##' ratio=dat$electors
##' vote=dat$result
##' names(ratio)=names(vote)=dat$Abbr
##' res = Rcartogram(state$x, state$y, state$poly, state$abbr, ratio, color=vote)$final
##' sum(shape_diff(res,state,state$polygon))
##' 
shape_diff = function(map1, map2, poly){
  stopifnot(nrow(map1)==length(poly), nrow(map2)==length(poly))
  unipoly = unique(poly)
  np = length(unipoly)
  dis = rep(NA,np)
  for (i in 1:np){
    idx = which(poly==unipoly[i])
    dis[i] = check2poly(map1$x[idx],map1$y[idx],map2$x[idx],map2$y[idx])
  }
  names(dis) = unipoly
  return(dis)
}


##' Compute the area of each region for a map
##' @param map0 coordinates of a map
##' @parame poly a vector of the polygon names. Must be of the same length with \code{map1} and \code{map2}, and unique for each polygon.
##' @param region a vector of the region names. Must be of the same length with \code{x}, and unique for each region.
##' @examples
##' dat=merge(usCapitals,election2012,by.x='Abbr',by.y='state')[-c(1,12),c(1,2,6,11:12)]
##' dat$State = tolower(dat$State)
##' ratio=dat$electors
##' vote=dat$result
##' names(ratio)=names(vote)=dat$Abbr
##' res = Rcartogram(state$x, state$y, state$poly, state$abbr, ratio, color=vote)$final
##' polyarea(res,state$polygon,state$abbr)
##' 
polyarea = function(map0,poly,region=poly){
  stopifnot(nrow(map0)==length(poly), length(region)==length(poly))
  unipoly = unique(cbind(poly,region))
  np = nrow(unipoly)
  res = data.frame(unipoly,area=NA,stringsAsFactors=FALSE)
  for (i in 1:np){
    idx = which(poly==unipoly[i,1])
    res[i,3] = sp::Polygon(data.frame(x=map0$x[idx],y=map0$y[idx]))@area
  }
  return(res)
}


##' Compare the current polygon sizes with the target ones
##' @param crtsize the current sizes of the polygons
##' @param tgtsize the target sizes of the polygons
##' @examples
##' dat=merge(usCapitals,election2012,by.x='Abbr',by.y='state')[-c(1,12),c(1,2,6,11:12)]
##' dat$State = tolower(dat$State)
##' ratio=dat$electors
##' vote=dat$result
##' names(ratio)=names(vote)=dat$Abbr
##' res = Rcartogram(state$x, state$y, state$poly, state$abbr, ratio, color=vote)$final
##' crt = polyarea(res,state$polygon,state$abbr)
##' crt = tapply(crt[,3],crt[,2],sum)
##' size_diff(crt,ratio)
##' 
size_diff = function(crtsize,tgtsize){
  stopifnot(length(crtsize)==length(tgtsize))
  crtsize = (crtsize-min(crtsize))/max(crtsize)
  tgtsize = (tgtsize-min(tgtsize))/max(tgtsize)
  return(sum(abs(crtsize-tgtsize))*sum(crtsize))
}
