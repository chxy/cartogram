##' Compare the shape of two polygons which have the same number of vertexes
##' @param x1,y1 generated polygons
##' @param x2,y2 original polygons
##' @export
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
                   a1front=NA,a1rear=NA,a2front=NA,a2rear=NA,
                   l1front=NA,l1rear=NA,l2front=NA,l2rear=NA,
                   a1=NA,a2=NA,r1=NA,r2=NA)
  n=n-1
  tmp = rbind(tmp[n,,drop=FALSE],tmp)
  rownames(tmp) = c(0:(n+1))
  tmp$a1front[1:n+1] = atan2(tmp$y1[1:n]-tmp$y1[1:n+1],tmp$x1[1:n]-tmp$x1[1:n+1])
  tmp$a1rear[1:n+1] = atan2(tmp$y1[1:n+2]-tmp$y1[1:n+1],tmp$x1[1:n+2]-tmp$x1[1:n+1])
  tmp$a2front[1:n+1] = atan2(tmp$y2[1:n]-tmp$y2[1:n+1],tmp$x2[1:n]-tmp$x2[1:n+1])
  tmp$a2rear[1:n+1] = atan2(tmp$y2[1:n+2]-tmp$y2[1:n+1],tmp$x2[1:n+2]-tmp$x2[1:n+1])
  tmp$a1 = tmp$a1rear - tmp$a1front
  tmp$a1[!is.na(tmp$a1) & tmp$a1<0] = tmp$a1[!is.na(tmp$a1) & tmp$a1<0] + 2*pi
  tmp$a2 = tmp$a2rear - tmp$a2front
  tmp$a2[!is.na(tmp$a2) & tmp$a2<0] = tmp$a2[!is.na(tmp$a2) & tmp$a2<0] + 2*pi
  tmp$l1front[1:n+1] = sqrt((tmp$x1[1:n]-tmp$x1[1:n+1])^2+(tmp$y1[1:n]-tmp$y1[1:n+1])^2)
  tmp$l1rear[1:n+1] = sqrt((tmp$x1[1:n+2]-tmp$x1[1:n+1])^2+(tmp$y1[1:n+2]-tmp$y1[1:n+1])^2)
  tmp$l2front[1:n+1] = sqrt((tmp$x2[1:n]-tmp$x2[1:n+1])^2+(tmp$y2[1:n]-tmp$y2[1:n+1])^2)
  tmp$l2rear[1:n+1] = sqrt((tmp$x2[1:n+2]-tmp$x2[1:n+1])^2+(tmp$y2[1:n+2]-tmp$y2[1:n+1])^2)
  tmp$r1 = tmp$l1front/tmp$l1rear
  tmp$r2 = tmp$l2front/tmp$l2rear
  res = tmp[1:n+1,]
  r1ad = abs(res$a1-res$a2)
  r2ed = atan(pmax(res$r1/res$r2,res$r2/res$r1)-1)
  d = sum((r1ad + r2ed) * (res$l1front+res$l1rear)/2)
  attr(d,'part') = data.frame(ad=r1ad, ed=r2ed, l1f=res$l1front, l1r=res$l1rear, l2f=res$l2front, l2r=res$l2rear,res[,-c(9:12)])
  return(d)
}


##' Get the slope from two points
##' @param x1,y1 coordinates of the first point
##' @param x2,y2 coordinates of the second point
##' 
slope = function(x1,x2,y1,y2){
  a = (y1-y2)/(x1-x2)
}


##' Get the quadrant of the second point using the first point as origin
##' @param x1,y1 coordinates of the first point
##' @param x2,y2 coordinates of the second point
##' 
quadrant = function(x2,x1,y2,y1){
  res = rep(NA,length(x1))
  res[x2>x1 & y2>y1] = 1
  res[x2<x1 & y2>y1] = 2
  res[x2<x1 & y2<y1] = 3
  res[x2>x1 & y2<y1] = 4
  res[x2>x1 & y2==y1] = 1
  res[x2==x1 & y2>y1] = 2
  res[x2<x1 & y2==y1] = 3
  res[x2==x1 & y2<y1] = 4
  res[x2==x1 & y2==y1] = 0
  return(res)
}


##' Compare the shape of the contiguous cartogram with that of the original map
##' @param map1 coordinates of the cartogram
##' @param map2 coordinates of the map
##' @param poly a vector of the polygon names. Must be of the same length with \code{map1} and \code{map2}, and unique for each polygon.
##' @param region a vector of the region names.
##' @export
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
    idx = poly==unipoly[i]
    dis[i] = check2poly(map1$x[idx],map1$y[idx],map2$x[idx],map2$y[idx])
  }
  names(dis) = unipoly
  return(dis)
}


##' Compute the area of each region for a map
##' @param map0 coordinates of a map
##' @param poly a vector of the polygon names. Must be of the same length with \code{map1} and \code{map2}, and unique for each polygon.
##' @param region a vector of the region names. Must be of the same length with \code{x}, and unique for each region.
##' @param regionarea whether to calculate the area by region or by poly
##' @export
##' @examples
##' dat=merge(usCapitals,election2012,by.x='Abbr',by.y='state')[-c(1,12),c(1,2,6,11:12)]
##' dat$State = tolower(dat$State)
##' ratio=dat$electors
##' vote=dat$result
##' names(ratio)=names(vote)=dat$Abbr
##' res = Rcartogram(state$x, state$y, state$poly, state$abbr, ratio, color=vote)$final
##' polyarea(res,state$polygon,state$abbr)
##' 
polyarea = function(map0,poly,region=poly,regionarea=FALSE){
  stopifnot(nrow(map0)==length(poly), length(region)==length(poly))
  unipoly = unique(cbind(poly,region))
  np = nrow(unipoly)
  res = data.frame(unipoly,area=NA,stringsAsFactors=FALSE)
  for (i in 1:np){
    idx = which(poly==unipoly[i,1])
    res[i,3] = sp::Polygon(data.frame(x=map0$x[idx],y=map0$y[idx]))@area
  }
  if (regionarea) {
    tmp = tapply(res[,3],res[,2],sum)
    res2 = res[!duplicated(res[,2]),2:3]
    res2[,1] = names(tmp)
    res2[,2] = unname(tmp)
    rownames(res2) = res2[,1]
    res = res2
  }
  return(res)
}


##' Compare the current polygon sizes with the target ones
##' @param crtsize the current sizes of the polygons
##' @param tgtsize the target sizes of the polygons
##' @param peri perimeters of the polygons. If NULL, then calculated from the current sizes.
##' @param scaled logical. Whether to scale the target sizes.
##' @export
##' @examples
##' dat=merge(usCapitals,election2012,by.x='Abbr',by.y='state')[-c(1,12),c(1,2,6,11:12)]
##' dat$State = tolower(dat$State)
##' ratio=dat$electors
##' vote=dat$result
##' names(ratio)=names(vote)=dat$Abbr
##' res = Rcartogram(state$x, state$y, state$poly, state$abbr, ratio, color=vote)$final
##' crt = polyarea(res,state$polygon,state$abbr)
##' crt = tapply(crt[,3],crt[,2],sum)
##' perim = perimeter(state$abbr,state$polygon,state$x,state$y)
##' size_diff(crt,ratio,perim)
##' 
size_diff = function(crtsize,tgtsize,peri=NULL,scaled=TRUE){
  stopifnot(length(crtsize)==length(tgtsize))
  if (!scaled) {
    return(abs(atan(crtsize/tgtsize-1)))
  }
  tgtsize = tgtsize/sum(tgtsize, na.rm=TRUE)
  crtsize = crtsize/sum(crtsize, na.rm=TRUE)
  if (is.null(peri)) peri = sqrt(crtsize*pi)*2
  return(atan(pmax(crtsize/tgtsize,tgtsize/crtsize)-1)*peri)
}
