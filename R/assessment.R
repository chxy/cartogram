##' Compare two polygons which have the same number of vertexes
##' @examples
##' x1 = c(0,0,1,1,0)
##' y1 = c(1,0,0,1,1)
##' x2 = c(3,3,5,5,3)
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
  
  area1 = sp::Polygon(data.frame(x=x1,y=y1))@area
  area2 = sp::Polygon(data.frame(x=x2,y=y2))@area
}

##' Get the slope from two points
##' 
slope = function(x1,x2,y1,y2){
  a = (y1-y2)/(x1-x2)
}
