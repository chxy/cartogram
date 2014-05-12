##' Find the centroid of a polygon.
##' 
##' This function is copied from the internal function \code{centroid.polygon()} 
##' of package \code{maps}.
##' @param p a data frame of two columns.
##' @return the x and y coordinates of the centroid.
##'
centroid_polygon = function(p) {
    n = nrow(p)
    i = c(n, 1:(n - 1))
    x1 = p[, 1]
    x2 = p[i, 1]
    y1 = p[, 2]
    y2 = p[i, 2]
    a = x1*y2 - x2*y1
    s = sum(a)*3
    if(s == 0) c(mean(x1), mean(y1))
    else c(sum((x1 + x2)*a)/s, sum((y1 + y2)*a)/s)
}

##' Find the centroids for the polygons.
##' 
##' @param region Region names.
##' @param x X-coordinates.
##' @param y Y-coordinates.
##' @return A data frame with region names and coordinates of the centroids.
##' @export
##' @examples
##' data(usCapitals)
##' state_centroid=centroid(state$abbr,state$x,state$y)
##'
centroid = function(region,x,y){
    stopifnot(length(x)==length(region), length(y)==length(region))
    dat=data.frame(r=region,x=x,y=y)
    uniregion=if (is.factor(region)) {levels(region)} else {sort(unique(region))}
    res=data.frame(region=uniregion,x=NA,y=NA,stringsAsFactors=FALSE)
    for (i in 1:nrow(res)) res[i,2:3]=centroid_polygon(dat[dat$r==uniregion[i],2:3])
    return(res)
}
