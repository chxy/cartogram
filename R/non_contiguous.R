##' Rescale a map of multiple polygons
##'
##' @param poly a data frame of three columns: region names, x and y coordinates of the vertices.
##' @param ratio a data frame of two columns: the region names and their
##' corresponding rescaling ratios. The ratios must be positive.
##' @return A data frame with region names and the new borders.
##' @export
##' @example inst/ex_non_contiguous.R
##' 
map_scaling = function(poly,ratio,standardize=FALSE){
    stopifnot(all(ratio[,2]>0))
    ratio[,2]=sqrt(ratio[,2])
    if (standardize) ratio[,2]=ratio[,2]/max(ratio[,2],na.rm=TRUE)
    dat=poly
    uniregion=unique(poly[,1])
    for (i in 1:length(uniregion)){
        crtratio=ratio[ratio[,1]==uniregion[i],2]
        if (length(crtratio) == 0) crtratio=1
        if (crtratio != 1){
            crtidx = poly[,1]==uniregion[i]
            dat[crtidx,2:3]=polygon_scaling(crtratio,poly[crtidx,2:3])
        }
    }
    dat
}


##' Scale one polygon
##' 
##' @param r the ratio. Must be positive.
##' @param p the data frame of the polygon vertices
##' @return the border of the scaled polygon.
##' @export
##' @examples
##' a=data.frame(x=c(1,2,2,1,1),y=c(1,1,2,2,1))
##' b1=polygon_scaling(0.8, a)
##' b2=polygon_scaling(1.5, a)
##' plot(a$x,a$y,type='l',xlim=c(-1,4),ylim=c(-1,4))
##' lines(b1$x,b1$y,col=2)
##' lines(b2$x,b2$y,col=3)
##' 
polygon_scaling = function(r, p){
    stopifnot(r>0)
    center = centroid_polygon(p)
    p2=p
    p2[,1]=(1-r)*center[1]+r*p[,1]
    p2[,2]=(1-r)*center[2]+r*p[,2]
    p2
}
