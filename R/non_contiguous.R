##' Rescale a map of multiple polygons
##'
##' @param poly a data frame of four columns: polygon names, region names, 
##' x and y coordinates of the vertices.
##' @param ratio a vector of the corresponding rescaling ratios. The 
##' length must be as the unique regions and the values must be positive. 
##' Suggest to give the region names to the vector; if not given, then 
##' the order of the ratios will match the regions.
##' @param color  a vector of color to fill in the polygons. Auto-completed if 
##' the length does not match with ratio. Suggest to give the region names to the 
##' vector; if not given, then the order of the ratios will match the regions.
##' @param border a vector of polygon borders. Auto-completed if the length 
##' does not match with ratio. Suggest to give the region names to the vector; 
##' if not given, then the order of the ratios will match the regions.
##' @param standardize whether to standardize the rescaling ratio.
##' @param name.text whether to print the region names on the circles or polygons.
##' @return A data frame with region names and the new borders.
##' @export
##' @example inst/ex_non_contiguous.R
##' 
map_scaling = function(poly,ratio,color=NULL,border=1,standardize=TRUE, name.text=TRUE){
    uniregion = unique(poly[,2])
    stopifnot(all(ratio>0), length(uniregion)==nrow(ratio))
    if (!is.null(names(ratio))) {
      stopifnot(length(setdiff(uniregion,names(ratio)))==0)
    } else names(ratio) = uniregion
    
    # ratio
    ratio=sqrt(ratio)
    if (standardize) ratio=ratio/max(ratio,na.rm=TRUE)
    
    # color and border
    if (!is.null(border)) border=complete_color(border, length(ratio))
    if (!is.null(color)) color=complete_color(color, length(ratio))
    if (!is.null(names(color))) {
      stopifnot(length(setdiff(uniregion,names(color)))==0)
    } else if (!is.null(color)) names(color) = uniregion
    if (!is.null(names(border))) {
      stopifnot(length(setdiff(uniregion,names(border)))==0)
    } else names(border) = uniregion
    
    # for each region
    dat=poly
    plot(dat[,3],dat[,4],type='n',xlab='',ylab='',xaxt='n',yaxt='n',frame=FALSE)
    for (i in 1:length(uniregion)){
        crtratio=ratio[uniregion[i]]
        # if (length(crtratio) == 0) crtratio=1
        crtidx = which(poly[,2]==uniregion[i])
        crtdat = poly[crtidx, -2]
        
        crtcenter = centroid_polygon(crtdat[,-1])
        unipoly = unique(crtdat[,1])
        center_vec = center_new = matrix(0,ncol=2,nrow=length(unipoly))
        for (j in 1:length(unipoly)){
          center_vec[j,] = centroid_polygon(crtdat[crtdat[,1]==unipoly[j],-1])
        }
        center_new[,1]=(1-crtratio)*crtcenter[1]+crtratio*center_vec[,1]
        center_new[,2]=(1-crtratio)*crtcenter[2]+crtratio*center_vec[,2]
        for (j in 1:length(unipoly)){
          tmpidx = crtidx[crtdat[,1]==unipoly[j]]
          dat[tmpidx,3:4]=polygon_scaling(crtratio,poly[tmpidx,3:4],center_new[j,])
          polygon(dat[tmpidx,3], dat[tmpidx,4], border = border[uniregion[i]], col = color[uniregion[i]])
        }
        if (name.text) text(crtcenter[1], crtcenter[2], uniregion[i], cex=0.8)
    }
    return(dat)
}


##' Scale one polygon
##' 
##' @param r the ratio. Must be positive.
##' @param p the data frame of the polygon vertices
##' @param target the coordinates(x,y) of the target centroid of the polygon. 
##' If null then the polygon does not need to move.
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
polygon_scaling = function(r, p, target=NULL){
    stopifnot(r>0)
    center = centroid_polygon(p)
    p2=p
    p2[,1]=(1-r)*center[1]+r*p[,1] + target[1] - center[1]
    p2[,2]=(1-r)*center[2]+r*p[,2] + target[2] - center[2]
    p2
}


##' Auto complete (or cut) a vector to a fixed length
##' 
##' @param cl a vector
##' @param targetlen the target length
##' @return a vector of completed cl with length n
##' @export
##' @examples
##' complete_color('red',5)
##' complete_color(c('red','blue'),5)
##' complete_color(c('red','blue','green','yellow','pink','grey'),5)
##' 
complete_color = function(cl,targetlen){
  l = length(cl)
  if (l==0) return()
  if (l < targetlen){
    cl = rep(cl,ceiling(targetlen/l))
  }
  cl=cl[1:targetlen]
  return(cl)
}