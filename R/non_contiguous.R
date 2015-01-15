##' Rescale a map of multiple polygons
##'
##' @param poly a data frame of four columns: polygon names, region names, 
##' x and y coordinates of the vertices.
##' @param ratio a vector of the corresponding rescaling ratios. The 
##' length must be as the unique regions and the values must be positive. 
##' Suggest to give the region names to the vector; if not given, then 
##' the order of the ratios will match the regions.
##' @param anchor a number in [0,1] to select the quantile of ratio on which
##' the re-scaled polygon will be remain the same size as origin.
##' If \code{anchor=1}, then there will be no overlapped polygons.
##' @param adjust whether to adjust the centroids of polygons
##' to avoid overlapping if \code{anchor<1}.
##' @param nbr a list of the neighbors of every region. Should be
##' the output of the function\code{nbrlist()}. Each element is
##' a vector of all the neighbor names of a region. If \code{nbr=NULL},
##' then it is assumed that no region has any neighbors, and hence the
##' argument adjust is set to FALSE. If nbr is not NULL, then names
##' must be given to all the elements of the list, for matching the
##' neighbors with the host region name (the second column of poly).
##' @param refloc a data frame containing at least abbr, x, and y.
##' Row names must be abbr that can match with region names. Using
##' as the reference location to adjust the polygon centers. Better
##' to choose the outcome of a dorling cartogram.
##' @return A data frame with region names and the new borders.
##' @export
##' @example inst/ex_non_contiguous.R
##' 
map_scaling = function(poly,ratio,anchor=0.85,adjust=FALSE,nbr=NULL,refloc=NULL){
    uniregion = unique(poly[,2])
    stopifnot(all(ratio>0), length(uniregion)==nrow(ratio))
    if (!is.null(names(ratio))) {
      stopifnot(length(setdiff(uniregion,names(ratio)))==0)
    } else names(ratio) = uniregion
    if (anchor==1 | is.null(nbr)) adjust=FALSE
    
    # ratio: automatically adjust to a better scaling factor vector
    ratio = sqrt(ratio/quantile(ratio,anchor,na.rm=TRUE))

    # for each region
    dat=poly
    regioncenter = matrix(0,ncol=2,nrow=length(uniregion))
    rownames(regioncenter) = uniregion
    for (i in 1:length(uniregion)){
        crtratio=ratio[uniregion[i]]
        # if (length(crtratio) == 0) crtratio=1
        crtidx = which(poly[,2]==uniregion[i])
        crtdat = poly[crtidx, -2]
        
        regioncenter[i,] = centroid_polygon(crtdat[,-1])
        unipoly = unique(crtdat[,1])
        center_vec = center_new = matrix(0,ncol=2,nrow=length(unipoly))
        for (j in 1:length(unipoly)){
          center_vec[j,] = centroid_polygon(crtdat[crtdat[,1]==unipoly[j],-1])
        }
        center_new[,1]=(1-crtratio)*regioncenter[i,1]+crtratio*center_vec[,1]
        center_new[,2]=(1-crtratio)*regioncenter[i,2]+crtratio*center_vec[,2]
        for (j in 1:length(unipoly)){
          tmpidx = crtidx[crtdat[,1]==unipoly[j]]
          dat[tmpidx,3:4]=polygon_scaling(crtratio,poly[tmpidx,3:4],center_new[j,])
        }
    }
    names(dat)[1:4] = c('poly','abbr','x','y')
    if (all(ratio<=1) | (!adjust)) return(dat)
    
    # if refloc is given
    if (!is.null(refloc)) {
      for (i in 1:length(uniregion)){
        crtidx = which(dat[,2]==uniregion[i])
        if (ratio[uniregion[i]]>1) {
          dat[crtidx,'x'] = dat[crtidx,'x']-(regioncenter[uniregion[i],1]-refloc[uniregion[i],'x'])*anchor/2
          dat[crtidx,'y'] = dat[crtidx,'y']-(regioncenter[uniregion[i],2]-refloc[uniregion[i],'y'])*anchor/2
        } else {
          dat[crtidx,'x'] = dat[crtidx,'x']-(regioncenter[uniregion[i],1]-refloc[uniregion[i],'x'])*anchor/5
          dat[crtidx,'y'] = dat[crtidx,'y']-(regioncenter[uniregion[i],2]-refloc[uniregion[i],'y'])*anchor/5
        }
      }
      return(dat)
    }
    
    # if adjust is true and refloc is null
    collispoly = names(ratio)[ratio>1]
    collisnbr = nbr[collispoly]
    allpoly = unique(c(collispoly,unlist(collisnbr)))
    alldat = dat[dat$abbr %in% allpoly,]
    collisarea = polyarea(alldat,alldat$poly,alldat$abbr,TRUE)
    collisarea$radius = sqrt(collisarea$area/pi)
    colliscenter = regioncenter[allpoly,,drop=FALSE]
    centerchange = regioncenter[collispoly,,drop=FALSE]
    centerchange[,1] = centerchange[,2] = 0
    
    dat2=dat
    for (i in order(ratio[collispoly],decreasing=TRUE)) {
      crtpoly = collispoly[i]
      crtnbr = collisnbr[[crtpoly]]
      if (length(crtnbr)==0) next
      #crtnbr1 = which(!crtnbr %in% collispoly)
      crtattr = (collisarea[crtnbr,3]/ratio[crtnbr]*(1-ratio[crtnbr])+collisarea[crtpoly,3]/ratio[crtpoly]*(1-ratio[crtpoly]))/2
      crtdist = sqrt((colliscenter[crtnbr,1]-colliscenter[crtpoly,1])^2+(colliscenter[crtnbr,2]-colliscenter[crtpoly,2])^2)
      centerchange[crtpoly,1] = sum((colliscenter[crtnbr,1]-colliscenter[crtpoly,1])*crtattr/crtdist)
      centerchange[crtpoly,2] = sum((colliscenter[crtnbr,2]-colliscenter[crtpoly,2])*crtattr/crtdist)
      dat2[dat$abbr==crtpoly,3] = dat[dat$abbr==crtpoly,3] + centerchange[crtpoly,1]
      dat2[dat$abbr==crtpoly,4] = dat[dat$abbr==crtpoly,4] + centerchange[crtpoly,2]
    }
    return(dat2)
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