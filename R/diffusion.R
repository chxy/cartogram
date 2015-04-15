##' Draw a diffusion-based cartogram
##' 
##' @param x a vector of x-coordinates of the border
##' @param y a vector of y-coordinates of the border.
##' @param poly a vector of the polygon names. Must be of the same length with \code{x}, and unique for each polygon.
##' @param region a vector of the region names. Must be of the same length with \code{x}, and unique for each region.
##' @param size the size vector for regions (length must be equal to the number of regions)
##' @param gridmap output of the function \code{checkerboard}. Default to be null.
##' @param nrows the row resolution to get a grid. If \code{gridmap} is not null, then nrows must be in the attribute 'nbins' of gridmap.
##' @param ncols the column resolution to get a grid.
##' @param blank.init between 0 and 1. NA's of the grids are filled with (1-blank.init) * min(density) + blank.init * max(density).
##' @param sea.init between 0 and 1. The sea is filled with (1-sea.init) * min(density) + sea.init * max(density).
##' @param sea.width the width of sea. Must be positive.
##' @param blur Gaussian smoothing paramter passed to the cartogram function.
##' @return a data frame with four columns: new x and y coordinates, polygon names and region names.
##' @export
##' @example inst/ex_diffusion.R
##' 

Rcartogram = function(x, y, poly, region, size, gridmap=NULL, nrows=50, ncols=nrows, blank.init=0, sea.init=0,  sea.width=1, blur=0){
  library(Rcartogram)
  
  stopifnot(sea.width>0, blur>=0)
  
  uniregion = unique(region); unipoly = unique(poly)
  stopifnot(length(x)==length(y), length(x)==length(poly), 
            length(poly)==length(region), length(size)==length(uniregion))
  if (!is.null(names(size))) {
    stopifnot(length(setdiff(uniregion, names(size)))==0)
  } else names(size) = uniregion
  
  # get the grid map
  label = region[!duplicated(poly)]
  names(label) = unipoly
  if (is.null(gridmap)) {
    gridmap=checkerboard(x,y,poly,label,xbins=nrows,ybins=ncols,plot=FALSE,txtbar=ifelse((nrows+ncols)>200,TRUE,FALSE))
  } else {
    stopifnot(!is.null(attr(gridmap, "nbins")))
    nrows = attr(gridmap, "nbins")[1]
    ncols = attr(gridmap, "nbins")[2]
  }
  
  xlim = range(gridmap$x)
  ylim = range(gridmap$y)
  
  # get the matrix
  gridrecog = matrix(as.character(gridmap$label),nrow=ncols)
  gridcount = matrix_count(gridrecog)
  #gridcount = matrix(sapply(gridrecog, function(x){sum(gridrecog==x,na.rm=TRUE)}),nrow=nrow(gridrecog))
  gridcount[is.na(gridrecog)] = 1
  gridsize = matrix(0, nrow=ncols, ncol=nrows)
  for (i in 1:length(uniregion)) gridsize[gridrecog==uniregion[i]] = size[uniregion[i]]
  
  # cartogram
  tmp=as.vector(gridsize)/as.vector(gridcount)
  tmp[is.na(gridrecog)] = max(c(0.0001,sum(range(tmp[!is.na(gridrecog)])*c(1-blank.init,blank.init))))
  grid = matrix(tmp,nrow=ncols)
  grid = addBoundary(grid, sea=sea.width, land.mean = max(c(0.0001,sum(range(grid)*c(1-sea.init,sea.init)))))
  extra = attr(grid, 'extra')
  res = cartogram(grid, zero=FALSE, blur=blur, sea=NA)
  
  # prediction
  pred1 = predict(res,(x - xlim[1]) / (diff(xlim)) * (nrows-1) + extra[2],
                 (y - ylim[1]) / (diff(ylim)) * (ncols-1) + extra[1])
  #pred2 = predict2.Cartogram(res,(x - xlim[1]) / (diff(xlim)) * (nrows-1) + extra[2],
  #               (y - ylim[1]) / (diff(ylim)) * (ncols-1) + extra[1])
  final = data.frame(x = (pred1$x - extra[2]) / (ncols - 1) * diff(ylim) + min(y),
                     y = (pred1$y - extra[1]) / (nrows - 1) * diff(xlim) + min(x),
                     abbr = region, poly = poly, stringsAsFactors=FALSE)
  finalx = range(final$x, na.rm=TRUE)
  final$x = (final$x-finalx[1]) / (diff(finalx)) * (diff(xlim)) + min(x)
  finaly = range(final$y, na.rm=TRUE)
  final$y = (final$y-finaly[1]) / (diff(finaly)) * (diff(ylim)) + min(y)
  
  return(final)
}

matrix_count = function(x) {
  p = ncol(x)
  x = c(x)
  m = table(x)
  matrix(m[as.character(x)], ncol = p)
}

##' Interpolate between two maps
##' @param coord1,coord2 coordinates of the two maps. Must be the lists or data frames that contain two vectors named x and y respectively.
##' @param wt weight of the interpolation between the cartogram and original map. Must be within 0 and 1.
##' @export
##' 
interpolate = function(coord1,coord2,wt=1){
  intplt = coord1
  if (wt < 1) {
    intplt$x = coord1$x * wt + coord2$x * (1-wt)
    intplt$y = coord1$y * wt + coord2$y * (1-wt)
  }
  return(intplt)
}


##' Plot the contiguous cartogram or map
##' @param coord a data frame contains x, y, abbr, and poly.
##' @param color a vector of color to fill in the polygons. Auto-completed
##' if the length does not match with ratio. We suggest the user to give
##' the region names to the vector; if not given, then the order of the
##' ratios will match the regions.
##' @param border a vector of polygon borders. Auto-completed if the length 
##' does not match with ratio. We suggest to give the region names to the
##' vector; if not given, then the order of the ratios will match the regions.
##' @param name.text whether to print the abbr on the polygons
##' @export
##' 
plotmap = function(coord, color, border=0, name.text=FALSE, ...){
  uniregion = unique(coord$abbr); unipoly = unique(coord$poly)

  # color
  if (!is.null(color)) color=complete_color(color, length(uniregion))
  if (!is.null(names(color))) {
    stopifnot(length(setdiff(uniregion,names(color)))==0)
  } else if (!is.null(color)) names(color) = uniregion
  
  # border
  if (!is.null(border)) border=complete_color(border, length(uniregion))
  if (!is.null(names(border))) {
    stopifnot(length(setdiff(uniregion,names(border)))==0)
  } else names(border) = uniregion
  
  # label
  label = coord$abbr[!duplicated(coord$poly)]
  names(label) = unipoly
  
  # plot
  plot(coord$x,coord$y,type='n',xlab='',ylab='',xaxt='n',yaxt='n',frame=FALSE, ...)
  for (i in 1:length(unipoly)) {
    tmpidx = which(coord$poly == unipoly[i])
    polygon(coord$x[tmpidx], coord$y[tmpidx], border = border[label[unipoly[i]]], col = color[label[unipoly[i]]])
  }
  if (name.text) {
    for (i in 1:length(uniregion)) {
      tmpidx = which(coord$abbr == uniregion[i])
      crtcenter = centroid_polygon(coord[tmpidx,3:4])
      text(crtcenter[1], crtcenter[2], uniregion[i], cex=0.8)
    }
  }
}
