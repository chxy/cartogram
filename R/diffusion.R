##' Draw a diffusion-based cartogram
##' 
##' @param x a vector of x-coordinates of the border
##' @param y a vector of y-coordinates of the border.
##' @param poly a vector of the polygon names. Must be of the same length with \code{x}, and unique for each polygon.
##' @param region a vector of the region names. Must be of the same length with \code{x}, and unique for each region.
##' @param size the size vector for regions (length must be equal to the number of regions)
##' @param color the color vector for regions (length must be equal to the number of regions)
##' @param border the border vector for regions (length must be equal to the number of regions)
##' @param gridmap output of the function \code{checkerboard}. Default to be null.
##' @param nrows the resolution to get a grid. If \code{gridmap} is not null, then nrows must be the attribute 'nbins' of gridmap.
##' @param diffuse a positive value to control the diffusing/shrinking rate
##' @param blank.init fill the NA's of the grids with blank.init * min(size)
##' @param interpolate weight of the interpolation between the cartogram and original map. Must be within 0 and 1.
##' @param ... other paramters passed to the cartogram function
##' @return a data frame with four columns: new x and y coordinates, polygon names and region names.
##' @export
##' @example inst/ex_diffusion.R
##' 

Rcartogram = function(x, y, poly, region, size, color, border=0, gridmap=NULL, nrows=50, diffuse=2, blank.init=.8, interpolate=1, ...){
  library(Rcartogram)
  uniregion = unique(region); unipoly = unique(poly)
  stopifnot(length(x)==length(y), length(x)==length(poly), 
            length(poly)==length(region), length(size)==length(uniregion))
  if (!is.null(names(size))) {
    stopifnot(length(setdiff(uniregion, names(size)))==0)
  } else names(size) = uniregion
  
  ncols = nrows
  xlim = range(x, na.rm = TRUE)
  ylim = range(y, na.rm = TRUE)
  
  # color and border
  if (!is.null(color)) color=complete_color(color, length(size))
  if (!is.null(border)) border=complete_color(border, length(size))
  if (!is.null(names(color))) {
    stopifnot(length(setdiff(uniregion,names(color)))==0)
  } else if (!is.null(color)) names(color) = uniregion
  if (!is.null(names(border))) {
    stopifnot(length(setdiff(uniregion,names(border)))==0)
  } else names(border) = uniregion
  
  # get the grid map
  label = region[!duplicated(poly)]
  names(label) = unipoly
  if (is.null(gridmap)) {
    gridmap=checkerboard(x,y,poly,label,nbins=nrows,plot=FALSE)  
  } else {
    stopifnot(exists(attr(gridmap, "nbins")))
    ncols = nrows = attr(gridmap, "nbins")
  }
  
  # get the matrix
  gridrecog = matrix(as.character(gridmap$label),nrow=nrows)
  gridcount = matrix(sapply(gridrecog, function(x){sum(gridrecog==x,na.rm=TRUE)}),nrow=nrow(gridrecog))
  gridcount[is.na(gridrecog)] = diffuse
  gridsize = matrix(min(size, na.rm=TRUE)*blank.init, nrow=nrows, ncol=ncols)
  for (i in 1:length(uniregion)) gridsize[gridrecog==uniregion[i]] = size[uniregion[i]]
  
  # cartogram
  tmp=as.vector(gridsize)/as.vector(gridcount)
  grid = matrix(tmp,nrow=nrows,ncol = ncols)
  grid = addBoundary(grid, sea=1, land.mean = min(grid))
  extra = attr(grid, 'extra')
  res = cartogram(grid, ...)
  
  # prediction
  pred = predict(res, (x - xlim[1]) / (diff(xlim)) * (nrows - 1) + 1 + extra[1],
                 (y - ylim[1]) / (diff(ylim)) * (ncols - 1) + 1 + extra[2])
  final = data.frame(x = (pred$x - extra[1] - 1) / (nrows - 1) * diff(xlim) + xlim[1],
                     y = (pred$y - extra[2] - 1) / (ncols - 1) * diff(ylim) + ylim[1],
                     abbr = region, poly = poly, stringsAsFactors=FALSE)
  intplt = final
  if (interpolate < 1) {
    intplt$x = final$x * interpolate + x * (1-interpolate)
    intplt$y = final$y * interpolate + y * (1-interpolate)
  }
  
  plot(intplt$x,intplt$y,type='n',xlab='',ylab='',xaxt='n',yaxt='n',frame=FALSE)
  for (i in 1:length(unipoly)) {
    tmpidx = which(poly == unipoly[i])
    polygon(intplt$x[tmpidx], intplt$y[tmpidx], border = border[label[unipoly[i]]], col = color[label[unipoly[i]]])
  }
  
  return(list(final=final,interpolation=intplt))
}
