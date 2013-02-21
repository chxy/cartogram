##' Draw a circle
##' 
##' This function is used to compute the locations of the circle 
##' border and draw multiple circles. 
##' It borrows the code from plotrix::draw.circle
##' 
##' @param xvec X-coordinates
##' @param yvec Y-coordinates
##' @param rvec Radii
##' @param vertex The number of vertices of the circle
##' @param border Color of border
##' @param col Color to render in circle
##' @param add Whether the circles are added to another plot.
##' @param square A logical value to determine whether to draw squares.
##' @export
##' @examples
##' x=y=1:5
##' r=5:1/5
##' circle(x,y,r,add=FALSE,asp=1)
##' circle(x,y,r,vertex=6,add=TRUE)  # hexagon
##' circle(x,y,r,vertex=4,add=TRUE)  # diamond
##' circle(x,y,r,square=TRUE,add=TRUE)  # square
##'
circle = function(xvec,yvec,rvec,vertex=100,border=1,col=NULL,add=TRUE, square=FALSE,...){
    n=length(xvec)
    stopifnot(length(yvec)==n && n==length(rvec))
    if (length(border) < n)  border = rep(border, length.out = n)
    if (!is.null(col) && length(col) < n) col = rep(col, length.out = n)
    # xylim = par("usr")
    # plotdim = par("pin")
    # ymult = (xylim[4] - xylim[3])/(xylim[2] - xylim[1]) * plotdim[1]/plotdim[2]
    if (square) {
        angles = seq(pi / 4, 7 * pi / 4, by = pi / 2)
    } else {
        angle.inc = 2 * pi / vertex
        angles = seq(0, 2 * pi - angle.inc, by = angle.inc)
    }
    if (!add) plot(c(min(x-r),max(x+r)),c(min(y-r),max(y+r)),type='n',xlab='x',ylab='y',...)
    for (i in 1:n){
        xv <- cos(angles) * rvec[i] + xvec[i]
        yv <- sin(angles) * rvec[i] + yvec[i]
        polygon(xv, yv, border = border[i], col = col)
    }
}

##' Draw a square
##' 
##' @param xvec X-coordinates
##' @param yvec Y-coordinates
##' @param rvec Half of the side length.
##' @param border Color of border
##' @param col Color to render in circle
##' @param add Whether the circles are added to another plot.
##' @export
##' @examples
##' x=y=1:5
##' r=5:1/5
##' square(x,y,r,add=FALSE,asp=1)
##'
square = function(xvec,yvec,rvec,border=1,col=NULL,add=TRUE,...){
    n=length(xvec)
    stopifnot(length(yvec)==n && n==length(rvec))
    if (length(border) < n)  border = rep(border, length.out = n)
    if (!is.null(col) && length(col) < n) col = rep(col, length.out = n)
    if (!add) plot(c(min(x-r),max(x+r)),c(min(y-r),max(y+r)),type='n',xlab='x',ylab='y',...)
    for (i in 1:n){
        xv <- c(-1, 1, 1, -1) * rvec[i] + xvec[i]
        yv <- c(-1, -1, 1, 1) * rvec[i] + yvec[i]
        polygon(xv, yv, border = border[i], col = col)
    }
}
