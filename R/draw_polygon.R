##' Draw a circle
##' 
##' @param xvec x-coordinates
##' @param yvec y-coordinates
##' @param rvec radii
##' @param vertex the number of vertices of the circle
##' @param border color of border
##' @param col color to render in circle
##' @param add Whether the circles are added to another plot.
##' @export
##' @examples
##' x=y=1:5
##' r=5:1/5
##' circle(x,y,r,add=FALSE,asp=1)
##'
circle = function(xvec,yvec,rvec,vertex=100,border=1,col=NULL,add=TRUE,...){
    # This function is used to compute the locations of the circle border 
    # and draw multiple circles.
    # Borrowing the code from plotrix::draw.circle
    n=length(xvec)
    stopifnot(length(yvec)==n && n==length(rvec))
    if (length(border) < n)  border = rep(border, length.out = n)
    if (!is.null(col) && length(col) < n) col = rep(col, length.out = n)
    # xylim = par("usr")
    # plotdim = par("pin")
    # ymult = (xylim[4] - xylim[3])/(xylim[2] - xylim[1]) * plotdim[1]/plotdim[2]
    angle.inc = 2 * pi / vertex
    angles = seq(0, 2 * pi - angle.inc, by = angle.inc)
    if (!add) plot(c(min(x-r),max(x+r)),c(min(y-r),max(y+r)),type='n',xlab='x',ylab='y',...)
    for (i in 1:n){
        xv <- cos(angles) * rvec[i] + xvec[i]
        yv <- sin(angles) * rvec[i] + yvec[i]
        polygon(xv, yv, border = border[i], col = col)
    }
}
