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
    if (!add) plot(c(min(xvec-rvec),max(xvec+rvec)),c(min(yvec-rvec),max(yvec+rvec)),type='n',xlab='longitude',ylab='latitude',...)
    for (i in 1:n){
        xv <- cos(angles) * rvec[i] + xvec[i]
        yv <- sin(angles) * rvec[i] + yvec[i]
        polygon(xv, yv, border = border[i], col = col[i])
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
    if (!add) plot(c(min(xvec-rvec),max(xvec+rvec)),c(min(yvec-rvec),max(yvec+rvec)),type='n',xlab='x',ylab='y',...)
    for (i in 1:n){
        xv <- c(-1, 1, 1, -1) * rvec[i] + xvec[i]
        yv <- c(-1, -1, 1, 1) * rvec[i] + yvec[i]
        polygon(xv, yv, border = border[i], col = col)
    }
}

##' Applying the Four Color Theorem to render the polygons.
##' 
##' @param nbr a list of the neighbor names. The format should be the same as the value of function \code{nbrlist()}.
##' @param start the starting region. Must be NULL or a name from \code{names(nbr)}.
##' @return a vector of assigned colors (integers in {1,2,3,4}). The names of the vector is the same as the name of the input "nbr".
##' @examples
##' data(usGeoInfo)
##' state_nbrs=nbrlist(state$state,state$x,state$y,corner=FALSE)
##' color=fct(state_nbrs)
##' library(maps)
##' mapcolor=color[gsub(":.*$","",map('state',plot=F)$names)]
##' map('state',fill=TRUE,col=mapcolor+1)
##' 
fct = function(nbr,start=NULL){
    region=names(nbr)
    stopifnot(!is.null(region), all(unlist(nbr) %in% region))
    if (!is.null(start)) {stopifnot(start %in% region)} else {start=names(which.max(lapply(nbr,length)))}
    
    n=length(region)
    res=rep(0,n)
    names(res)=paste("polygon",1:n)
    res[1]=1
    names(res)[1]=start
    for (i in 2:n){
        candidate=setdiff(nbr[[names(res)[i-1]]],names(res))
        if (length(candidate)==0) {candidate=setdiff(region,names(res))[1]} else {candidate=candidate[1]}
        wrongcolor=na.omit(res[nbr[[candidate]]])
        if (length(wrongcolor)==0) {
            res[i]=1
        } else {
            candcolor=setdiff(1:4,wrongcolor)
            if (length(candcolor)>0) {res[i]=candcolor[1]} else {warning("The starting polygon leads to a color conflict.")}
        }
        names(res)[i]=candidate
    }
    return(res)
}
