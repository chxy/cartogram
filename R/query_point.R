##' Whether a point locates in the squares.
##' 
##' @param p Location of the point. Should be a vector of length 2.
##' @param sqxrange A matrix with 2 columns. The first column is the lower bound of the squares' x-coordinates, and the second column is the upper bound of the x-coordinates.
##' @param sqyrange A matrix with 2 columns. The first column is the lower bound of the squares' y-coordinates, and the second column is the upper bound of the y-coordinates.
##' @param sqname A vector of the square names. Default to be the rownames of sqxrange.
##' @export
##' @examples
##' xrange = yrange = matrix(c(1:5,4:8),ncol=2)
##' pointinsquares(c(3.5,3.5),xrange,yrange,LETTERS[1:5])
##' 
pointinsquares = function(p,sqxrange,sqyrange,sqname=rownames(sqxrange)){
  a=which(p[1]>=sqxrange[,1] & p[1]<=sqxrange[,2] & p[2]>=sqyrange[,1] & p[2]<=sqyrange[,2])
  b=rep(NA,length(sqname))
  if (length(a)>0) {
    b[1:length(a)]=sqname[a]
  }
  return(c(length(a),b))
}
