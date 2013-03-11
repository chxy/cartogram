##' Produce a Checkerboard Cartogram.
##' 
##' @param xborder A vector of x-coordinates of the border.
##' @param yborder A vector of y-coordinates of the border.
##' @param name A vector of the polygon names. Must be of the same length with xborder, and unique for each polygon.
##' @param density A vector of the variable of interest. The length is the same as the number of labels. A name must be given to each element to match the label.
##' @param label A vector of the displayed names for polygons. One label could be used for several polygons. Must be unique.
##' @param binwidth A vector of length 2 indicating the binwidths in x and y direction. Default to be 1/50 of the range.
##' @example inst/ex_gridmap.R
##' @export
##'
checkerboard = function(xborder,yborder,name,label=NULL,density=1,
                        binwidth=c(diff(range(xborder))/50,diff(range(yborder))/50)){
    nborder=length(name)
    stopifnot(nborder==length(xborder),nborder==length(yborder))
    if (is.null(label)) {label=sort(unique(name)); names(label)=label} else {
        region=unique(name)
        stopifnot(length(label)>=length(region),all(region %in% names(label)))
    }    
    if (length(density)==1) {density=rep(1,length(region)); names(density)=region}
    stopifnot(length(density)>=length(region),all(region %in% names(density)))
    
    xrange=matrix(unlist(tapply(xborder,name,function(a){c(min(a),max(a))})),ncol=2,byrow=TRUE)
    yrange=matrix(unlist(tapply(yborder,name,function(a){c(min(a),max(a))})),ncol=2,byrow=TRUE)

    xgrid=seq(min(xrange)+0.4*binwidth[1],max(xrange),by=binwidth[1])
    ygrid=seq(min(yrange)+0.4*binwidth[2],max(yrange),by=binwidth[2])
    
    grids=data.frame(x=rep(xgrid,each=length(ygrid)),y=rep(ygrid,times=length(xgrid)))    
    query=t(apply(grids,1,pointinsquares,sqxrange=xrange,sqyrange=yrange,sqname=unique(name)))
    query=query[,colSums(is.na(query))!=nrow(query)]
    maxin=ncol(query)
    grids[,3:4]=query[,1:2]
    colnames(grids)[3:4]=c('edge','poly1')
    
    library(sp)
    idx = which(grids$edge>1)
    for (k in 1:length(idx)){
        i=idx[k]
        pol=na.omit(query[i,2:maxin])
        ptx=grids[i,1]
        pty=grids[i,2]
        whicharea=rep(0,length(pol))
        for (j in 1:length(pol)){
            whicharea[j]=point.in.polygon(ptx,pty,xborder[name==pol[j]],yborder[name==pol[j]])
        }
        if (any(whicharea>0)) {grids[i,4]=pol[whicharea>0][1]}
        if (k%%100==0) print(k)
    }    
    idx = which(grids$edge==1)
    for (k in 1:length(idx)){
        i=idx[k]
        pol=query[i,2]
        ptx=grids[i,1]
        pty=grids[i,2]
        whicharea=point.in.polygon(ptx,pty,xborder[name==pol],yborder[name==pol])
        if (whicharea==0) {grids[i,4]=NA}
        if (k%%100==0) print(k)
    }
}

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