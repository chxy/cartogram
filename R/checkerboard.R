##' Produce a Checkerboard Map
##' 
##' @param xborder A vector of x-coordinates of the border.
##' @param yborder A vector of y-coordinates of the border.
##' @param name A vector of the polygon names. Must be of the same length with \code{xborder}, and unique for each polygon.
##' @param label A vector of the displayed names for polygons. One label could serve several polygons. Must be unique and \code{names(label)} must contain all the unique input \code{name}.
##' @param binwidth A vector of length 2 indicating the binwidths in x and y direction. Default to be 1/50 of the range.
##' @param plot Whether to plot the checkerboard map.
##' @return A data frame of four columns: x and y coordinates of the grids, the name and the label that a grid point belongs to.
##' @example inst/ex_gridmap.R
##' @export
##'
checkerboard = function(xborder,yborder,name,label=NULL,
                        binwidth=c(diff(range(xborder))/50,diff(range(yborder))/50),
                        plot=TRUE){
    nborder=length(name)
    stopifnot(nborder==length(xborder),nborder==length(yborder))
    if (is.null(label)) {label=sort(unique(name)); names(label)=label} else {
        region=unique(name)
        stopifnot(length(label)>=length(region), all(region %in% names(label)))
    }    
    if (length(density)==1) {density=rep(1,length(region)); names(density)=region}
    stopifnot(length(density)>=length(region),all(region %in% names(density)))
    
    xrange=matrix(unlist(tapply(xborder,name,function(a){c(min(a),max(a))})),ncol=2,byrow=TRUE)
    yrange=matrix(unlist(tapply(yborder,name,function(a){c(min(a),max(a))})),ncol=2,byrow=TRUE)

    xgrid=seq(min(xrange)+0.4*binwidth[1],max(xrange),by=binwidth[1])
    ygrid=seq(min(yrange)+0.4*binwidth[2],max(yrange),by=binwidth[2])
    
    grids=data.frame(x=rep(xgrid,each=length(ygrid)),y=rep(ygrid,times=length(xgrid)))    
    query=t(apply(grids,1,pointinsquares,sqxrange=xrange,sqyrange=yrange,sqname=sort(unique(name))))
    query=query[,colSums(is.na(query))!=nrow(query)]
    maxin=ncol(query)
    grids[,3:4]=query[,2:1]
    colnames(grids)[3:4]=c('poly','edge')
    
    txtpb = txtProgressBar(min=0,max=1,width = 40,style=3)
    
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
        if (any(whicharea>0)) {grids[i,3]=pol[whicharea>0][1]}
        setTxtProgressBar(txtpb, 0.5/length(idx)*k)
    }
    idx = which(grids$edge==1)
    for (k in 1:length(idx)){
        i=idx[k]
        pol=grids[i,3]
        ptx=grids[i,1]
        pty=grids[i,2]
        whicharea=point.in.polygon(ptx,pty,xborder[name==pol],yborder[name==pol])
        if (whicharea==0) {grids[i,3]=NA}
        setTxtProgressBar(txtpb, 0.5/length(idx)*k+0.5)
    }
    close(txtpb)
    
    grids$label = factor(label[grids$poly],levels=unique(label))
    if (plot) plot(y~x,data=grids,pch=15,col=grids$label)
    return(grids[,c(1:3,5)])
}


##' Produce a Grid-based Cartogram
##' 
##' @param grids The output of function \code{checkerboard}.
##' @param density A vector of the variable of interest. \code{names(density)} should match \code{grids$label}.
##' @param iteration The number of iterations.
##' @param animation Whether to plot the map in each iteration.
##' @param sleep.time Only works when animation=TRUE.
##' @return grids indicates the grids and their new affiliation.
##' @return count  
##' @example inst/ex_gridmap.R
##' @export
##'

grid_cart = function(grids,density,iteration=100,animation=FALSE,sleep.time=0.2){
    all_labels=levels(grids$label)
    grids$label=as.character(grids$label)
    stopifnot(length(intersect(names(density),unique(grids$label)))>1)
    grids=grids[grids$label %in% names(density) | is.na(grids$label),]
    
    dens=data.frame(density=density,orig_area=table(grids$label)[names(density)])
    rownames(dens)=names(density)
    dens=dens[!is.na(dens$orig_area),]
    dens$goal=round(dens$density*mean(dens$orig_area)/mean(dens$density))
    dens$crt_area=dens$orig_area
    
    xgrid=length(unique(grids$x))
    ygrid=length(unique(grids$y))
    ncell=xgrid*ygrid
    if (ncell!=nrow(grids)) stop("The input is not a full grid.")
    crtgrid=grids
    sse=rep(NA,iteration)
    
    for (k in 1:iteration){
        if (animation) {
            plot(y~x,data=crtgrid,pch=15,col=factor(crtgrid$label,levels=all_labels))
            Sys.sleep(sleep.time)
        }
        tmpgrid=crtgrid
        ord=order(dens$goal-dens$crt_area)[1:sum((dens$goal-dens$crt_area)<0)]
        for (j in ord){
            idxj = which(crtgrid$label==rownames(dens)[j])
            for (i in idxj){
                bottom = i-1
                top    = i+1
                left   = i-ygrid
                right  = i+ygrid
                if (i %% ygrid == 1)  bottom = NA
                if (i %% ygrid == 0)  top    = NA
                if (i <= ygrid)       left   = NA
                if (i >  ncell-ygrid) right  = NA
                fournbrs=c(bottom,top,left,right)
                cell=tmpgrid[i,4]
                cellnbrs=tmpgrid[fournbrs,4]
                cond1 = all(cellnbrs[1:2]==cell,na.rm=TRUE) & all(cellnbrs[3:4]!=cell,na.rm=TRUE)
                cond2 = all(cellnbrs[1:2]!=cell,na.rm=TRUE) & all(cellnbrs[3:4]==cell,na.rm=TRUE)
                if (any(cellnbrs != cell, na.rm=TRUE) && (!cond1) && (!cond2)) {
                    mynbrs=na.omit(cellnbrs[cellnbrs != cell])
                    goal=dens[mynbrs,'goal']
                    candidate=mynbrs[which.max(goal)]
                    if ((!candidate %in% rownames(dens)[ord[1:j]]) && dens[cell,'crt_area']>3) {
                        crtgrid[i,4]=candidate
                        dens$crt_area=table(crtgrid$label)[rownames(dens)]
                    }
                }
            }
        }
        sse[k]=sum((dens$goal-dens$crt_area)^2)
        cat(k," step - SSE - ",sse[k],"\n")
    }
    plot(y~x,data=crtgrid,pch=15,col=factor(crtgrid$label,levels=all_labels))
    return(list(grids=crtgrid[,c(1,2,4)],count=dens,SSE=sse))
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