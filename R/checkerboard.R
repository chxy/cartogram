##' Produce a Checkerboard Map
##' 
##' @param xborder A vector of x-coordinates of the border.
##' @param yborder A vector of y-coordinates of the border.
##' @param name A vector of the polygon names. Must be of the same length with \code{xborder}, and unique for each polygon.
##' @param label A vector of the displayed names for polygons. One label could serve several polygons. Must be unique and \code{names(label)} must contain all the unique input \code{name}.
##' @param binwidth A vector of length 2 indicating the binwidths in x and y direction. Default to be 1/50 of the range.
##' @param plot Whether to plot the checkerboard map.
##' @param pal palette. The input for the argument "col" in \code{image()}.
##' @return A data frame of four columns: x and y coordinates of the grids, the name and the label that a grid point belongs to.
##' @example inst/ex_gridmap.R
##' @export
##'
checkerboard = function(xborder,yborder,name,label=NULL,nbins=NULL,
                        binwidth=c(diff(range(xborder))/50,diff(range(yborder))/50),
                        plot=TRUE, pal=NULL){
    nborder=length(name)
    stopifnot(nborder==length(xborder),nborder==length(yborder))
    if (is.null(label)) {label=sort(unique(name)); names(label)=label} else {
        region=unique(name)
        stopifnot(length(label)>=length(region), all(region %in% names(label)))
    }    
    if (!is.null(nbins)) {
        nbins=round(nbins)
        stopifnot(nbins>10)
        binwidth=c(diff(range(xborder))/nbins,diff(range(yborder))/nbins)
    }
    
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
    
    if (is.null(pal)) {
        set.seed(1000)
        pal=palette(sample(c(rainbow(24),colors()[c(1,4:11,13:26,28:29,76:87)*5+3]),length(unique(label)),rep=FALSE))
    }
    
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
    
    if (plot) {
        # plot(y~x,data=grids,pch=15,col=grids$label)
        grids=grids[order(grids$y,grids$x),]
        image(xgrid,ygrid,matrix(as.integer(grids$label),nrow=length(xgrid),ncol=length(ygrid)),col=pal,xlab='',ylab='',xaxt='n',yaxt='n',frame=F)
    } 
    
    res = grids[,c(1:3,5)]
    attr(res,'nbins') = round(diff(range(xborder))/binwidth[1])
    return(res)
}


##' Produce a Grid-based Cartogram by "reversi"/"game of life"
##' 
##' The iteration will stop if the SSE does not change for the latest 5 steps.
##' 
##' @param grids The output of function \code{checkerboard}.
##' @param density A vector of the variable of interest. \code{names(density)} should match \code{grids$label}.
##' @param iteration The number of iterations.
##' @param animation Whether to plot the map in each iteration.
##' @param sleep.time Only works when animation=TRUE.
##' @return grids indicates the grids and their new affiliation.
##' @return count original size, goal size and current size of the regions.
##' @return error sum of squared error and sum of absolute error
##' @example inst/ex_gridmap.R
##' @export
##'
grid_cart = function(grids,density,iteration=100,animation=FALSE,sleep.time=0.1,preserve.sea=TRUE){
    all_labels=levels(grids$label)
    grids$label=as.character(grids$label)
    stopifnot(length(intersect(names(density),unique(grids$label)))>1)
    grids=grids[grids$label %in% names(density) | is.na(grids$label),]
    grids=grids[order(grids$x,grids$y),]
    
    xgrid=length(unique(grids$x))
    ygrid=length(unique(grids$y))
    ncell=xgrid*ygrid
    if (ncell!=nrow(grids)) stop("The input is not a full grid.")
    crtgrid=grids
    sse=sae=rep(NA,iteration)
    
    dens=data.frame(density=density,orig_area=table(grids$label)[names(density)])
    rownames(dens)=names(density)
    dens=dens[!is.na(dens$orig_area),]
    dens$goal=round(dens$density*mean(dens$orig_area)/mean(dens$density))
    dens$crt_area=dens$orig_area
    
    if (!preserve.sea){
        grids$label[is.na(grids$label)]="outer"
        crtgrid=grids
        dens=rbind(dens,dens[1,])
        rownames(dens)[nrow(dens)]="outer"
        dens["outer","density"]=mean(density)
        dens["outer",2]=dens["outer",3]=dens["outer",4]=ncell-sum(dens$orig_area)
    }
    
    # sort the regions from center to edge
    # geo_ord=order(geocenter(grids)$label_ord)
    set.seed(1000)
    pal=palette(sample(c(rainbow(24),colors()[c(1,4:11,13:26,28:29,76:87)*5+3]),length(all_labels),rep=FALSE))

    txtpb = txtProgressBar(min=0,max=1,width = 40,style=3)
    
    par(mfrow=c(1,2))
    for (k in 1:iteration){
        if (animation) {
            #plot(y~x,data=crtgrid,pch=15,col=factor(crtgrid$label,levels=all_labels),cex=1.7)
            image(unique(crtgrid$x),unique(crtgrid$y),matrix(as.integer(factor(crtgrid$label,levels=all_labels)),nrow=length(unique(crtgrid$x)),ncol=length(unique(crtgrid$y)),byrow=TRUE),col=pal,xlab='',ylab='',xaxt='n',yaxt='n',frame=F)
            plot(1:iteration,sse,type='o',ylim=c(0,0.3))
            Sys.sleep(sleep.time)
        }
        
        # find the regions whose current area is greater than the ideal area
        # then we can shrink them by peeling the cells
        den_ord=order(dens$goal-dens$crt_area)[1:sum((dens$goal-dens$crt_area)<0)]
        
        for (j in 1:length(den_ord)){
            tmpgrid=crtgrid
            tmparea=dens[,3:4]
            cell=rownames(dens)[den_ord[j]]
            idxj = which(crtgrid$label==cell)
            for (i in idxj){
                bottom = i-1
                top = i+1
                left = i-ygrid
                right = i+ygrid
                topleft = i+1-ygrid
                topright = i+1+ygrid
                bottomleft = i-1-ygrid
                bottomright = i-1+ygrid
                if (i %% ygrid == 1)  {bottom = bottomleft = bottomright = NA}
                if (i %% ygrid == 0)  {top    = topleft    = topright    = NA}
                if (i <= ygrid)       {left   = bottomleft = topleft     = NA}
                if (i >  ncell-ygrid) {right  = topright   = bottomright = NA}
                eightnbrs = c(topleft,left,bottomleft,bottom,bottomright,right,topright,top) 
                cellnbrs=tmpgrid[eightnbrs,4]
                #security=sum(c(1,5,1,5,1,5,1,5)*(cellnbrs == cell),na.rm=TRUE)
                #maxsecurity=sum(c(1,5,1,5,1,5,1,5)*(!is.na(cellnbrs)))
                if (any(cellnbrs != cell, na.rm=TRUE) && dens[cell,'crt_area']>2) {
                    pressure = (c(1,2,1,2,1,2,1,2)) * (dens[cellnbrs,'goal']-tmparea[cellnbrs,'crt_area'])
                    #goal     = tapply(pressure,cellnbrs,sum,na.rm=TRUE)
                    goal    = tapply(pressure,cellnbrs,max,na.rm=TRUE)
                    candidate= names(goal)[which.max(goal)]
                    if ((!cell %in% cellnbrs) || (goal[candidate]-goal[cell])>4  && (!candidate %in% rownames(dens)[den_ord[1:j]])) {
                        crtgrid[i,4]=candidate
                        dens$crt_area=table(crtgrid$label)[rownames(dens)]
                    }
                }
            }
        }
        sse[k]=sum((dens$goal-dens$crt_area)^2)/ncell
        sae[k]=sum(abs(dens$goal-dens$crt_area))/ncell
        if (all(sse[k-(0:min(4,k-1))]==sse[k])) k=iteration
        # cat(k," step, SSE - ",sse[k],", ABS error - ",sae[k],"\n")
        setTxtProgressBar(txtpb, k/iteration)
    }
    close(txtpb)
    
    #plot(y~x,data=crtgrid,pch=15,col=factor(crtgrid$label,levels=all_labels),cex=1.7)
    image(unique(crtgrid$x),unique(crtgrid$y),matrix(as.integer(factor(crtgrid$label,levels=all_labels)),nrow=length(unique(crtgrid$x)),ncol=length(unique(crtgrid$y)),byrow=TRUE),col=pal,xlab='',ylab='',xaxt='n',yaxt='n',frame=F)
    plot(1:iteration,sse,type='o')
    return(list(grids=crtgrid[,c(1,2,4)],count=dens,error=data.frame(SSE=sse,AE=sae)))
}


##' Find the geographic center of a map
##' 
##' @param grids output of function \code{checkerboard}.
##' @return the order of regions from the center to the edge.
##' @export
##' @examples
##' geocenter(gridmap)
geocenter = function(grids){
    grids$label=as.character(grids$label)
    rownames(grids)=paste(grids$x,grids$y)
    
    grids=grids[!is.na(grids$poly),]
    xmean=mean(grids$x); xstd=sd(grids$x)
    ymean=mean(grids$y); ystd=sd(grids$y)
    
    grids$weight=((grids$x-xmean)/xstd)^2+((grids$y-ymean)/ystd)^2
    grids$order=rank(grids$weight,ties.method="first")
    
    ord=order(grids$order)
    poly=unique(grids[ord,'poly'])
    label=unique(grids[ord,'label'])
    
    res=list(poly_ord=1:length(poly),label_ord=1:length(label))
    names(res$poly_ord)=poly
    names(res$label_ord)=label
    res$poly_ord=res$poly_ord[order(poly)]
    res$label_ord=res$label_ord[order(label)]
    res
}


##' Example simulator
##' 
##' @param nr number of rows
##' @param nc number of columns
##' @param g number of groups
##' @param seed seeds for reproduce the example
##' @return a data frame of columns: x(row coord), y(column coord), m(group), 'label'(group name), 'd'(density).
##' @export
##' @examples
##' grid.sim()
##' grid.sim(16,12,10)
##' 
grid.sim = function(nr=15,nc=15,g=12,seed=100){
    set.seed(seed)
    p=palette(sample(c(rainbow(24),colors()[c(1,4:11,13:26,28:29,76:87)*5+3]),g,rep=FALSE))
    a=matrix(rpois(nr*nc,2),nrow=nr,ncol=nc)
    b=cbind(expand.grid(1:nr,1:nc),as.vector(a))
    colnames(b)=c('x','y','v')
    xmean=mean(b$x); xstd=sd(b$x)
    ymean=mean(b$y); ystd=sd(b$y)
    b$w=((b$x-xmean)/xstd)^2+((b$y-ymean)/ystd)^2
    b$m=kmeans(b[,c('x','y','v','x','y','w')],g,iter.max=100)$cluster
    b$d=round(5*sin(exp(b$m))+10,3)
    b$label=as.factor(LETTERS[b$m])
    r=matrix(b$m,nrow=nr,ncol=nc)
    image(1:nr,1:nc,r,col=p,xlab='',ylab='',xaxt='n',yaxt='n',frame=F,asp=nc/nr)
    b[,c('x','y','m','label','d')]
}
