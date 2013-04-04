##' Find the centroid of a polygon.
##' 
##' This function is copied from the package \code{maps}.
##' @param p a data frame of two columns.
##' @return the x and y coordinates of the centroid.
##'
centroid_polygon = function(p) {
    n = nrow(p)
    i = c(n, 1:(n - 1))
    x1 = p[, 1]
    x2 = p[i, 1]
    y1 = p[, 2]
    y2 = p[i, 2]
    a = x1*y2 - x2*y1
    s = sum(a)*3
    if(s == 0) c(mean(x1), mean(y1))
    else c(sum((x1 + x2)*a)/s, sum((y1 + y2)*a)/s)
}

##' Find the centroids for the polygons.
##' 
##' @param region Region names.
##' @param x X-coordinates.
##' @param y Y-coordinates.
##' @return A data frame with region names and coordinates of the centroids.
##' @export
##' @examples
##' data(usCapitals)
##' state_centroid=centroid(state$abbr,state$x,state$y)
##'
centroid = function(region,x,y){
    stopifnot(length(x)==length(region), length(y)==length(region))
    dat=data.frame(r=region,x=x,y=y)
    uniregion=sort(unique(region))
    res=data.frame(region=uniregion,x=NA,y=NA)
    res$region=as.character(res$region)
    for (i in 1:nrow(res)) res[i,2:3]=centroid_polygon(dat[dat$r==uniregion[i],2:3])
    return(res)
}

##' Find the counts of shared border and perimeter for polygons.
##' 
##' @param region Region names.
##' @param x X-coordinates of all the border.
##' @param y Y-coordinates of all the border.
##' @return A square matrix with diagonal elements being the perimeter (count of border points) of the polygon, and the non-diagonal cells being the count of shared border between the polygons on the row and column.
##' @export
##' @examples
##' data(usCapitals)
##' state_border=border_summary(state$abbr,state$x,state$y)
##' 
border_summary = function(region, x, y){
    stopifnot(length(x)==length(region), length(y)==length(region))
    name = sort(unique(region))
    k = length(name)
    digitx = max(min(nchar(as.character(x))),5)
    digity = max(min(nchar(as.character(y))),5)
    dat=data.frame(r=region,x=x,y=y)
    dat$r=as.character(dat$r)
    dat$p=paste(round(dat$x,digitx),round(dat$y,digity))
    dat=dat[!duplicated(dat),]
    perimeter=table(dat$r)
    
    censordat=dat[duplicated(dat$p)|duplicated(dat$p,fromLast=TRUE),c('r','p')]
    peri=table(censordat$r)
    uniregion=names(peri)[order(peri,decreasing=TRUE)]
    
    s=0
    border=data.frame(r=NULL,r2=NULL)
    while (nrow(censordat)>0){
        s=s+1
        idx1=censordat$r %in% uniregion[s]
        if (sum(idx1)>0){
            tmp1=censordat[idx1,]
            censordat=censordat[!idx1,]
            idx2=censordat$p %in% tmp1$p
            tmp2=censordat[idx2,]
            censordat=censordat[!idx2,]
            
            idx3= duplicated(tmp2$p) | duplicated(tmp2$p,fromLast=TRUE)
            if (sum(idx3)>0) {
                tmp3=tmp2[idx3,]
                tmp2=tmp2[!idx3,]
                tmpp=tmp1[tmp1$p %in% tmp3$p, c('p','r')]
                for (i in 1:nrow(tmpp)){
                    tmpdat=tmp3[tmp3$p==tmpp$p[i],]
                    tmpstate=c(tmpp$r[i],tmpdat$r)
                    tmpborder=t(combn(tmpstate,2))
                    colnames(tmpborder)=c('r','r2')
                    border=rbind(border,tmpborder)
                }
            }        
            tmpp=tmp1[tmp1$p %in% tmp2$p, c('p','r')]
            tmpp=tmpp[order(tmpp$p),]
            tmpp$r2=tmp2$r[order(tmp2$p)]
            border=rbind(border,tmpp[,2:3])        
        }
    }
    
    res=matrix(0,nrow=k,ncol=k)
    colnames(res)=rownames(res)=name
    diag(res)=perimeter
    res1=table(border)
    cnames=colnames(res1)
    rnames=rownames(res1)
    for (i in 1:nrow(res1)){
        for (j in 1:ncol(res1)){
            if (res1[i,j]>0) {
                res[rnames[i],cnames[j]]=res[rnames[i],cnames[j]]+res1[i,j]
                res[cnames[j],rnames[i]]=res[cnames[j],rnames[i]]+res1[i,j]
            }
        }
    }
    return(res)
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
