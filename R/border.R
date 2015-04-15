##' Count the vertexes
##' 
##' Find the counts of shared vertexes between regions, 
##' and the number of vertexes for each region.
##' 
##' @param region Region names.
##' @param x X-coordinates of all the vertexes.
##' @param y Y-coordinates of all the vertexes.
##' @return A square matrix with diagonal elements being the perimeter 
##' (count of vertexes) of the regions, and the non-diagonal cells being 
##' the count of shared vertexes between the row and column regions.
##' @export
##' @examples
##' data(usCapitals)
##' state_vertex=border_summary_count(state$abbr,state$x,state$y)
##' 
border_summary_count = function(region, x, y){
  stopifnot(length(x)==length(region), length(y)==length(region))
  name = if (is.factor(region)) {levels(region)} else {sort(unique(region))}
  k = length(name)
  
  # check if the coordinates are identical
  # by rounding the numbers to maximal 5 digits and pasting x-y
  digitx = max(min(nchar(as.character(x))),5)
  digity = max(min(nchar(as.character(y))),5)
  dat=data.frame(r=region,x=x,y=y,stringsAsFactors=FALSE)
  dat$p=paste(round(dat$x,digitx),round(dat$y,digity))
  
  # remove the duplicated vertexes (usually the end point of each polygon)
  dat=dat[!duplicated(dat),]
  perimeter=table(dat$r)[name]
  
  # match the coordinates
  censordat=dat[duplicated(dat$p)|duplicated(dat$p,fromLast=TRUE),c('r','p')]
  peri=table(censordat$r)
  uniregion=names(peri)[order(peri,decreasing=TRUE)]
  
  # loop to get all pairs
  s=0
  border=data.frame(r=NULL,r2=NULL)
  while (nrow(censordat)>0){
    s=s+1
    idx1=censordat$r %in% uniregion[s]
    if (sum(idx1)==0) next
    
    # For each region, find the overlapped points and their belonging regions
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
  
  # tabulate the pairs
  border$r=factor(as.character(border$r),levels=name)
  border$r2=factor(as.character(border$r2),levels=name)
  res=table(border)
  class(res) = 'matrix'
  names(attr(res,'dimnames'))=NULL
  res=res+t(res)
  diag(res) = perimeter
  return(res)
}


##' Calculate the perimeter by region
##' 
##' @param region Region names.
##' @param poly Polygon names. One region may consist of several polygons.
##' @param x X-coordinates of all the vertexes.
##' @param y Y-coordinates of all the vertexes.
##' @return a vector of perimeters by region
##' @export
##' @examples
##' data(usCapitals)
##' perimeter(state$abbr,state$polygon,state$x,state$y)
perimeter = function(region, poly, x, y){
  stopifnot(length(x)==length(region), length(y)==length(region), length(poly)==length(region))
  dat = data.frame(r=as.character(region), l=as.character(poly),
                   x=x, y=y, stringsAsFactors=FALSE)
  dat = dat[which(diff(as.integer(factor(dat$l)))==0),]
  peri = by(dat,dat$l,function(s){sum(sqrt((diff(s$x))^2+(diff(s$y))^2))})
  tmp = dat[!duplicated(dat[,1:2]),1:2]
  tmp$peri = unname(peri[tmp$l])
  peri = tapply(tmp$peri,tmp$r,sum)
  return(peri)
}


##' Measure the length of edges
##' 
##' Calculate the sum length of all shared edges between regions, 
##' and the perimeter for each region.
##' 
##' @param region Region names.
##' @param poly Polygon names. One region may consist of several polygons.
##' @param x X-coordinates of all the vertexes.
##' @param y Y-coordinates of all the vertexes.
##' @param nb a list of the neighbors of each region. Default to be the outcome from 
##' the function \code{nbrlist()}.
##' @return A square matrix with diagonal elements being the perimeter 
##' (count of vertexes) of the regions, and the non-diagonal cells being 
##' the count of shared vertexes between the row and column regions.
##' @export
##' @examples
##' data(usCapitals)
##' state_border=border_summary_length(state$abbr,state$polygon,state$x,state$y)
##' system.time(border_summary_length(state$abbr,state$polygon,state$x,state$y))
##' 
border_summary_length = function(region, poly, x, y, nb=NULL){
  stopifnot(length(x)==length(region), length(y)==length(region), length(poly)==length(region))
  name = if (is.factor(region)) {levels(region)} else {sort(unique(region))}
  k = length(name)
  # To check if the coordinates are identical, by rounding the numbers to mininal 5 digits
  digitx = max(min(nchar(as.character(x))),5)
  digity = max(min(nchar(as.character(y))),5)
  dat = data.frame(r=as.character(region), l=as.character(poly),
                   x=x, y=y, stringsAsFactors=FALSE)
  polytail = unname(cumsum(table(poly)[unique(poly)]))
  polyhead = c(1,polytail[-length(polytail)]+1)
  polygroup = data.frame(head=polyhead, tail=polytail)
  polyidx = unlist(apply(polygroup,1,function(x) c((x[1]+1):x[2],unname(x[1]))))
  dat$qx = x[polyidx]
  dat$qy = y[polyidx]
  dat$p = paste(round(dat$x,digitx),round(dat$y,digity))
  dat$q = paste(round(dat$qx,digitx),round(dat$qy,digity))
  dat = dat[which(diff(as.integer(factor(dat$l)))==0),]
  dat$pq = paste(dat$p,dat$q)
  dat$qp = paste(dat$q,dat$p)
  
  # get the perimeter
  peri = perimeter(region, poly, x, y)
  
  # find the neighbor first
  if (is.null(nb)) nb=nbrlist(region, x, y, corner=FALSE)
  nbnames = names(nb)
  
  # neighbor matrix
  res = matrix(0,nrow=k,ncol=k)
  rownames(res) = colnames(res) = name
  for (i in 1:k) {
    res[nbnames[i],nb[[i]]] = 1
  }
  res[lower.tri(res)] = 0
  
  # fill in the matrix
  for (i in 1:k){
    idxrow = which(res[i,]==1)
    if (length(idxrow)==0) next
    for (j in idxrow){
      tmpdat = dat[dat$r %in% name[c(i,j)],]
      tmpdat = tmpdat[duplicated(tmpdat$p) | duplicated(tmpdat$p,fromLast=TRUE),]
      tmpres = overlapC(tmpdat$pq,tmpdat$qp)
      tmpres = tmpres[rowSums(tmpres)>0,,drop=FALSE]
      resrow = data.frame(r1=tmpdat[tmpres[,1],'r'], r2=tmpdat[tmpres[,2],'r'],
                          tmpdat[tmpres[,1],c('x','y','qx','qy')],
                          stringsAsFactors=FALSE)
      resrow$dist = sqrt((resrow$x-resrow$qx)^2+(resrow$y-resrow$qy)^2)
      res[i,j] = sum(resrow$dist[resrow$r1 != resrow$r2])
    }
  }
  
  res = res + t(res)
  diag(res) = peri
  return(res)
}

library(Rcpp)
cppFunction('
  NumericMatrix overlapC(CharacterVector x, CharacterVector y) {
    int nrow = x.length();
    NumericMatrix out(nrow, 2);

    for (int i = 0; i < nrow; i++) {
      for (int j = i+1; j < nrow; j++) {
        if (x[i] == x[j] || x[i] == y[j]) {
          out(i, 0) = i+1;
          out(i, 1) = j+1;
        }
      }
    }
    return out;
  }
')
