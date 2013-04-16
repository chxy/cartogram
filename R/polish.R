##' Produce a lattice cartogram by polishing the grids in rows and in columns
##' 
##' @param grids The output of function \code{checkerboard}.
##' @param density A vector of the variable of interest. 
##' \code{names(density)} should match \code{grids$label}.
##' @param pal palette. The input for the argument "col" in \code{image()}.
##' @return a data frame of coordinates and the corresponding labels, as well as the errors.
##' @example inst/ex_gridmap.R
##' @export
##'
polish_cart = function(grids,density,pal=NULL){
    all_labels=if (is.factor(grids$label)) levels(grids$label) else unique(grids$label)
    grids$label=as.character(grids$label)
    stopifnot(length(intersect(names(density),unique(grids$label)))>1)
    grids=grids[grids$label %in% names(density) | is.na(grids$label),]
    
    xgrid=sort(unique(grids$x))
    ygrid=sort(unique(grids$y))
    ncell=length(xgrid)*length(ygrid)
    if (ncell!=nrow(grids)) stop("The input is not a full grid.")
    
    #plot(y~x,data=grids,pch=15,col=factor(grids$label,levels=all_labels),cex=1.7)
    if (is.null(pal)) {
        set.seed(1000)
        pal=palette(sample(c(rainbow(24),colors()[c(1,4:11,13:26,28:29,76:87)*5+3]),length(all_labels),rep=FALSE))
    }
    x11(width=4,height=4)
    grids=grids[order(grids$y,grids$x),]
    image(xgrid,ygrid,matrix(as.integer(factor(grids$label,levels=all_labels)),nrow=length(xgrid),ncol=length(ygrid)),col=pal,xlab='',ylab='',xaxt='n',yaxt='n',frame=F)
    
    dens=data.frame(density=density,orig_area=table(grids$label)[names(density)])
    rownames(dens)=names(density)
    dens=dens[!is.na(dens$orig_area),]
    dens$goal=round(dens$density*mean(dens$orig_area)/mean(dens$density))
    
    xcenter=round(mean(1:length(xgrid)))
    ycenter=round(mean(1:length(ygrid)))
    
    newgrids1=data.frame(x=NULL,y=NULL,label=NULL)
    for (i in 1:length(xgrid)){
        column=grids[grids$x==xgrid[i],]
        newgrids1=rbind(newgrids1,panning(column,dens,'x',ycenter))
    }
    newgrids1=global.matching(newgrids1,'x')
    image1=complete.image(newgrids1)
    newgrids1=image1$df
    dens$mid_area=table(newgrids1$label)[rownames(dens)]
    
    #plot(y~x,data=newgrids1,pch=15,col=factor(newgrids1$label,levels=all_labels),cex=1.7)
    x11(width=4,height=4)
    image(image1$matrix,col=pal,xlab='',ylab='',xaxt='n',yaxt='n',frame=F)
    Sys.sleep(0.5)
    
    newgrids2=data.frame(x=NULL,y=NULL,label=NULL)
    for (i in unique(newgrids1$y)){
        rowline=newgrids1[newgrids1$y==i,]
        newgrids2=rbind(newgrids2,panning(rowline,dens,'y',xcenter))
    }
    newgrids2=global.matching(newgrids2,'y')
    image2=complete.image(newgrids2)
    newgrids2=image2$df
    newncell=sum(!is.na(newgrids2$label))
    
    dens$end_area=table(newgrids2$label)[rownames(dens)]
    err=c(SSE=sum((dens$goal-dens$end_area)^2)/newncell,
          AE=sum(abs(dens$goal-dens$end_area))/newncell)
    #plot(y~x,data=newgrids2,pch=15,col=factor(newgrids2$label,levels=all_labels),cex=1.7)
    x11(width=4,height=4)
    image(image2$matrix,col=pal,xlab='',ylab='',xaxt='n',yaxt='n',frame=F)
    return(list(grids=newgrids2,count=dens[,c(1,3,2,4,5)],error=err))
}


##' Complete the lattice if there are missings.
##' 
##' If nrow(df)!=length(unique(x))*length(unique(y)), 
##' then NA's will be filled in to make a full lattice.
##' 
##' @param df a data frame with column x(the x coordinate), y(y coordinate), 
##' and label(which region it belongs to). The data point should be in a line 
##' in either x or y direction.
##' @param omit.sea logical. If TRUE then the NA rows or columns around the map
##' will be removed. 
##' @return a data frame of column x(the new x coordinate), 
##' y(the new y coordinate), and label.
##' 
complete.image=function(df,omit.sea=TRUE){
    names(df)[which(!names(df)%in%c('x','y'))]='label'
    x=sort(unique(df$x))
    y=sort(unique(df$y))
    all_labels=if (is.factor(df$label)) levels(df$label) else sort(unique(df$label))
    df$label=as.integer(factor(df$label,levels=all_labels))
    res=matrix(NA,nrow=length(y),ncol=length(x))
    rownames(res)=y; colnames(res)=x
    for (j in 1:ncol(res)){
        tmp=df[df$x==x[j],]
        res[as.character(tmp$y),j]=tmp$label
    }
    if (omit.sea) {
        NArows=which(rowMeans(is.na(res))!=1)
        NAcols=which(colMeans(is.na(res))!=1)
        res=res[NArows[1]:NArows[length(NArows)],NAcols[1]:NAcols[length(NAcols)]]
        x=as.numeric(colnames(res))
        y=as.numeric(rownames(res))
    }
    list(x=x,y=y,matrix=t(res),df=data.frame(x=rep(x,each=length(y)),y=rep(y,length(x)),label=all_labels[as.vector(res)],stringsAsFactors=TRUE))
}


##' Stretch or squeeze a sequence of cells by a target density
##' 
##' @param df a data frame with column x(the x coordinate), y(y coordinate), 
##' and label(which region it belongs to). The data point should be in a line 
##' in either x or y direction.
##' @param dnsty a data frame with columns goal(the target number of cells) and 
##' orig_area(the original number of cells). The rownames must contain 
##' all the labels in \code{df}.
##' @param by If by='x' then the code will stretch the cells in y/vertical direction.
##' If by='y' then the code will stretch the cells in x/horizontal direction.
##' @param center the global center of y if by='x' and global center of x if by='y'. 
##' "Global" center means not the center of the input \code{df}, 
##' but the center of the map.
##' @return a data frame of column x(the new x coordinate), 
##' y(the new y coordinate), and label. The number of rows of the output 
##' might be different from the input \code{df}.
##' 
panning=function(df,dnsty,by,center){
    df$label=as.character(df$label)
    df$label[is.na(df$label)]='NA'
    jump=c(1,which(diff(as.integer(as.factor(df$label)))!=0)+1)
    unilabel=df$label[jump]
    labellen=c(diff(jump),nrow(df)+1-max(jump))
    strength=sqrt(dnsty[unilabel,'goal'] / dnsty[unilabel,'orig_area'])
    ratiolen=labellen / dnsty[unilabel,'orig_area']
    ratiolen[is.na(ratiolen)]=.3
    naidx=which(is.na(strength))
    for (i in naidx){
        strength[i]=min(c(1,strength[i-1],strength[i+1]),na.rm=TRUE)
    }
    newlength=labellen*strength
    newlength[ratiolen>=.3]=floor(newlength[ratiolen>=.3])
    newlength[ratiolen<.3]=ceiling(newlength[ratiolen<.3])
    newcolumn=rep(unilabel,newlength)
    newcolumn[newcolumn=='NA']=NA
    
    centeridx=sum(jump<=center)
    newcenter=round((center-jump[centeridx])*strength[centeridx])+ifelse(centeridx==1,0,sum(newlength[1:(centeridx-1)]))
    
    if (by=='x') return(data.frame(x=df$x[1],y=(1:length(newcolumn))-newcenter,label=newcolumn,stringsAsFactors=FALSE))
    if (by=='y') return(data.frame(x=(1:length(newcolumn))-newcenter,y=df$y[1],label=newcolumn,stringsAsFactors=FALSE))
}


##' Measure the similarity of two sequences of cells
##' 
##' 1. Contigency table or mean(a!=b)
##' 2. diff lengths of the sequences -- ignore or place a little panelty?
##' 3. neighors -- 1 neighbor or 3 neighbors? Assign some weights to the diagonal neighbors?
##' 
##' @param a a  vector
##' @param b a numeric vector of the same length as "a"
##' @param na.panelty non-negative. Apply to answered Question 2.
##' @param diag.wt non-negative. Apply to answer Question 3.
##' @return the score of similarity
##' @examples
##' similarity(letters[1:5],letters[1:5])
##' similarity(1:5,5:1)
##' similarity(1:5,2:6,diag.wt=0.5)
##' similarity(1:5,c(NA,2:5),na.panelty=0.2)
##'
similarity=function(a,b,na.panelty=0,diag.wt=0){
    n=length(a)
    stopifnot(n==length(b),na.panelty>=0,diag.wt>=0)
    r1=sum(a==b,na.rm=TRUE)
    r1top=sum(a[-1]==b[1:(n-1)],na.rm=TRUE)
    r1bot=sum(a[1:(n-1)]==b[-1],na.rm=TRUE)
    r2=sum(is.na(a)|is.na(b))
    #r3=n-r1-r2
    (r1+(r1top+r1bot)*diag.wt-r2*na.panelty)/n
}


##' Find the optimal matching position for two sequences
##' 
##' 1. shift one sequence by one cell at a time, from 
##' matching the bottom of the left sequence with the top of the right one, to
##' matching the top of the left sequence with the bottom of the right one.
##' 2. completely unmatched case, like a division line? Then we match 
##' the center of the left sequence with the center of the right one.
##' 3. ties of the measurement? Add weights to the diagonal neighbors and
##' add panelty to the NA's, then measure the similarity again. If ties still
##' exist, then use the closest tie to the center of the two sequences.
##' 4. To match two sequences - "1,1,NA,NA,2,3,3" and "1,1,2,2,3,3,3", 
##' the standard result 
##' 1 1 NA NA 2 3 3
##' 1 1  2  2 3 3 3
##' is worse than that with a loose rule -
##' 1 1 NA 2 3 3 NA
##' 1 1  2 2 3 3  3 
##' 
##' @param a a numeric vector
##' @param b a numeric vector
##' @param a1loc the global location of the first element of "a". 
##' @param na.loose logical. Whether to allow the number of NA's within a sequence changes. 
##' @return the matched vectors and their position
##' @examples
##' local.matching(1:5,1:5,3)
##' local.matching(1:5,3:7,0)
##' local.matching(rep(1,7),rep(1,3),-3)
##' local.matching(rep(1,7),2:5,1)
##' 
local.matching=function(a,b,a1loc,na.loose=TRUE){
    n1=length(a)
    n2=length(b)
    s=rep(NA,n1+n2-1)
    for (i in 1:length(s)){
        a1=c(rep(NA,max(0,i-n1)),a,rep(NA,max(n2-i,0)))
        b1=c(rep(NA,max(0,n1-i)),b,rep(NA,max(i-n2,0)))
        s[i]=similarity(a1,b1,na.panelty=0,diag.wt=0)
    }
    opti=which(s==max(s))
    if (length(opti)>1){
        optis=rep(NA,length(opti))
        for (i in 1:length(optis)){
            a1=c(rep(NA,max(0,opti[i]-n1)),a,rep(NA,max(n2-opti[i],0)))
            b1=c(rep(NA,max(0,n1-opti[i])),b,rep(NA,max(opti[i]-n2,0)))
            optis[i]=similarity(a1,b1,na.panelty=1,diag.wt=1)
        }
        opti=opti[optis==max(optis)]
        opti=opti[which.min(abs(opti-(n1+n2)/2))]
    }
    res.df=data.frame(a=c(rep(NA,max(0,opti-n1)),a,rep(NA,max(n2-opti,0))),
                      b=c(rep(NA,max(0,n1-opti)),b,rep(NA,max(opti-n2,0))),
                      loc=(a1loc-max(0,opti-n1)):(a1loc-1+n1+max(n2-opti,0)),
                      stringsAsFactors=FALSE)
    res.bloc=res.df$loc[(1:n2)+max(0,n1-opti)]
    if (na.loose) {
        imp=nrow(res.df)*10
        
        res.df$a[is.na(res.df$a)]=imp
        arle=rle(res.df$a)
        arle$values[arle$values==imp]=NA
        
        res.df$b[is.na(res.df$b)]=imp
        brle=rle(res.df$b)
        brle$values[brle$values==imp]=NA
        uniblen=length(brle$values)
        bidx=which(is.na(brle$values[-c(1,uniblen)]))
        if (bidxlen <- length(bidx) > 0) {
            for (i in 1:bidxlen) {
                bidxleft=sum(brle$lengths[1:(bidx[i]-1)])
                bidxright=sum(brle$lengths[1:bidx[i]])+1
                bleft=brle$values[bidx[i]-1]
                bright=brle$values[bidx[i]+1]
                
                aleft=which(arle$values==bleft)
                aright=which(arle$values==bright)
                if (length(aleft)>0 & length(aright)>0) {
                    aleft=aleft[which.min(abs(sapply(aleft,function(x){sum(arle$lengths[1:x])-bidxleft})))]
                    aright=aright[which.min(abs(sapply(aright,function(x){sum(arle$lengths[1:(x-1)])+1-bidxright})))]
                    aNA=sum(arle$lengths[(aleft+1):(aright-1)])
                    brle$lengths[bidx[i]]=aNA
                }
            }
            if (is.na(brle$values[1])){
                brle$values=brle$values[-1]
                brle$lengths=brle$lengths[-1]
            }
            if (is.na(brle$values[uniblen])){
                brle$values=brle$values[-uniblen]
                brle$lengths=brle$lengths[-uniblen]
            }
            b2=inverse.rle(brle)
            in.res=local.matching(a,b2,a1loc,na.loose=FALSE)
            res.df=in.res$df
            res.bloc=in.res$bloc
        }
    }
    return(list(df=res.df,bloc=res.bloc))
}


##' Find the optimal matching position for the map
##' 
##' @param df a data frame with x,y,and label
##' @param by If by='x' then the function will match the y/column sequences
##' in x/horizontal direction. If by='y' then will match the x/row sequences
##' in y/vertical direction.
##' @return a data frame of the column x(the new x coordinate), 
##' y(the new y coordinate), and label. Column label should be the same as "df".
##' 
global.matching=function(df,by){
    stopifnot(by %in% c('x','y'), c('x','y') %in% names(df))
    names(df)[which(!names(df)%in%c('x','y'))]='label'
    df=df[order(df$x,df$y),]
    x=sort(unique(df$x))
    y=sort(unique(df$y))
    res=df
    if (by=='x'){  # then column 'x' and 'label' will not change 
        for (i in 2:length(x)){
            x1=df[df$x==x[i-1],'label']
            x2=df[df$x==x[i],'label']
            x2pos=local.matching(x1,x2,res$y[res$x==x[i-1]][1],FALSE)$bloc
            res$y[res$x==x[i]]=x2pos
        }
    } else {       # then column 'y' and 'label' will not change 
        for (j in 2:length(y)){
            y1=df[df$y==y[j-1],'label']
            y2=df[df$y==y[j],'label']
            y2pos=local.matching(y1,y2,res$x[res$y==y[j-1]][1],FALSE)$bloc
            res$x[res$y==y[j]]=y2pos
        }
    }
    res
}
