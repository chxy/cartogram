##' Produce a Pseudo-Dorling Cartogram.
##' 
##' @param name A vector of region names.
##' @param centroidx A vector of x-coordinates of the regions.
##' @param centroidy A vector of y-coordinates of the regions.
##' @param density A vector of the variable of interest. It will be used as the radii of the circles.
##' @param nbr A list of the neighbors of every region. Each element is a vector of all the neighbor names of a region. If nbr=NULL, then it is assumed that no region has any neighbors. If nbr is not NULL, then names should be given to all the elements of the list, for matching the neighbors with the host region name, otherwise the parameter "name" (a character vector) will be used as the element names of nbr. Besides, any values in nbr that are not in "name" will be removed. The length of nbr could be different from the length of "name", but any element in nbr whose name is not in "name" will be removed too.
##' @param shared.border A matrix of the counts of shared borders, typically generated from the function border_summary(). It is used to scale the attract force.
##' @param tolerance Tolerant value for the sum of overlapped radii.
##' @param dist.ratio The threshold to determine whether an attract force is added. It is applied to the ratio of the distance between two centroids, and the sum of those two radii.
##' @param iteration The limit of the number of iterations. Default to be 9999.
##' @param polygon.vertex The number of vertice of the circle. Default to be 100. If polygon.vertex=4 then diamands applies. If polygon.vertex=6, then hexagon applies.
##' @param animation Whether to show the movements of centroids.
##' @param sleep.time Only works when animation=TRUE.
##' @example inst/example.R
##' @export
##' 
dorling = function(name, centroidx, centroidy, density, nbr=NULL, shared.border=NULL, tolerance=0.1, dist.ratio=1.2, iteration=9999, polygon.vertex=100, animation=TRUE, sleep.time=0.3, ...){
    n=length(name)
    stopifnot(n==length(centroidx), n==length(centroidy), n==length(density), is.numeric(iteration))
    
    # identify all the names
    name=as.character(name)
    if (is.null(names(nbr)) && sum(duplindex <- duplicated(name))) {
        name[duplindex]=paste(name[duplindex],name[duplindex],1:sum(duplindex),sep="_")
    }
    
    # clean "nbr"
    if (!is.null(nbr)) {        
        if (is.null(names(nbr))) {
            stopifnot(n==length(nbr))
            names(nbr)=name
        } else {
            nbr=nbr[names(nbr) %in% name]
        }
        if (any(!unlist(nbr) %in% name)) {
            for (i in 1:n){
                nbr[[i]]=nbr[[i]][nbr[[i]] %in% name]
            }
        }
    }

    # Set up the data
    dat=data.frame(name=name,x=centroidx,y=centroidy,density=density,stringsAsFactors=FALSE)
    rownames(dat)=dat$name
    
    # original distance
    origindist=as.matrix(dist(dat[,2:3]))
    
    # rescale the density (the radius)
    dat$density=dat$density/max(dat$density)*mean(origindist)/5
    
    # the closest distance for paired centroids
    circleDist=outer(dat$density,dat$density,"+")
    diag(circleDist)=0
    colnames(circleDist)=rownames(circleDist)=name
    
    # set up initial values
    # crtloc is the current centroid locations
    # frc is the force, including repel force and attract force
    crtloc=frc=dat[,2:3]
    crtDist=origindist
    s=0
    err=circleDist-crtDist
    
    while (sum(sapply(err,max,0))>tolerance) {
        s = s + 1
        if (!is.null(iteration) && s>iteration) {
            warning("Reach the largest iteration limit.")
            break
        }
        if (s%%10==0) cat("Iteration: ",s,"\n")
        if (animation) {
            plot(y~x,crtloc,type='p',col=2,pch=20)
            circle(crtloc$x,crtloc$y,dat$density,vertex=polygon.vertex,...)
            text(crtloc$x,crtloc$y,dat$name,cex=0.8)
            Sys.sleep(sleep.time)
        }
               
        frc$xforce=frc$yforce=frc$xattract=frc$yattract=frc$xrepel=frc$yrepel=0.00000
        
        # Calculate the repel force
        idx = circleDist > crtDist
        idx = idx & lower.tri(idx)
        err = circleDist-crtDist
        for (i in which(rowSums(idx)>0)){
            for (j in which(idx[i,1:(i-1)])){
                ratio=(circleDist[i,j]-crtDist[i,j])/2/crtDist[i,j]
                frc$xrepel[i]=frc$xrepel[i]+ratio*(crtloc$x[i]-crtloc$x[j])
                frc$xrepel[j]=frc$xrepel[j]+ratio*(crtloc$x[j]-crtloc$x[i])
                frc$yrepel[i]=frc$yrepel[i]+ratio*(crtloc$y[i]-crtloc$y[j])
                frc$yrepel[j]=frc$yrepel[j]+ratio*(crtloc$y[j]-crtloc$y[i])
            }
        }
        
        # Calculate the attract force
        for (i in 1:length(nbr)){
            for (j in 1:length(nbr[[i]])){
                crtstate=names(nbr)[i]
                crtnbr=which(rownames(crtDist)==nbr[[i]][j])
                distratio=crtDist[crtstate,crtnbr]/circleDist[crtstate,crtnbr]
                if (distratio > dist.ratio & crtDist[crtstate,crtnbr]>origindist[crtstate,crtnbr]){
                    border_ratio=ifelse(is.null(shared.border),1/(round(s/10)+8),shared.border[crtstate,nbr[[i]][j]]/shared.border[crtstate,crtstate])
                    ratio=(crtDist[crtstate,crtnbr]-circleDist[crtstate,crtnbr])/crtDist[crtstate,crtnbr]*border_ratio
                    frc$xattract[i]=frc$xattract[i]-ratio*(crtloc$x[i]-crtloc$x[crtnbr])
                    frc$xattract[crtnbr]=frc$xattract[crtnbr]-ratio*(crtloc$x[crtnbr]-crtloc$x[i])
                    frc$yattract[i]=frc$yattract[i]-ratio*(crtloc$y[i]-crtloc$y[crtnbr])
                    frc$yattract[crtnbr]=frc$yattract[crtnbr]-ratio*(crtloc$y[crtnbr]-crtloc$y[i])
                }
            }
        }
        
        # Find the final force
        frc$xforce=frc$xrepel+frc$xattract
        frc$yforce=frc$yrepel+frc$yattract
        
        # Reduce the force if it changes the relative direction of the neighbors
        closest=data.frame(cbind(rownames(crtDist),rownames(crtDist)[nnbr(crtDist,k=1)]))
        closest$xdist=apply(closest,1,function(xv){abs(crtloc[xv[1],1]-crtloc[xv[2],1])})
        closest$ydist=apply(closest,1,function(xv){abs(crtloc[xv[1],2]-crtloc[xv[2],2])})
        closest$dist=sqrt(closest$xdist^2+closest$ydist^2)
        closest$xforce=abs(frc$xforce)
        closest$yforce=abs(frc$yforce)
        closest$force=sqrt(closest$xforce^2+closest$yforce^2)
        closest$xratio=closest$xdist/closest$xforce
        closest$yratio=closest$ydist/closest$yforce
        closest$ratio=closest$dist/closest$force
        closest$xratio[closest$xratio<tolerance]=1
        closest$yratio[closest$yratio<tolerance]=1
        closest$ratio[closest$ratio<tolerance]=1
        closest$r=pmin(closest$xratio,closest$yratio,closest$ratio,na.rm=TRUE)
        closest$r[closest$r>1]=1
        frc$xforce=frc$xforce*closest$r
        frc$yforce=frc$yforce*closest$r
        crtloc=crtloc+frc[,8:7]
        crtDist=as.matrix(dist(crtloc))
    }
    
    plot(y~x,crtloc,type='p',col=2,pch=20)
    circle(crtloc$x,crtloc$y,dat$density,vertex=polygon.vertex,...)
    text(crtloc$x,crtloc$y,dat$name,cex=0.8)
}
