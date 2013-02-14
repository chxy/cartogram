library(shiny)

shinyServer(function(input, output) {

    output$origPlot <- reactivePlot(function() {

        plot(y~x,state,type='n',xlab="",ylab="",frame=FALSE,xaxt='n',yaxt='n')
        map("state",add=TRUE)
        points(Latitude~Longitude,dat,type='p',col=2,pch=20)
        circle(dat$Longitude,dat$Latitude,dat$density,col='pink')
        text(dat$Longitude,dat$Latitude,dat$Abbr,cex=0.8)
    })
    
    output$distPlot <- reactivePlot(function() {
        
        for (s in 1:input$iteration) {
            frc$xforce=frc$yforce=frc$xattract=frc$yattract=frc$xrepel=frc$yrepel=0.00000
            idx= circleDist > crtDist
            idx= idx & lower.tri(idx)
            err= circleDist-crtDist
            for (i in which(rowSums(idx)>0)){
                for (j in which(idx[i,1:(i-1)])){
                    ratio=(circleDist[i,j]-crtDist[i,j])/2/crtDist[i,j]
                    frc$xrepel[i]=frc$xrepel[i]+ratio*(crtloc$Longitude[i]-crtloc$Longitude[j])
                    frc$xrepel[j]=frc$xrepel[j]+ratio*(crtloc$Longitude[j]-crtloc$Longitude[i])
                    frc$yrepel[i]=frc$yrepel[i]+ratio*(crtloc$Latitude[i]-crtloc$Latitude[j])
                    frc$yrepel[j]=frc$yrepel[j]+ratio*(crtloc$Latitude[j]-crtloc$Latitude[i])
                }
            }
            for (i in 1:length(nbrs)){
                for (j in 1:length(nbrs[[i]])){
                    crtstate=names(nbrs)[i]
                    crtnbr=which(rownames(crtDist)==nbrs[[i]][j])
                    distratio=crtDist[crtstate,crtnbr]/circleDist[crtstate,crtnbr]
                    if (distratio > input$distratio){
                        ratio=(crtDist[crtstate,crtnbr]-circleDist[crtstate,crtnbr])/8/crtDist[crtstate,crtnbr]
                        frc$xattract[i]=frc$xattract[i]-ratio*(crtloc$Longitude[i]-crtloc$Longitude[crtnbr])
                        frc$xattract[crtnbr]=frc$xattract[crtnbr]-ratio*(crtloc$Longitude[crtnbr]-crtloc$Longitude[i])
                        frc$yattract[i]=frc$yattract[i]-ratio*(crtloc$Latitude[i]-crtloc$Latitude[crtnbr])
                        frc$yattract[crtnbr]=frc$yattract[crtnbr]-ratio*(crtloc$Latitude[crtnbr]-crtloc$Latitude[i])
                    }
                }
            }
            frc$xforce=frc$xrepel+frc$xattract
            frc$yforce=frc$yrepel+frc$yattract
            closest=data.frame(cbind(rownames(crtDist),rownames(crtDist)[nnbr(crtDist,k=1)]))
            closest$dist=apply(closest,1,function(xv){crtDist[xv[1],xv[2]]})
            closest$force=sqrt(frc$xforce^2+frc$yforce^2)
            closest$idx=closest$force>closest$dist
            frc$xforce[closest$idx]=frc$xforce[closest$idx]*closest$dist[closest$idx]/closest$force[closest$idx]
            frc$yforce[closest$idx]=frc$yforce[closest$idx]*closest$dist[closest$idx]/closest$force[closest$idx]
            
            crtloc=crtloc+frc[,7:8]
            crtDist=as.matrix(dist(crtloc))
        }
        
        plot(Latitude~Longitude,crtloc,type='p',col=2,pch=20,main='Dorling Cartogram')
        circle(crtloc$Longitude,crtloc$Latitude,dat$density,col='pink')
        text(crtloc$Longitude,crtloc$Latitude,dat$Abbr,cex=0.8)
        
    })
})
