#' @importFrom graphics plot axis points lines title legend par abline
#ddplot() outpupt the design diagram for SMART

ddplot<-function(data,pch,title,xlab,legend,xtext){
       D<-data.frame(data)
       if (is.null(D$O1)) {Base<-0} else {Base<-1}
       N<-nrow(D)
       olpar<-par(mar=c(3,3,3,3))
       on.exit(par(olpar))
       Smat<-seqscan(data=D)
       #SEQ=Smat$SEQ; A1=Smat$A1; A2=Smat$A2; A3=Smat$A3;
       # O1=Smat$O1; O2=Smat$O2; O3=Smat$O3
       ns<-nrow(Smat)
       Nstage<-nstage(data=D)

       if (Nstage==1 && Base==0){
         SEQ<-Smat$SEQ
         A1<-Smat$A1
         Y0<-rep(NA,ns)
         Y1<-SEQ
         Smat<-(data.frame(SEQ,A1,Y0,Y1))

         mi<-min(Smat$Y1)
         ma<-max(Smat$Y1)
         Smat$Y0<-mi+(ma-mi)/2
         #plot
         Smat<-as.matrix(Smat)
         max<-ns+1
         lab<-c("Registration","A1")
         if (is.null(xtext)) {lab<-lab}
         else {lab<-xtext}
         oldbar<-par(mar=c(4,4,4,2))
         on.exit(par(oldbar))
         plot(1,xlab=xlab,ylab="",
              xaxt="n",yaxt="n",xlim=c(-1.5,0.5),
              ylim=c(0,max),bty="n",type="n")
         axis(side=1,-1:0,padj=1,labels=lab)
         legend(x=-1,y=max,legend[2],pch=c(pch[1]),bty="n")
         for (i in 1:ns){
           points(x=0,y=max-Smat[i,4],pch=pch[1],cex=1.5)
           lines(x=c(0,-1),y=max-c(Smat[i,4],Smat[i,3]))
         }} else
       if (Nstage==1 && Base==1){
         SEQ<-Smat$SEQ; O1<-Smat$O1; A1<-Smat$A1
            Y0<-Y1<-rep(NA,ns); Y2<-SEQ
            Smat<-(data.frame(SEQ,O1,A1,Y0,Y1,Y2))
            Hvec<-sort(unique(Smat$O1))
            for (h in Hvec){
                 mi<-min(Smat$Y2[which(Smat$O1==h)])
                 ma<-max(Smat$Y2[which(Smat$O1==h)])
                 Smat$Y1[which(Smat$O1==h)]<-mi+(ma-mi)/2
                 }
            mi<-min(Smat$Y1)
            ma<-max(Smat$Y1)
            Smat$Y0<-mi+(ma-mi)/2
            #plot
            Smat<-as.matrix(Smat)
            max<-ns+1
            lab<-c("Registration","O1","A1")
            if (is.null(xtext)) {lab<-lab} 
            else {lab<-xtext}
            oldbar<-par(mar=c(4,4,4,2))
            on.exit(par(oldbar))
            plot(1,xlab=xlab,ylab="",
                 xaxt="n",yaxt="n",xlim=c(-2.5,0.5),
                 ylim=c(0,max),bty="n",type="n")
            axis(side=1,-2:0,
                 labels=lab)
            legend(x=-2,y=max,
                   legend,
                   pch=c(pch[2],pch[1]),bty="n")
            for (i in 1:ns){
              points(x=0,y=max-Smat[i,6],pch=pch[1],cex=1.5)
              lines(x=c( 0,-1),y=max-c(Smat[i,6],Smat[i,5]))
              points(x=-1,y=max-Smat[i,5],pch=pch[2],cex=1.5)
              lines(x=c(-1,-2),y=max-c(Smat[i,5],Smat[i,4]))
            }} else
       if (Nstage==2 && Base==0){
         SEQ<-Smat$SEQ; A1<-Smat$A1; A2<-Smat$A2; O2<-Smat$O2
         Y0<-Y1<-Y2<-rep(NA,ns); Y3<-SEQ
         Smat<-(data.frame(SEQ,A1,O2,A2,Y0,Y1,Y2,Y3))

          Ivec<-sort(unique(Smat$A1))
          for (i in Ivec){
              Jvec<-sort(unique(Smat$O2[which(Smat$A1==i)]))
              for (j in Jvec) {
                   mi<-min(Smat$Y3[which(Smat$A1==i & Smat$O2==j)])
                   ma<-max(Smat$Y3[which(Smat$A1==i & Smat$O2==j)])
                   Smat$Y2[which(Smat$A1==i & Smat$O2==j)]<-mi+(ma-mi)/2
                   }
              mi<-min(Smat$Y2[which(Smat$A1==i)])
              ma<-max(Smat$Y2[which(Smat$A1==i)])
              Smat$Y1[which(Smat$A1==i)]<-mi+(ma-mi)/2
              }
          mi<-min(Smat$Y1)
          ma<-max(Smat$Y1)
          Smat$Y0<-mi+(ma-mi)/2
          #plot
          Smat<-as.matrix(Smat)
          max<-ns+1
          lab<-c("Registration","A1","O2","A2")
          if (is.null(xtext)) {lab<-lab} 
          else {lab<-xtext}
          oldbar<-par(mar=c(4,4,4,2))
          on.exit(par(oldbar))
          plot(1,xlab=xlab,ylab="",xaxt="n",yaxt="n",
               xlim=c(-3.5,0.5),ylim=c(0,max),bty="n",type="n")
          axis(side=1,-3:0,padj=1,labels=lab)
          legend(x=-3,y=max,legend,pch=c(pch[2],pch[1]),bty="n")
          for (i in 1:ns){
               points(x=0,y=max-Smat[i,8],pch=pch[1],cex=1.5)
               lines(x=c( 0,-1),y=max-c(Smat[i,8],Smat[i,7]))
               points(x=-1,y=max-Smat[i,7],pch=pch[2],cex=1.5)
               lines(x=c(-1,-2),y=max-c(Smat[i,7],Smat[i,6]))
               points(x=-2,y=max-Smat[i,6],pch=pch[1],cex=1.5)
               lines(x=c(-2,-3),y=max-c(Smat[i,6],Smat[i,5]))
          }} else
       if (Nstage==2 && Base==1){
         SEQ<-Smat$SEQ; O1<-Smat$O1; A1<-Smat$A1; O2<-Smat$O2; A2<-Smat$A2
        Y0<-Y1<-Y2<-Y3<-rep(NA,ns); Y4<-SEQ
        Smat<-(data.frame(SEQ,O1,A1,O2,A2,Y0,Y1,Y2,Y3,Y4))
        Hvec<-sort(unique(Smat$O1))
        for (h in Hvec){
             Ivec<-sort(unique(Smat$A1[which(Smat$O1==h)]))
             for (i in Ivec){
                  Jvec<-sort(unique(Smat$O2[which(Smat$O1==h & Smat$A1==i)]))
                  for (j in Jvec) {
                    mi<-min(Smat$Y4[which(Smat$O1==h & Smat$A1==i &
                                           Smat$O2==j)])
                    ma<-max(Smat$Y4[which(Smat$O1==h & Smat$A1==i &
                                           Smat$O2==j)])
                    Smat$Y3[which(Smat$O1==h & Smat$A1==i & Smat$O2==j)]<-
                      mi+(ma-mi)/2
                    }
                  mi<-min(Smat$Y3[which(Smat$O1==h & Smat$A1==i)])
                  ma<-max(Smat$Y3[which(Smat$O1==h & Smat$A1==i)])
                  Smat$Y2[which(Smat$O1==h & Smat$A1==i)]<-mi+(ma-mi)/2
                  }
             mi<-min(Smat$Y2[which(Smat$O1==h)])
             ma<-max(Smat$Y2[which(Smat$O1==h)])
             Smat$Y1[which(Smat$O1==h)]<-mi+(ma-mi)/2
             }
        mi<-min(Smat$Y1)
        ma<-max(Smat$Y1)
        Smat$Y0<-mi+(ma-mi)/2
        #plot
        Smat<-as.matrix(Smat)
        max<-ns+1
        lab<-c("Registration","O1","A1","O2","A2")
        if (is.null(xtext)) {lab<-lab} 
        else {lab<-xtext}
        oldbar<-par(mar=c(4,4,4,2))
        on.exit(par(oldbar))
        plot(1,xlab=xlab,ylab="",xaxt="n",yaxt="n",
             xlim=c(-4.5,0.5),ylim=c(0,max),bty="n",type="n")
        axis(side=1,-4:0,labels=lab)
        legend(x=-4,y=max,legend,pch=c(pch[2],pch[1]),bty="n")
        for (i in 1:ns){
             points(x=0,y=max-Smat[i,10],pch=pch[1],cex=1.5)
             lines(x=c( 0,-1),y=max-c(Smat[i,10],Smat[i,9]))
             points(x=-1,y=max-Smat[i,9],pch=pch[2],cex=1.5)
             lines(x=c(-1,-2),y=max-c(Smat[i,9],Smat[i,8]))
             points(x=-2,y=max-Smat[i,8],pch=pch[1],cex=1.5)
             lines(x=c(-2,-3),y=max-c(Smat[i,8],Smat[i,7]))
             points(x=-3,y=max-Smat[i,7],pch=pch[2],cex=1.5)
             lines(x=c(-3,-4),y=max-c(Smat[i,7],Smat[i,6]))
             } } else
       if (Nstage==3 && Base==0){
         SEQ<-Smat$SEQ
         A1<-Smat$A1
         O2<-Smat$O2
         A2<-Smat$A2
         O3<-Smat$O3
         A3<-Smat$A3
         Y0<-Y1<-Y2<-Y3<-Y4<-rep(NA,ns); Y5<-SEQ
         Smat<-(data.frame(SEQ,A1,O2,A2,O3,A3,Y0,Y1,Y2,Y3,Y4,Y5))

      Ivec<-sort(unique(Smat$A1))
      for (i in Ivec){
        Jvec<-sort(unique(Smat$O2[which(Smat$A1==i)]))
        for (j in Jvec) {
          Kvec<-sort(unique(Smat$A2[which(Smat$A1==i &Smat$O2==j)]))
          for (k in Kvec) {
            Lvec<-sort(unique(Smat$O3[which(Smat$A1==i &
                                             Smat$O2==j &
                                             Smat$A2==k)]))
            for (l in Lvec) {
                 mi<-min(Smat$Y5[which(Smat$A1==i & Smat$O2==j &
                                        Smat$A2==k & Smat$O3==l)])
                 ma<-max(Smat$Y5[which(Smat$A1==i & Smat$O2==j &
                                        Smat$A2==k & Smat$O3==l)])
                 Smat$Y4[which(Smat$A1==i & Smat$O2==j & Smat$A2==k &
                                 Smat$O3==l)]<-mi+(ma-mi)/2
                 }
            mi<-min(Smat$Y4[which(Smat$A1==i & Smat$O2==j & Smat$A2==k)])
            ma<-max(Smat$Y4[which(Smat$A1==i & Smat$O2==j & Smat$A2==k)])
            Smat$Y3[which(Smat$A1==i & Smat$O2==j & Smat$A2==k)]<-
              mi+(ma-mi)/2
            }
          mi<-min(Smat$Y3[which(Smat$A1==i & Smat$O2==j)])
          ma<-max(Smat$Y3[which(Smat$A1==i & Smat$O2==j)])
          Smat$Y2[which(Smat$A1==i & Smat$O2==j)]<-mi+(ma-mi)/2
          }
    mi<-min(Smat$Y2[which(Smat$A1==i)])
    ma<-max(Smat$Y2[which(Smat$A1==i)])
    Smat$Y1[which(Smat$A1==i)]<-mi+(ma-mi)/2
    }
    mi<-min(Smat$Y1)
    ma<-max(Smat$Y1)
    Smat$Y0<-mi+(ma-mi)/2

    Smat<-as.matrix(Smat)
    #plot
    max<-ns+1
    lab<-c("Registration","A1","O2","A2","O3","A3")
    if (is.null(xtext)) {lab<-lab} 
    else {lab<-xtext}
    oldbar<-par(mar=c(4,4,4,2))
    on.exit(par(oldbar))
    plot(1,xlab=xlab,ylab="",xaxt="n",yaxt="n",
         xlim=c(-5.5,0.5),ylim=c(0,max),bty="n",type="n")
    axis(side=1,-5:0,padj=1,labels=lab)
    legend(x=-5,y=max,legend,pch=c(pch[2],pch[1]),bty="n")
    for (i in 1:ns){
      points(x=0,y=max-Smat[i,12],pch=pch[1])
      lines(x=c( 0,-1),y=max-c(Smat[i,12],Smat[i,11]))
      points(x=-1,y=max-Smat[i,11],pch=pch[2])
      lines(x=c(-1,-2),y=max-c(Smat[i,11],Smat[i,10]))
      points(x=-2,y=max-Smat[i,10],pch=pch[1])
      lines(x=c(-2,-3),y=max-c(Smat[i,10],Smat[i,9]))
      points(x=-3,y=max-Smat[i,9],pch=pch[2])
      lines(x=c(-3,-4),y=max-c(Smat[i,9],Smat[i,8]))
      points(x=-4,y=max-Smat[i,8],pch=pch[1])
      lines(x=c(-4,-5),y=max-c(Smat[i,8],Smat[i,7]))
    }} 

       ti <- "Design diagram of SMART"
       if (is.null(title)) {ti<-ti} 
       else {ti<-title}
    title(ti)
}



