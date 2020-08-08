#evcmat() estimate the variance-covariance matrix of ATS value estimators

evcmat<-function(data,method="Gest",family="gaussian",common=TRUE){
       D<-as.data.frame(data); Ma<-method; FA<-family; CO<-common
       Nstage<-nstage(data=D)
       if (is.null(D$O1)) {Base<-0} else {Base<-1}
       Dmat<-as.matrix(atsscan(data=D))
       G<-dim(Dmat)[1]                       #number of strategies
       L<-dim(Dmat)[2]
       Dmat<-Dmat[,c(-1,-L)]
       Dmat[which(is.na(Dmat))]<-0
       evmat<-matrix(rep(NA,G*G),ncol=G)
       if (Nstage==1 && Base==0) {
         for (g1 in seq(G)){
          for (g2 in seq(G)){
            evmat[g1,g2]<-ecov(data=D,ats1=Dmat[g1],
                              ats2=Dmat[g2],family=FA,common=CO)
            }}}
       else {
         for (g1 in seq(G)){
           for (g2 in seq(G)){
                evmat[g1,g2]<-ecov(data=D,
                                  ats1=Dmat[g1,],ats2=Dmat[g2,],
                                  family=FA,common=CO)
                }}}
       return(evmat)
}

