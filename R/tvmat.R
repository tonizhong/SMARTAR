#getvmat() to get var-cov matrix for effect size
#input information matrix and method

tvmat<-function(sim,family="gaussian",method="Gest"){
Smat<-sim
FA<-family
MA<-method
if (is.null(Smat$O1)) {Base<-0} else {Base<-1}
  Nstage<-nstage(data=Smat)
  Dmat<-atsscan(data=Smat)
  G<-nrow(Dmat)
  vmat<-Imat<-matrix(NA,ncol=G,nrow=G)
  if (Nstage==1 && Base==0) {
   for (j in seq(G)){
    for (k in seq(G)){
     DJ<-as.numeric(Dmat[j,2])
     DJ[which(is.na(DJ))]<-0
     DK<-as.numeric(Dmat[k,2])
     DK[which(is.na(DK))]<-0
     vmat[j,k]<-tcov(ats1=DJ,ats2=DK,sim=Smat,family=FA,method=MA)
     }}} else
  if (Nstage==1 && Base==1) {
    for (j in seq(G)){
      for (k in seq(G)){
        DJ<-as.numeric(Dmat[j,2:3])
        DJ[which(is.na(DJ))]<-0
        DK<-as.numeric(Dmat[k,2:3])
        DK[which(is.na(DK))]<-0
        vmat[j,k]<-tcov(ats1=DJ,ats2=DK,sim=Smat,family=FA,method=MA)
        }}} else
  if (Nstage==2 && Base==0) {
    for (j in seq(G)){
     for (k in seq(G)){DJ<-as.numeric(Dmat[j,2:4])
       DJ[which(is.na(DJ))]<-0
       DK<-as.numeric(Dmat[k,2:4])
       DK[which(is.na(DK))]<-0
       vmat[j,k]<-tcov(ats1=DJ,ats2=DK,sim=Smat,family=FA,method=MA)
       }}} else
  if (Nstage==2 && Base==1) {
    for (j in seq(G)){
      for (k in seq(G)){
        DJ<-as.numeric(Dmat[j,2:7])
        DJ[which(is.na(DJ))]<-0
        DK<-as.numeric(Dmat[k,2:7])
        DK[which(is.na(DK))]<-0
        vmat[j,k]<-tcov(ats1=DJ,ats2=DK,sim=Smat,family=FA,method=MA)
        }}} else
  if (Nstage==3 && Base==0) {
    for (j in seq(G)){
      for (k in seq(G)){DJ<-as.numeric(Dmat[j,2:8])
       DJ[which(is.na(DJ))]<-0
       DK<-as.numeric(Dmat[k,2:8])
       DK[which(is.na(DK))]<-0
       vmat[j,k]<-tcov(ats1=DJ,ats2=DK,sim=Smat,method=MA)
       }}} 

      return(vmat)
}
