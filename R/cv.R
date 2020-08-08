#' @importFrom stats var

#CV() estimate common within-sequence variance
cv<-function(data){
   D<-as.data.frame(data)
   if (is.null(D$O1)) {Base<-0} else {Base<-1}
   N<-nrow(D)
   Nstage<-nstage(data=D)
   Smat<-seqscan(data=D)[,-1]
   k<-nrow(Smat)

   group<-rep(NA,N)
   D<-data.frame(D,group)

   if (Nstage==1 && Base==0) {
     for (i in seq(k)) {D$group[which(D$A1==Smat[i,1])]<-i}}
   else
   if (Nstage==1 && Base==1) {
     for (i in seq(k)) {D$group[which(D$O1==Smat[i,1] &
                                        D$A1==Smat[i,2])]<-i}}
   else
   if (Nstage==2 && Base==0) {
     for (i in seq(k)) {
       D$group[which(D$A1==Smat[i,1] &
                                        D$O2==Smat[i,2] &
                                        D$A2==Smat[i,3])]<-i}}
   else
   if (Nstage==2 && Base==1) {
     for (i in seq(k)) {
       D$group[which(D$O1==Smat[i,1] &
                       D$A1==Smat[i,2] &
                       D$O2==Smat[i,3] & D$A2==Smat[i,4])]<-i}}
   else
   if (Nstage==3 && Base==0) {
     for (i in seq(k)) {
       D$group[which(D$A1==Smat[i,1] &
                       D$O2==Smat[i,2] &
                       D$A2==Smat[i,3] & D$O3==Smat[i,4] &
                       D$A3==Smat[i,5])]<-i}}
   sumvar<-0
   for (i in seq(k)){
        vi<-var(D$Y[which(D$group==i)])
        ni<-nrow(D[which(D$group==i),])
        sumvar<-sumvar+vi*(ni-1)
        }
   comv<-sumvar/(N-k)

   return(comv)
}


