#weight() compute the inversed probability weight (IPW)

weight<-function(data){
D<-as.data.frame(data)
if (is.null(D$O1)) {Base<-0} else {Base<-1}
Nstage<-nstage(data=D)
N<-nrow(D)

if (Nstage==1 && Base==0) {
 P1<-IPW<-rep(NA,N)
 D<-data.frame(D,P1,IPW)
 Ivec<-sort(unique(D$A1))
 for (i in Ivec){
  nn<-length(D$Y[D$A1==i])
  nd<-N
  D$P1[which(D$A1==i)]<-nn/nd
  D$IPW<-1/D$P1
  }} else
if (Nstage==1 && Base==1) {
 P1<-IPW<-rep(NA,N)
 D<-data.frame(D,P1,IPW)
 Hvec<-sort(unique(D$A1))
 for (h in Hvec){
  Ivec<-sort(unique(D$A1[which(D$O1==h)]))
    for (i in Ivec){
     nn<-length(D$Y[which(D$O1==h & D$A1==i)])
     nd<-length(D$Y[which(D$O1==h)])
     D$P1[which(D$O1==h & D$A1==i)]<-nn/nd
     D$IPW<-1/D$P1
     }}} else
if (Nstage==2 && Base==0) {
 P1<-P2<-IPW<-rep(NA,N)
 D<-data.frame(D,P1,P2,IPW)
 Ivec<-sort(unique(D$A1))
 for (i in Ivec){
  nn1<-length(D$Y[D$A1==i])
  nd1<-N
  D$P1[which(D$A1==i)]<-nn1/nd1
  Jvec<-sort(unique(D$O2[which(D$A1==i)]))
   for (j in Jvec){
    Kvec<-sort(unique(D$A2[which(D$A1==i & D$O2==j)]))
     for (k in Kvec){
      nn2<-length(D$Y[D$A1==i & D$O2==j & D$A2==k])
      nd2<-length(D$Y[D$A1==i & D$O2==j])
      D$P2[which(D$A1==i & D$O2==j & D$A2==k)]<-nn2/nd2
      D$IPW<-1/(D$P1*D$P2)
      }}}} else
if (Nstage==2 && Base==1) {
 P1<-P2<-IPW<-rep(NA,N)
 D<-data.frame(D,P1,P2,IPW)
 Hvec<-sort(unique(D$O1))
 for (h in Hvec){
  Ivec<-sort(unique(D$A1[which(D$O1==h)]))
    for (i in Ivec){
      nn1<-length(D$Y[D$O1==h & D$A1==i])
      nd1<-length(D$Y[D$O1==h])
      D$P1[which(D$O1==h & D$A1==i)]<-nn1/nd1
      Jvec<-sort(unique(D$O2[which(D$O1==h & D$A1==i)]))
        for (j in Jvec){
         Kvec<-sort(unique(D$A2[which(D$O1==h & D$A1==i & D$O2==j)]))
         for (k in Kvec){
          nn2<-length(D$Y[D$O1==h & D$A1==i & D$O2==j & D$A2==k])
          nd2<-length(D$Y[D$O1==h & D$A1==i & D$O2==j])
          D$P2[which(D$O1==h & D$A1==i & D$O2==j & D$A2==k)]<-nn2/nd2
          D$IPW<-1/(D$P1*D$P2)
          }}}}} else
if (Nstage==3 && Base==0) {
 P1<-P2<-P3<-IPW<-rep(NA,N)
 D<-data.frame(D,P1,P2,P3=IPW)
 Ivec<-sort(unique(D$A1))
 for (i in Ivec){
  nn1<-length(D$Y[D$A1==i])
  nd1<-N
  D$P1[which(D$A1==i)]<-nn1/nd1
  Jvec<-sort(unique(D$O2[which(D$A1==i)]))
  for (j in Jvec){
   Kvec<-sort(unique(D$A2[which(D$A1==i & D$O2==j)]))
   for (k in Kvec){
    nn2<-length(D$Y[D$A1==i & D$O2==j & D$A2==k])
    nd2<-length(D$Y[D$A1==i & D$O2==j])
    D$P2[which(D$A1==i & D$O2==j & D$A2==k)]<-nn2/nd2
    D$IPW<-1/(D$P1*D$P2)
    Lvec<-sort(unique(D$O3[which(D$A1==i & D$O2==j & D$A2==k)]))
     for (l in Lvec){
      Mvec<-sort(unique(D$A3[which(D$A1==i & D$O2==j & D$A2==k & D$O3==l)]))
      for (m in Mvec) {
        nn3<-length(D$Y[D$A1==i & D$O2==j & D$A2==k & D$O3==l & D$A3==m])
        nd3<-length(D$Y[D$A1==i & D$O2==j & D$A2==k & D$O3==l])
        D$P3[which(D$A1==i & D$O2==j & D$A2==k & D$O3==l & D$A3==m)]<-nn3/nd3
        D$IPW<-1/(D$P1*D$P2*D$P3)
        }}}}}} 

       return(D$IPW)
}

