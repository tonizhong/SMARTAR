#function atsscan() return all the strategies defined in a
#sequential randomized clinical trial data

atsscan<-function(data){
  D<-as.data.frame(data)
  if (is.null(D$O1)) {Base<-0} else {Base<-1}
  Nstage<-nstage(data=D)
  N<-nrow(D)
  Dmat<-NULL
  if (Nstage==1 && Base==0) {
    g<-1
    Ivec<-sort(unique(D$A1))
    for (i in Ivec){
         ng<-length(D$Y[which(D$A1==i)])
         Dmat<-rbind(Dmat,c(g,i,ng))
         colnames(Dmat)<-c("ATS","d0","N")
         g<-g+1}} else
  if (Nstage==1 && Base==1) {
    g<-1
    Ivec0<-sort(unique(D$A1[which(D$O1==0)]))
    Ivec1<-sort(unique(D$A1[which(D$O1==1)]))
    if (!length(Ivec0)) {Ivec0<--99}
    if (!length(Ivec1)) {Ivec1<--99}
    for (i0 in Ivec0){
    for (i1 in Ivec1){
         n0<-length(D$Y[which(D$O1==0 & D$A1==i0)])
         n1<-length(D$Y[which(D$O1==1 & D$A1==i1)])
         ng<-sum(n0,n1)
         Dmat<-rbind(Dmat,c(g,i0,i1,ng))
         colnames(Dmat)<-c("ATS","d0","d1","N")
         g<-g+1}}} else
  if (Nstage==2 && Base==0) {
    g<-1
    Ivec<-sort(unique(D$A1))
    for (i in Ivec){
         Jvec0<-sort(unique(D$A2[which(D$A1==i & D$O2==0)]))
         Jvec1<-sort(unique(D$A2[which(D$A1==i & D$O2==1)]))
         for (j0 in Jvec0) {
         for (j1 in Jvec1) {
              n0<-length(D$Y[which(D$A1==i & D$O2==0 & D$A2==j0)])
              n1<-length(D$Y[which(D$A1==i & D$O2==1 & D$A2==j1)])
              ng<-sum(n0,n1)
                    Dmat<-rbind(Dmat,c(g,i,j0,j1,ng))
                    colnames(Dmat)<-c("ATS","d0","d00","d10","N")
                    g<-g+1}}}} else
  if (Nstage==2 && Base==1) {
    g<-1
    Ivec0<-sort(unique(D$A1[which(D$O1==0)]))
    Ivec1<-sort(unique(D$A1[which(D$O1==1)]))
    for (i0 in Ivec0){
    for (i1 in Ivec1){
      Jvec00<-sort(unique(D$A2[which(D$O1==0 & D$A1==i0 &
                                      D$O2==0)]))
      Jvec01<-sort(unique(D$A2[which(D$O1==0 & D$A1==i0 &
                                      D$O2==1)]))
      Jvec10<-sort(unique(D$A2[which(D$O1==1 & D$A1==i1 &
                                      D$O2==0)]))
      Jvec11<-sort(unique(D$A2[which(D$O1==1 & D$A1==i1 &
                                      D$O2==1)]))
      for (j00 in Jvec00) {
      for (j01 in Jvec01) {
      for (j10 in Jvec10) {
      for (j11 in Jvec11) {
        n00<-length(D$Y[which(D$O1==0 & D$A1==i0 &
                               D$O2==0 & D$A2==j00)])
        n01<-length(D$Y[which(D$O1==0 & D$A1==i0 &
                               D$O2==1 & D$A2==j01)])
        n10<-length(D$Y[which(D$O1==1 & D$A1==i1 &
                               D$O2==0 & D$A2==j10)])
        n11<-length(D$Y[which(D$O1==1 & D$A1==i1 &
                               D$O2==1 & D$A2==j11)])
        ng<-sum(n00,n01,n10,n11)
              Dmat<-rbind(Dmat,c(g,i0,i1,j00,j01,j10,j11,ng))
              colnames(Dmat)<-c("ATS","d0","d1","d00","d01","d10","d11","N")
              g<-g+1}}}}}}} else
    if (Nstage==3 && Base==0) {
      g<-1
      Ivec<-sort(unique(D$A1))
      for (i in Ivec){
        Jvec0<-sort(unique(D$A2[which(D$A1==i & D$O2==0)]))
        Jvec1<-sort(unique(D$A2[which(D$A1==i & D$O2==1)]))
        for (j0 in Jvec0) {
        for (j1 in Jvec1) {
         Kvec00<-sort(unique(D$A3[which(D$A1==i & D$O2==0 &
                                         D$A2==j0 & D$O3==0)]))
         Kvec01<-sort(unique(D$A3[which(D$A1==i & D$O2==0 &
                                         D$A2==j0 & D$O3==1)]))
         Kvec10<-sort(unique(D$A3[which(D$A1==i & D$O2==1 &
                                         D$A2==j1 & D$O3==0)]))
         Kvec11<-sort(unique(D$A3[which(D$A1==i & D$O2==1 &
                                         D$A2==j1 & D$O3==1)]))
         for (k00 in Kvec00) {
         for (k01 in Kvec01) {
         for (k10 in Kvec10) {
         for (k11 in Kvec11) {
           n00<-length(D$Y[which(D$A1==i & D$O2==0 &
                                  D$A2==j0 & D$O3==0 & D$A3==k00)])
           n01<-length(D$Y[which(D$A1==i & D$O2==0 &
                                  D$A2==j0 & D$O3==1 & D$A3==k01)])
           n10<-length(D$Y[which(D$A1==i & D$O2==1 &
                                  D$A2==j1 & D$O3==0 & D$A3==k10)])
           n11<-length(D$Y[which(D$A1==i & D$O2==1 &
                                  D$A2==j1 & D$O3==1 & D$A3==k11)])
           ng<-sum(n00,n01,n10,n11)
           Dmat<-rbind(Dmat,c(g,i,j0,j1,k00,k01,k10,k11,ng))
           colnames(Dmat)<-c("ATS","d0","d00","d01","d000",
                            "d001","d010","d011","N")
           g<-g+1 }}}}}}}} 

        return(Dmat)
}


