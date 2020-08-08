#overlap() assess the overlapped of two strategies in design structure.
#if two strategies make the same decision at ith stage given
# same history, the index I=1; otherwise I=0

overlap<-function(ats1,ats2,nstage=NULL,baseline=0){
        A<-ats1; B<-ats2; Nstage<-nstage; Base<-baseline

        A[which(is.na(A))]<-0
        B[which(is.na(B))]<-0
        if (nstage==1 && Base==0) {if (A[1]==B[1]) {Ivec<-1} else {Ivec<-0}}
        if (nstage==1 && Base==1) {L<-2
                                  Ivec<-rep(0,L)
                                  for (l in 1:L) {if (A[l]==B[l]) {Ivec[l]<-1}}}
        if (nstage==2 && Base==0) {L<-3
                                  Ivec<-rep(0,L)
                                  for (l in 1:L){if (A[l]==B[l]) {Ivec[l]<-1}}
                                  if (A[1]!=B[1]) {Ivec[2:3]<-0}}
        if (nstage==2 && Base==1) {L<-6
                                  Ivec<-rep(0,L)
                                  for (l in 1:L){if (A[l]==B[l]) {Ivec[l]<-1}}
                                  if (A[1]!=B[1]) {Ivec[3:4]<-0}
                                  #d0!=d0 then (d00,d01) not overlapped
                                  if (A[2]!=B[2]) {Ivec[5:6]<-0}}
        #d1!=d1 then (d10,d11) not overlapped
        if (nstage==3 && Base==0) {
          L<-7
          Ivec<-rep(0,L)
          for (l in 1:L){if (A[l]==B[l]) {Ivec[l]<-1}}
                         if (A[1]!=B[1]) {Ivec[2:7]<-0}
                         if (A[2]!=B[2]) {Ivec[4:5]<-0}
                         if (A[3]!=B[3]) {Ivec[6:7]<-0}}
 
        return(Ivec)
}

