#function gemean() compute G-estimator (IPWN) for single ATS value

gemean<-function(data,ats=NULL){
       D<-as.data.frame(data)
       ATS<-ats
       if (is.null(D$O1)) {Base<-0} else {Base<-1}
       Nstage<-nstage(data=D)
       N<-nrow(D)

       if (Nstage==1 && Base==0) {eu<-mean(D$Y[which(D$A1==ATS[1])])} else
       if (Nstage==1 && Base==1) {w0<-P0<-length(D$Y[which(D$O1==0)])/N
                                 w1<-P1<-1-P0
                                 mu0<-mean(D$Y[which(D$O1==0 &
                                                      D$A1==ATS[1])])
                                 mu1<-mean(D$Y[which(D$O1==1 &
                                                      D$A1==ATS[2])])
                                 eu<-w0*mu0+w1*mu1
                                 } else
       if (Nstage==2 && Base==0) {
         P0<-length(D$Y[which(D$A1==ATS[1] &
                               D$O2==0)])/length(D$Y[which(D$A1==ATS[1])])
         P1<-1-P0
         w0<-P0
         w1<-P1
         mu0<-mean(D$Y[which(D$A1==ATS[1] & D$O2==0 &
                              D$A2==ATS[2])])
         mu1<-mean(D$Y[which(D$A1==ATS[1] & D$O2==1 &
                              D$A2==ATS[3])])
         eu<-w0*mu0+w1*mu1} else
       if (Nstage==2 && Base==1) {
         P0<-length(D$Y[which(D$O1==0)])/N
         P1<-1-P0
         P00<-length(D$Y[which(D$O1==0 & D$A1==ATS[1] &
                                D$O2==0)])/length(D$Y[which(D$O1==0 &
                                                              D$A1==ATS[1])])
         P01<-length(D$Y[which(D$O1==0 & D$A1==ATS[1] &
                                D$O2==1)])/length(D$Y[which(D$O1==0 &
                                                              D$A1==ATS[1])])
         P10<-length(D$Y[which(D$O1==1 & D$A1==ATS[2] &
                                D$O2==0)])/length(D$Y[which(D$O1==1 &
                                                              D$A1==ATS[2])])
         P11<-length(D$Y[which(D$O1==1 & D$A1==ATS[2] &
                                D$O2==1)])/length(D$Y[which(D$O1==1 &
                                                              D$A1==ATS[2])])
         w00<-P0*P00
         w01<-P0*P01
         w10<-P1*P10
         w11<-P1*P11
         mu00<-mean(D$Y[which(D$O1==0 & D$A1==ATS[1] &
                               D$O2==0 & D$A2==ATS[3])])
         mu01<-mean(D$Y[which(D$O1==0 & D$A1==ATS[1] &
                               D$O2==1 & D$A2==ATS[4])])
         mu10<-mean(D$Y[which(D$O1==1 & D$A1==ATS[2] &
                               D$O2==0 & D$A2==ATS[5])])
         mu11<-mean(D$Y[which(D$O1==1 & D$A1==ATS[2] &
                               D$O2==1 & D$A2==ATS[6])])
         eu<-w00*mu00+w01*mu01+w10*mu10+w11*mu11
         } else
       if (Nstage==3 && Base==0) {
         P0<-length(D$Y[which(D$A1==ATS[1] &
                               D$O2==0)])/length(D$Y[which(D$A1==ATS[1])])
         P1<-1-P0
         P00<-length(D$Y[which(D$A1==ATS[1] &
                                D$O2==0 & D$A2==ATS[2] & D$O3==0)])/
             length(D$Y[which(D$A1==ATS[1] &
                                D$O2==0 & D$A2==ATS[2])])
         P01<-1-P00
         P10<-length(D$Y[which(D$A1==ATS[1] &
                                D$O2==1 & D$A2==ATS[3] & D$O3==0)])/
             length(D$Y[which(D$A1==ATS[1] &
                                D$O2==1 & D$A2==ATS[3])])
         P11<-1-P10

        w00<-P0*P00
        w01<-P0*P01
        w10<-P1*P10
        w11<-P1*P11
        mu00<-mean(D$Y[which(D$A1==ATS[1] &
                              D$O2==0 & D$A2==ATS[2] & D$O3==0 & D$A3==ATS[4])])
        mu01<-mean(D$Y[which(D$A1==ATS[1] &
                              D$O2==0 & D$A2==ATS[2] & D$O3==1 & D$A3==ATS[5])])
        mu10<-mean(D$Y[which(D$A1==ATS[1] &
                              D$O2==1 & D$A2==ATS[3] & D$O3==0 & D$A3==ATS[6])])
        mu11<-mean(D$Y[which(D$A1==ATS[1] &
                              D$O2==1 & D$A2==ATS[3] & D$O3==1 & D$A3==ATS[7])])
        eu<-w00*mu00+w01*mu01+w10*mu10+w11*mu11}

       return(eu)
}

