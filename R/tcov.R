#tcov() to get var-cov component for effect size
#input two strategies and sequence information
#matrix, also the method to estimate

tcov<-function(ats1,ats2,sim,family="gaussian",method="Gest"){
  ATS1<-ats1
  ATS2<-ats2
  Smat<-sim
  FA<-family
  MA<-method
  if (is.null(Smat$O1)) {Base<-0} else {Base<-1}
  Nstage<-nstage(data=Smat)

  I<-overlap(ats1=ATS1,ats2=ATS2,nstage=Nstage,baseline=Base)
  if (Nstage==1 && Base==0) {
    pi1<-Smat$PI1[which(Smat$A1==ATS1[1])]   #P(A1==1)
    uJ<-Smat$MEAN[which(Smat$A1==ATS1[1])]
    if (FA=="gaussian") {vJ<-Smat$SD[which(Smat$A1==ATS1[1])]^2} else
    if (FA=="binomial") {vJ<-uJ*(1-uJ)} else
      stop("invalid enter for argument 'family'")
    tvc<-I[1]*vJ/pi1
    } else
  if (Nstage==1 && Base==1) {
    pr<-mean(Smat$P1[which(Smat$O1==1)])                   #P(O1==1)
    pi0<-Smat$PI1[which(Smat$O1==1 & Smat$A1==ATS1[1])]    #P(A1==1|O1==0)
    pi1<-Smat$PI1[which(Smat$O1==1 & Smat$A1==ATS1[1])]    #P(A1==1|O1==1)

    u0J<-Smat$MEAN[which(Smat$O1==0 & Smat$A1==ATS1[1])]
    if (is.na(u0J)) {u0J<-0}
    # within-sequence true mean (EY)
    u1J<-Smat$MEAN[which(Smat$O1==1 & Smat$A1==ATS1[2])]
    if (is.na(u1J)) {u1J<-0}
    u0K<-Smat$MEAN[which(Smat$O1==0 & Smat$A1==ATS2[1])]
    if (is.na(u0K)) {u0K<-0}
    u1K<-Smat$MEAN[which(Smat$O1==1 & Smat$A1==ATS2[2])]
    if (is.na(u1K)) {u1K<-0}

    if (FA=="gaussian"){v0J<-Smat$SD[which(Smat$O1==0 &
                                            Smat$A1==ATS1[1])]^2
    #within-sequence variance for cont outcome
                        v1J<-Smat$SD[which(Smat$O1==1 &
                                            Smat$A1==ATS1[2])]^2
                        v0K<-Smat$SD[which(Smat$O1==0 &
                                            Smat$A1==ATS2[1])]^2
                        v1K<-Smat$SD[which(Smat$O1==1 &
                                            Smat$A1==ATS2[2])]^2} else
    if (FA=="binomial"){v0J<-u0J*(1-u0J)
                        v1J<-u1J*(1-u1J)
                        v0K<-u0K*(1-u0K)
                        v1K<-u1K*(1-u1K)} else
      stop("invalid enter for argument 'family'")
    if (is.na(v0J)) {v0J<-0}
    if (is.na(v1J)) {v1J<-0}
    if (is.na(v0K)) {v0K<-0}
    if (is.na(v1K)) {v1K<-0}

    B1<-1*pr*(1-pr)*(u1J-u0J)*(u1K-u0K)
    #MLE for cont outcome without com assumption
                     B2<-I[1]*(1-pr)*v0J/pi0
                     B3<-I[2]*pr*v1J/pi1
                     tvc<-B1+B2+B3
    } else
  if (Nstage==2 && Base==0) {
    pi1<-mean(Smat$PI1[which(Smat$A1==ATS1[1])])
    pr <-mean(Smat$P2[which(Smat$A1==ATS1[1] &
                             Smat$O2==1)])
    pi20J<-mean(Smat$PI2[which(Smat$A1==ATS1[1] &
                                Smat$O2==0 & Smat$A2==ATS1[2])])
    pi21J<-mean(Smat$PI2[which(Smat$A1==ATS1[1] &
                                Smat$O2==1 & Smat$A2==ATS1[3])])

    u0J<-Smat$MEAN[which(Smat$A1==ATS1[1] &
                          Smat$O2==0 & Smat$A2==ATS1[2])]
    u1J<-Smat$MEAN[which(Smat$A1==ATS1[1] &
                          Smat$O2==1 & Smat$A2==ATS1[3])]
    u0K<-Smat$MEAN[which(Smat$A1==ATS2[1] &
                          Smat$O2==0 & Smat$A2==ATS2[2])]
    u1K<-Smat$MEAN[which(Smat$A1==ATS2[1] &
                          Smat$O2==1 & Smat$A2==ATS2[3])]

    if (FA=="gaussian"){v0J<-Smat$SD[which(Smat$A1==ATS1[1]
                                          & Smat$O2==0 &
                                            Smat$A2==ATS1[2])]^2
                        v1J<-Smat$SD[which(Smat$A1==ATS1[1] &
                                            Smat$O2==1 &
                                            Smat$A2==ATS1[3])]^2
                        v0K<-Smat$SD[which(Smat$A1==ATS2[1] &
                                            Smat$O2==0 &
                                            Smat$A2==ATS2[2])]^2
                        v1K<-Smat$SD[which(Smat$A1==ATS2[1] &
                                            Smat$O2==1 &
                                            Smat$A2==ATS2[3])]^2}
    else
    if (FA=="binomial"){v0J<-u0J*(1-u0J)
                        v1J<-u1J*(1-u1J)
                        v0K<-u0K*(1-u0K)
                        v1K<-u1K*(1-u1K)} else
                          stop("invalid enter for argument 'family'")

    if (is.na(u0J)) {u0J<-0}
    if (is.na(u1J)) {u1J<-0}
    if (is.na(u0K)) {u0K<-0}
    if (is.na(u1K)) {u1K<-0}

    if (length(v0J)==0) {v0J<-0}
    if (length(v1J)==0) {v1J<-0}
    if (length(v0K)==0) {v0K<-0}
    if (length(v1K)==0) {v1K<-0}

    if (is.nan(pr)) {pr<-0}
    if (is.nan(pi1)) {pi1<-1}
    if (is.nan(pi20J)) {pi20J<-1}
    if (is.nan(pi21J)) {pi21J<-1}

    B1<-I[1]*(u1J-u0J)*(u1K-u0K)*pr*(1-pr)/pi1
                    B2<-I[2]*v0J*(1-pr)/(pi1*pi20J)
                    B3<-I[3]*v1J*pr    /(pi1*pi21J)
                    tvc<-B1+B2+B3
    } else
  if (Nstage==2 && Base==1) {
    P0<-mean(Smat$P1[which(Smat$O1==0)]); if (is.nan(P0)) {P0<-0}
    P1<-mean(Smat$P1[which(Smat$O1==1)]); if (is.nan(P1)) {P1<-0}

    pi0J<-mean(Smat$PI1[which(Smat$O1==0 & Smat$A1==ATS1[1])])
    if (is.nan(pi0J)) {pi0J<-1}
    pi1J<-mean(Smat$PI1[which(Smat$O1==1 & Smat$A1==ATS1[2])])
    if (is.nan(pi1J)) {pi1J<-1}

    P00J<-mean(Smat$P2[which(Smat$O1==0 & Smat$A1==ATS1[1] & Smat$O2==0)])
    if (is.nan(P00J)) {P00J<-0}
    P01J<-mean(Smat$P2[which(Smat$O1==0 & Smat$A1==ATS1[1] & Smat$O2==1)])
    if (is.nan(P01J)) {P01J<-0}
    P10J<-mean(Smat$P2[which(Smat$O1==1 & Smat$A1==ATS1[2] & Smat$O2==0)])
    if (is.nan(P10J)) {P10J<-0}
    P11J<-mean(Smat$P2[which(Smat$O1==1 & Smat$A1==ATS1[2] & Smat$O2==1)])
    if (is.nan(P11J)) {P11J<-0}

    P00K<-mean(Smat$P2[which(Smat$O1==0 & Smat$A1==ATS2[1] & Smat$O2==0)])
    if (is.nan(P00K)) {P00K<-0}
    P01K<-mean(Smat$P2[which(Smat$O1==0 & Smat$A1==ATS2[1] & Smat$O2==1)])
    if (is.nan(P01K)) {P01K<-0}
    P10K<-mean(Smat$P2[which(Smat$O1==1 & Smat$A1==ATS2[2] & Smat$O2==0)])
    if (is.nan(P10K)) {P10K<-0}
    P11K<-mean(Smat$P2[which(Smat$O1==1 & Smat$A1==ATS2[2] & Smat$O2==1)])
    if (is.nan(P11K)) {P11K<-0}
    pi00J<-mean(Smat$PI2[which(Smat$O1==0 & Smat$A1==ATS1[1] &
                                Smat$O2==0 & Smat$A2==ATS1[3])])
    if (is.nan(pi00J)) {pi00J<-1}
    pi01J<-mean(Smat$PI2[which(Smat$O1==0 & Smat$A1==ATS1[1] &
                                Smat$O2==1 & Smat$A2==ATS1[4])])
    if (is.nan(pi01J)) {pi01J<-1}
    pi10J<-mean(Smat$PI2[which(Smat$O1==1 & Smat$A1==ATS1[2] &
                                Smat$O2==0 & Smat$A2==ATS1[5])])
    if (is.nan(pi10J)) {pi10J<-1}
    pi11J<-mean(Smat$PI2[which(Smat$O1==1 & Smat$A1==ATS1[2] &
                                Smat$O2==1 & Smat$A2==ATS1[6])])
    if (is.nan(pi11J)) {pi11J<-1}
    u00J<-Smat$MEAN[which(Smat$O1==0 & Smat$A1==ATS1[1] &
                           Smat$O2==0 & Smat$A2==ATS1[3])]
    if (is.na(u00J)) {u00J<-0}
    u01J<-Smat$MEAN[which(Smat$O1==0 & Smat$A1==ATS1[1] &
                           Smat$O2==1 & Smat$A2==ATS1[4])]
    if (is.na(u01J)) {u01J<-0}
    u10J<-Smat$MEAN[which(Smat$O1==1 & Smat$A1==ATS1[2] &
                           Smat$O2==0 & Smat$A2==ATS1[5])]
    if (is.na(u10J)) {u10J<-0}
    u11J<-Smat$MEAN[which(Smat$O1==1 & Smat$A1==ATS1[2] &
                           Smat$O2==1 & Smat$A2==ATS1[6])]
    if (is.na(u11J)) {u11J<-0}
    u00K<-Smat$MEAN[which(Smat$O1==0 & Smat$A1==ATS2[1] &
                           Smat$O2==0 & Smat$A2==ATS2[3])]
    if (is.na(u00K)) {u00K<-0}
    u01K<-Smat$MEAN[which(Smat$O1==0 & Smat$A1==ATS2[1] &
                           Smat$O2==1 & Smat$A2==ATS2[4])]
    if (is.na(u01K)) {u01K<-0}
    u10K<-Smat$MEAN[which(Smat$O1==1 & Smat$A1==ATS2[2] &
                           Smat$O2==0 & Smat$A2==ATS2[5])]
    if (is.na(u10K)) {u10K<-0}
    u11K<-Smat$MEAN[which(Smat$O1==1 & Smat$A1==ATS2[2] &
                           Smat$O2==1 & Smat$A2==ATS2[6])]
    if (is.na(u11K)) {u11K<-0}
    if (FA=="gaussian"){
      v00J<-Smat$SD[which(Smat$O1==0 & Smat$A1==ATS1[1] &
                           Smat$O2==0 & Smat$A2==ATS1[3])]^2
      v01J<-Smat$SD[which(Smat$O1==0 & Smat$A1==ATS1[1] &
                           Smat$O2==1 & Smat$A2==ATS1[4])]^2
      v10J<-Smat$SD[which(Smat$O1==1 & Smat$A1==ATS1[2] &
                           Smat$O2==0 & Smat$A2==ATS1[5])]^2
      v11J<-Smat$SD[which(Smat$O1==1 & Smat$A1==ATS1[2] &
                           Smat$O2==1 & Smat$A2==ATS1[6])]^2} else
    if (FA=="binomial"){v00J<-u00J*(1-u00J)
                        v01J<-u01J*(1-u01J)
                        v10J<-u10J*(1-u10J)
                        v11J<-u11J*(1-u11J)}else
                          stop("invalid enter for argument 'family'")
    if (length(v00J)==0) {v00J<-0}
    if (length(v01J)==0) {v01J<-0}
    if (length(v10J)==0) {v10J<-0}
    if (length(v11J)==0) {v11J<-0}


    A0000<- (P0*(1-P0)*P00J*P00K+I[1]*P0*P0*P00J*
              (1-P00J)/(P0*pi0J))*u00J*u00K
    A0001<- (P0*(1-P0)*P00J*P01K-I[1]*P0*P0*P00J*
              (1-P00J)/(P0*pi0J))*u00J*u01K
    A0010<--(P0*(1-P0)*P00J*P10K)*u00J*u10K
    A0011<--(P0*(1-P0)*P00J*P11K)*u00J*u11K

    A0100<- (P0*(1-P0)*P01J*P00K-I[1]*P0*P0*P01J*
              (1-P01J)/(P0*pi0J))*u01J*u00K
    A0101<- (P0*(1-P0)*P01J*P01K+I[1]*P0*P0*P01J*
              (1-P01J)/(P0*pi0J))*u01J*u01K
    A0110<--(P0*(1-P0)*P01J*P10K)*u01J*u10K
    A0111<--(P0*(1-P0)*P01J*P11K)*u01J*u11K

    A1000<--(P1*(1-P1)*P10J*P00K)*u10J*u10K
    A1001<--(P1*(1-P1)*P10J*P01K)*u10J*u11K
    A1010<- (P1*(1-P1)*P10J*P10K+I[1]*P1*P1*P10J*
              (1-P10J)/(P1*pi1J))*u10J*u10K
    A1011<- (P1*(1-P1)*P10J*P11K-I[1]*P1*P1*P10J*
              (1-P10J)/(P1*pi1J))*u10J*u11K

    A1100<--(P1*(1-P1)*P10J*P00K)*u10J*u10K
    A1101<--(P1*(1-P1)*P10J*P01K)*u10J*u11K
    A1110<- (P1*(1-P1)*P11J*P10K+I[1]*P1*P1*P11J*
              (1-P11J)/(P1*pi1J))*u11J*u10K
    A1111<- (P1*(1-P1)*P10J*P11K-I[1]*P1*P1*P11J*
              (1-P11J)/(P1*pi1J))*u11J*u11K
    A<-sum(A0000,A0001,A0010,A0011,
          A0100,A0101,A0110,A0111,
          A1000,A1001,A1010,A1011,
          A1100,A1101,A1110,A1111)

    if (I[1]==1 && I[3]==1) {B0000<-P0*P0*P00J*P00J*
      v00J/(P0*pi0J*P00J*pi00J)} else {B0000<-0}
    if (I[1]==1 && I[4]==1) {B0101<-P0*P0*P01J*P01J*
      v01J/(P0*pi0J*P01J*pi01J)} else {B0101<-0}
    if (I[2]==1 && I[5]==1) {B1010<-P1*P1*P10J*P10J*
      v10J/(P1*pi1J*P10J*pi10J)} else {B1010<-0}
    if (I[2]==1 && I[6]==1) {B1111<-P1*P1*P11J*P11J*
      v11J/(P1*pi1J*P11J*pi11J)} else {B1111<-0}
    B<-sum(B0000,B0101,B1010,B1111)
    tvc<-A+B
    } else
  if (Nstage==3 && Base==0) {
    piJ<-mean(Smat$PI1[which(Smat$A1==ATS1[1])])
    if (is.nan(piJ)) {piJ<-0} # A1<-dJ[1]

  P0J<-mean(Smat$P2[which(Smat$A1==ATS1[1] & Smat$O2==0)])
  if (is.nan(P0J)) {P0J<-0}
  P1J<-mean(Smat$P2[which(Smat$A1==ATS1[1] & Smat$O2==1)])
  if (is.nan(P1J)) {P1J<-0}
  P0K<-mean(Smat$P2[which(Smat$A1==ATS2[1] & Smat$O2==0)])
  if (is.nan(P0K)) {P0K<-0}
  P1K<-mean(Smat$P2[which(Smat$A1==ATS2[1] & Smat$O2==1)])
  if (is.nan(P1K)) {P1K<-0}

  pi0J<-mean(Smat$PI2[which(Smat$A1==ATS1[1] &
                             Smat$O2==0 & Smat$A2==ATS1[2])])
  if (is.nan(pi0J)) {pi0J<-0}
  pi1J<-mean(Smat$PI2[which(Smat$A1==ATS1[1] &
                             Smat$O2==1 & Smat$A2==ATS1[3])])
  if (is.nan(pi1J)) {pi1J<-0}

  P00J<-mean(Smat$P3[which(Smat$A1==ATS1[1] &
                            Smat$O2==0 & Smat$A2==ATS1[2] & Smat$O3==0)])
  if (is.nan(P00J)) {P00J<-0}
  P01J<-mean(Smat$P3[which(Smat$A1==ATS1[1] &
                            Smat$O2==0 & Smat$A2==ATS1[2] & Smat$O3==1)])
  if (is.nan(P01J)) {P01J<-0}
  P10J<-mean(Smat$P3[which(Smat$A1==ATS1[1] &
                            Smat$O2==1 & Smat$A2==ATS1[3] & Smat$O3==0)])
  if (is.nan(P10J)) {P10J<-0}
  P11J<-mean(Smat$P3[which(Smat$A1==ATS1[1] &
                            Smat$O2==1 & Smat$A2==ATS1[3] & Smat$O3==1)])
  if (is.nan(P11J)) {P11J<-0}

  P00K<-mean(Smat$P3[which(Smat$A1==ATS2[1] &
                            Smat$O2==0 & Smat$A2==ATS2[2] & Smat$O3==0)])
  if (is.nan(P00K)) {P00K<-0}
  P01K<-mean(Smat$P3[which(Smat$A1==ATS2[1] &
                            Smat$O2==0 & Smat$A2==ATS2[2] & Smat$O3==1)])
  if (is.nan(P01K)) {P01K<-0}
  P10K<-mean(Smat$P3[which(Smat$A1==ATS2[1] &
                            Smat$O2==1 & Smat$A2==ATS2[3] & Smat$O3==0)])
  if (is.nan(P10K)) {P10K<-0}
  P11K<-mean(Smat$P3[which(Smat$A1==ATS2[1] &
                            Smat$O2==1 & Smat$A2==ATS2[3] & Smat$O3==1)])
  if (is.nan(P11K)) {P11K<-0}

  pi00J<-mean(Smat$PI3[which(Smat$A1==ATS1[1] & Smat$O2==0 &
                              Smat$A2==ATS1[2] &
                              Smat$O3==0 & Smat$A3==ATS1[4])])
  if (is.nan(pi00J)) {pi00J<-0}
  pi01J<-mean(Smat$PI3[which(Smat$A1==ATS1[1] & Smat$O2==0 &
                              Smat$A2==ATS1[2] &
                              Smat$O3==1 & Smat$A3==ATS1[5])])
  if (is.nan(pi01J)) {pi01J<-0}
  pi10J<-mean(Smat$PI3[which(Smat$A1==ATS1[1] & Smat$O2==1 &
                              Smat$A2==ATS1[3] &
                              Smat$O3==0 & Smat$A3==ATS1[6])])
  if (is.nan(pi10J)) {pi10J<-0}
  pi11J<-mean(Smat$PI3[which(Smat$A1==ATS1[1] & Smat$O2==1 &
                              Smat$A2==ATS1[3] &
                              Smat$O3==1 & Smat$A3==ATS1[7])])
  if (is.nan(pi11J)) {pi11J<-0}

  u00J<-Smat$MEAN[which(Smat$A1==ATS1[1] &
                         Smat$O2==0 & Smat$A2==ATS1[2] &
                         Smat$O3==0 & Smat$A3==ATS1[4])]
  if (is.nan(u00J)) {u00J<-0}        # within-sequence true mean (EY)
  u01J<-Smat$MEAN[which(Smat$A1==ATS1[1] &
                         Smat$O2==0 & Smat$A2==ATS1[2] &
                         Smat$O3==0 & Smat$A3==ATS1[5])]
  if (is.nan(u01J)) {u01J<-0}
  u10J<-Smat$MEAN[which(Smat$A1==ATS1[1] &
                         Smat$O2==0 & Smat$A2==ATS1[3] &
                         Smat$O3==0 & Smat$A3==ATS1[6])]
  if (is.nan(u10J)) {u10J<-0}
  u11J<-Smat$MEAN[which(Smat$A1==ATS1[1] &
                         Smat$O2==0 & Smat$A2==ATS1[3] &
                         Smat$O3==0 & Smat$A3==ATS1[7])]
  if (is.nan(u11J)) {u11J<-0}

  u00K<-Smat$MEAN[which(Smat$A1==ATS2[1] &
                         Smat$O2==0 & Smat$A2==ATS2[2] &
                         Smat$O3==0 & Smat$A3==ATS2[4])]
  if (is.nan(u00K)) {u00K<-0}        # within-sequence true mean (EY)
  u01K<-Smat$MEAN[which(Smat$A1==ATS2[1] &
                         Smat$O2==0 & Smat$A2==ATS2[2] &
                         Smat$O3==0 & Smat$A3==ATS2[5])]
  if (is.nan(u01K)) {u01K<-0}
  u10K<-Smat$MEAN[which(Smat$A1==ATS2[1] &
                         Smat$O2==0 & Smat$A2==ATS2[3] &
                         Smat$O3==0 & Smat$A3==ATS2[6])]
  if (is.nan(u10K)) {u10K<-0}
  u11K<-Smat$MEAN[which(Smat$A1==ATS2[1] &
                         Smat$O2==0 & Smat$A2==ATS2[3] &
                         Smat$O3==0 & Smat$A3==ATS2[7])]
  if (is.nan(u11K)) {u11K<-0}

  if (FA=="gaussian"){
    v00J<-Smat$SD[which(Smat$A1==ATS1[1] &
                         Smat$O2==0 & Smat$A2==ATS1[2] &
                         Smat$O3==0 & Smat$A3==ATS1[4])]^2
    # within-sequence true mean (EY)
    v01J<-Smat$SD[which(Smat$A1==ATS1[1] &
                         Smat$O2==0 & Smat$A2==ATS1[2] &
                         Smat$O3==0 & Smat$A3==ATS1[5])]^2
    v10J<-Smat$SD[which(Smat$A1==ATS1[1] &
                         Smat$O2==0 & Smat$A2==ATS1[3] &
                         Smat$O3==0 & Smat$A3==ATS1[6])]^2
    v11J<-Smat$SD[which(Smat$A1==ATS1[1] &
                         Smat$O2==0 & Smat$A2==ATS1[3] &
                         Smat$O3==0 &
                         Smat$A3==ATS1[7])]^2}else
  if (FA=="binomial") {bv00J<-u00J*(1-u00J)
  if (is.na(bv00J)) {bv00J<-0}
  #within-sequence var est for bin outcome
                       bv01J<-u01J*(1-u01J)
                       if (is.na(bv01J)) {bv01J<-0}
                       bv10J<-u10J*(1-u10J)
                       if (is.na(bv10J)) {bv10J<-0}
                       bv11J<-u11J*(1-u11J)
                       if (is.na(bv11J)) {bv11J<-0}}else
                         stop("invalid enter for argument 'family'")
  if (length(v00J)==0) {v00J<-0}
  if (length(v01J)==0) {v01J<-0}
  if (length(v10J)==0) {v10J<-0}
  if (length(v11J)==0) {v11J<-0}

  A0000<- (I[1]*P0J*(1-P0J)*P00J*P00K/piJ+I[2]*
            P0J*P0J*P00J*(1-P00J)/
            (piJ*P0J*pi00J))*u00J*u00K
  A0001<- (I[1]*P0J*(1-P0J)*P00J*P01K/piJ-I[2]*
            P0J*P0J*P00J*(1-P00J)/
            (piJ*P0J*pi00J))*u00J*u01K
  A0010<--(I[1]*P0J*(1-P0J)*P00J*P10K/piJ)*
    u00J*u10K
  A0011<--(I[1]*P0J*(1-P0J)*P00J*P11K/piJ)*
    u00J*u11K

  A0100<- (I[1]*P0J*(1-P0J)*P01J*P00K/piJ-I[2]*
            P0J*P0J*P01J*(1-P01J)/(piJ*P0J*pi01J))*u01J*u00K
  A0101<- (I[1]*P0J*(1-P0J)*P01J*P01K/piJ+I[2]*
            P0J*P0J*P01J*(1-P01J)/(piJ*P0J*pi01J))*u01J*u01K
  A0110<--(I[1]*P0J*(1-P0J)*P01J*P10K/piJ)*u01J*u10K
  A0111<--(I[1]*P0J*(1-P0J)*P01J*P11K/piJ)*u01J*u11K

  A1000<--(I[1]*P1J*(1-P1J)*P10J*P00K/piJ)*u10J*u10K
  A1001<--(I[1]*P1J*(1-P1J)*P10J*P01K/piJ)*u10J*u11K
  A1010<- (I[1]*P1J*(1-P1J)*P10J*P10K/piJ+I[3]*
            P1J*P1J*P10J*(1-P10J)/(piJ*P1J*pi10J))*u10J*u10K
  A1011<- (I[1]*P1J*(1-P1J)*P10J*P11K/piJ-I[3]*
            P1J*P1J*P10J*(1-P10J)/(piJ*P1J*pi10J))*u10J*u11K

  A1100<--(I[1]*P1J*(1-P1J)*P10J*P00K/piJ)*u10J*u10K
  A1101<--(I[1]*P1J*(1-P1J)*P10J*P01K/piJ)*u10J*u11K
  A1110<- (I[1]*P1J*(1-P1J)*P11J*P10K/piJ+I[3]*
            P1J*P1J*P11J*(1-P11J)/(piJ*P1J*pi11J))*u11J*u10K
  A1111<- (I[1]*P1J*(1-P1J)*P10J*P11K/piJ-I[3]*
            P1J*P1J*P11J*(1-P11J)/(piJ*P1J*pi11J))*u11J*u11K
  A<-sum(A0000,A0001,A0010,A0011,
        A0100,A0101,A0110,A0111,
        A1000,A1001,A1010,A1011,
        A1100,A1101,A1110,A1111)

  if (FA=="binomial"){
    if (I[1]==1 && I[2]==1 && I[4]==1) {B0000<-P0J*P0J*P00J*
      P00K*bv00J/(piJ*P0J*pi00J*P00J*pi00J)}  else {B0000<-0}
    if (I[1]==1 && I[2]==1 && I[5]==1) {B0101<-P0J*P0J*P01J*
      P01K*bv01J/(piJ*P0J*pi00J*P01J*pi01J)}  else {B0101<-0}
    if (I[1]==1 && I[3]==1 && I[6]==1) {B1010<-P1J*P1J*P10J*
      P10K*bv10J/(piJ*P1J*pi10J*P10J*pi10J)}  else {B1010<-0}
    if (I[1]==1 && I[3]==1 && I[7]==1) {B1111<-P1J*P1J*P11J*
      P11K*bv11J/(piJ*P1J*pi11J*P11J*pi11J)}  else {B1111<-0}
    } else
  if (FA=="gaussian")  {
    if (I[1]==1 && I[2]==1 && I[4]==1) {B0000<-P0J*P0J*P00J*
      P00K*v00J/(piJ*P0J*pi00J*P00J*pi00J)}   else {B0000<-0}
    if (I[1]==1 && I[2]==1 && I[5]==1) {B0101<-P0J*P0J*P01J*
      P01K*v01J/(piJ*P0J*pi00J*P01J*pi01J)}   else {B0101<-0}
    if (I[1]==1 && I[3]==1 && I[6]==1) {B1010<-P1J*P1J*P10J*
      P10K*v10J/(piJ*P1J*pi10J*P10J*pi10J)}   else {B1010<-0}
    if (I[1]==1 && I[3]==1 && I[7]==1) {B1111<-P1J*P1J*P11J*
      P11K*v11J/(piJ*P1J*pi11J*P11J*pi11J)}   else {B1111<-0}
  }else
    stop("invalid enter for argument 'family'")
  B<-sum(B0000,B0101,B1010,B1111)
  tvc<-A+B
  } 

     return(tvc)
}
