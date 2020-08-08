#ecov() estimted covariance between two DTRs (variance when two DTR identical)
#' @importFrom stats var

ecov<-function(data,ats1=c(0),ats2=c(0),family="gaussian",common=TRUE){
  D<-as.data.frame(data); ATS1<-ats1; ATS2<-ats2; FA<-family; C<-common
  if (is.null(D$O1)) {Base<-0} else {Base<-1}
  Nstage<-nstage(data=D)
  N<-nrow(D)
  ATS1[which(is.na(ATS1))] <-0
  ATS2[which(is.na(ATS2))]<-0
  
  I<-overlap(ats1=ATS1,ats2=ATS2,nstage=Nstage,baseline=Base)
  
  if (Nstage==1 && Base==0) {DJ<-D[which(D$A1==ATS1[1]),]
  DK<-D[which(D$A1==ATS2[1]),]
  
  nJ<-length(DK$Y)
  
  uJ<-mean(DJ$Y)
  if (is.na(uJ)) {uJ<-0}
  # within-sequence true mean (EY)
  uK<-mean(DK$Y)
  if (is.na(uK)) {uK<-0}
  
  vJ<-var(DJ$Y)
  if (is.na(vJ)) {vJ<-0}
  #within-sequence variance for cont outcome
  vK<-var(DK$Y)
  if (is.na(vK)) {vK<-0}
  bvJ<-uJ*(1-uJ)
  if (is.na(bvJ)) {bvJ<-0}
  #within-sequence var est for bin outcome
  bvK<-uK*(1-uK)
  if (is.na(bvK)) {bvK<-0}
  comvar<-cv(data=D)
  if (is.na(comvar)) {comvar<-0}
  
  if (FA=="binomial") {vc<-I[1]*bvJ/nJ}
  else if (FA=="gaussian"){
    if(C){vc<-I[1]*comvar/nJ}
    else {vc<-I[1]*vJ/nJ}
  }
  else
    stop("invalid enter for argument 'family'")
  }
  else
    if (Nstage==1 && Base==1) {
      DJ<-D[which( (D$O1==0 & D$A1==ATS1[1])|(D$O1==1 & D$A1==ATS1[2]) ),]
      DK<-D[which( (D$O1==0 & D$A1==ATS2[1])|(D$O1==1 & D$A1==ATS2[2]) ),]
      
      n0J<-length(DJ$Y[which(DJ$O1==0)])
      if (n0J==0|is.na(n0J)) {n0J<-1}
      #sequence size of R=0 under dJ
      n1J<-length(DJ$Y[which(DJ$O1==1)])
      if (n1J==0|is.na(n1J)) {n1J<-1}
      #sequence size of R=1 under dJ
      
      P<-length(D$Y[which(D$O1==1)])/length(D$Y)
      
      u0J<-mean(DJ$Y[which(DJ$O1==0)])
      if (is.na(u0J)) {u0J<-0}
      # within-sequence true mean (EY)
      u1J<-mean(DJ$Y[which(DJ$O1==1)])
      if (is.na(u1J)) {u1J<-0}
      u0K<-mean(DK$Y[which(DK$O1==0)])
      if (is.na(u0K)) {u0K<-0}
      u1K<-mean(DK$Y[which(DK$O1==1)])
      if (is.na(u1K)) {u1K<-0}
      v0J<-var(DJ$Y[which(DJ$O1==0)])
      if (is.na(v0J)) {v0J<-0}
      #within-sequence variance for cont outcome
      v1J<-var(DJ$Y[which(DJ$O1==1)])
      if (is.na(v1J)) {v1J<-0}
      v0K<-var(DK$Y[which(DK$O1==0)])
      if (is.na(v0K)) {v0K<-0}
      v1K<-var(DK$Y[which(DK$O1==1)])
      if (is.na(v1K)) {v1K<-0}
      bv0J<-u0J*(1-u0J)
      if (is.na(bv0J)) {bv0J<-0}
      #within-sequence var est for bin outcome
      bv1J<-u1J*(1-u1J)
      if (is.na(bv1J)) {bv1J<-0}
      bv0K<-u0K*(1-u0K)
      if (is.na(bv0K)) {bv0K<-0}
      bv1K<-u1K*(1-u1K)
      if (is.na(bv1K)) {bv1K<-0}
      comvar<-cv(data=D)
      if (is.na(comvar)) {comvar<-0}
      
      if (FA=="binomial") {B1<-1*P*(1-P)*(u1J-u0J)*(u1K-u0K)/N
      #MLE for binary outcome
      B2<-I[1]*(1-P)*(1-P)*bv0J/n0J
      B3<-I[2]*P*P*bv1J/n1J
      vc<-B1+B2+B3}
      else if (FA=="gaussian") {
        if(C){
          B1<-1*P*(1-P)*(u1J-u0J)*(u1K-u0K)/N
          #MLE for cont outcome with com assumption
          B2<-I[1]*(1-P)*(1-P)*comvar/n0J
          B3<-I[2]*P*P*comvar/n1J
          vc<-B1+B2+B3
        } else {B1<-1*P*(1-P)*(u1J-u0J)*(u1K-u0K)/N
        #MLE for cont outcome without com assumption
        B2<-I[1]*(1-P)*(1-P)*v0J/n0J
        B3<-I[2]*P*P*v1J/n1J
        vc<-B1+B2+B3}
      }
      else
        stop("invalid enter for argument 'family'")
    } else
      if (Nstage==2 && Base==0) {
        #so far no O1
        DJ<-D[which((D$A1==ATS1[1] & D$O2==0 & D$A2==ATS1[2])|
                      (D$A1==ATS1[1] & D$O2==1 & D$A2==ATS1[3])),]
        DK<-D[which((D$A1==ATS2[1] & D$O2==0 & D$A2==ATS2[2])|
                      (D$A1==ATS2[1] & D$O2==1 & D$A2==ATS2[3])),]
        
        #sample size of A1=d1
        nj<-length(D$Y[which(D$A1==ATS1[1])])
        #sequence size of R=0 under dJ
        n0J<-length(DJ$Y[which(DJ$O2==0)])
        if (n0J==0|is.na(n0J)) {n0J<-1}
        #sequence size of R=1 under dJ
        n1J<-length(DJ$Y[which(DJ$O2==1)])
        if (n1J==0|is.na(n1J)) {n1J<-1}
        
        #estimate respose rate P=P(R=1|A1=d1J)
        P<-length(D$Y[which(D$A1==ATS1[1]&D$O2==1)])/
          length(D$Y[which(D$A1==ATS1[1])])
        
        # within-sequence true mean (EY)
        u0J<-mean(DJ$Y[which(DJ$O2==0)])
        if (is.na(u0J)) {u0J<-0}
        u1J<-mean(DJ$Y[which(DJ$O2==1)])
        if (is.na(u1J)) {u1J<-0}
        u0K<-mean(DK$Y[which(DK$O2==0)])
        if (is.na(u0K)) {u0K<-0}
        u1K<-mean(DK$Y[which(DK$O2==1)])
        if (is.na(u1K)) {u1K<-0}
        
        #within-sequence variance for cont outcome       
        v0J<-var(DJ$Y[which(DJ$O2==0)])
        if (is.na(v0J)) {v0J<-0}
        v1J<-var(DJ$Y[which(DJ$O2==1)])
        if (is.na(v1J)) {v1J<-0}
        v0K<-var(DK$Y[which(DK$O2==0)])
        if (is.na(v0K)) {v0K<-0}
        v1K<-var(DK$Y[which(DK$O2==1)])
        if (is.na(v1K)) {v1K<-0}
        
        #within-sequence var est for bin outcome
        bv0J<-u0J*(1-u0J)
        if (is.na(bv0J)) {bv0J<-0}
        bv1J<-u1J*(1-u1J)
        if (is.na(bv1J)) {bv1J<-0}
        bv0K<-u0K*(1-u0K)
        if (is.na(bv0K)) {bv0K<-0}
        bv1K<-u1K*(1-u1K)
        if (is.na(bv1K)) {bv1K<-0}
        
        #common within-sequence variance
        comvar<-cv(data=D)
        if (is.na(comvar)) {comvar<-0}
        
        #MLE for binary outcome
        if (FA=="binomial") {B1<-I[1]*P*(1-P)*(u1J-u0J)*(u1K-u0K)/nj
        B2<-I[2]*(1-P)*(1-P)*bv0J/n0J
        B3<-I[3]*P*P*bv1J/n1J
        vc<-B1+B2+B3}
        else if (FA=="gaussian") {
          if(C) {B1<-I[1]*P*(1-P)*(u1J-u0J)*(u1K-u0K)/nj
          #MLE for cont outcome with com assumption
          B2<-I[2]*(1-P)*(1-P)*comvar/n0J
          B3<-I[3]*P*P*comvar/n1J
          vc<-B1+B2+B3}
          else  {B1<-I[1]*P*(1-P)*(u1J-u0J)*(u1K-u0K)/nj
          #MLE for cont outcome without com assumption
          B2<-I[2]*(1-P)*(1-P)*v0J/n0J
          B3<-I[3]*P*P*v1J/n1J
          vc<-B1+B2+B3}
        }
        else stop("invalid enter for argument 'family'")
        
      } else
        if (Nstage==2 && Base==1) {
          DJ<-D[which((D$O1==0 & D$A1==ATS1[1] & D$O2==0 &
                         D$A2==ATS1[3])|             
                        (D$O1==0 & D$A1==ATS1[1] & D$O2==1 &
                           D$A2==ATS1[4])|              
                        (D$O1==1 & D$A1==ATS1[2] & D$O2==0 &
                           D$A2==ATS1[5])|              
                        (D$O1==1 & D$A1==ATS1[2] & D$O2==1 &
                           D$A2==ATS1[6])),]          
          #so far no O1
          DK<-D[which((D$O1==0 & D$A1==ATS2[1] & D$O2==0 &
                         D$A2==ATS2[3])|             
                        (D$O1==0 & D$A1==ATS2[1] & D$O2==1 &
                           D$A2==ATS2[4])|              
                        (D$O1==1 & D$A1==ATS2[2] & D$O2==0 &
                           D$A2==ATS2[5])|               
                        (D$O1==1 & D$A1==ATS2[2] & D$O2==1 &
                           D$A2==ATS2[6])),]            
          
          n0J<-length(D$Y[which(D$O1==0 & D$A1==ATS1[1])])
          if (n0J==0|is.na(n0J)) {n0J<-1}
          #sample size of A1=d1
          n1J<-length(D$Y[which(D$O1==1 & D$A1==ATS1[2])])
          if (n1J==0|is.na(n1J)) {n1J<-1}
          
          n00J<-length(DJ$Y[which(DJ$O1==0 & DJ$O2==0)])
          if (n00J==0|is.na(n00J)) {n00J<-1}
          #sequence size of R=0 under dJ
          n01J<-length(DJ$Y[which(DJ$O1==0 & DJ$O2==1)])
          if (n01J==0|is.na(n01J)) {n01J<-1}
          #sequence size of R=1 under dJ
          n10J<-length(DJ$Y[which(DJ$O1==1 & DJ$O2==0)])
          if (n10J==0|is.na(n10J)) {n10J<-1}
          #sequence size of R=0 under dJ
          n11J<-length(DJ$Y[which(DJ$O1==1 & DJ$O2==1)])
          if (n11J==0|is.na(n11J)) {n11J<-1}
          
          
          P0<-length(D$Y[which(D$O1==0)])/length(D$Y)
          #estimate respose rate P=P(R=1|A1=d1J)
          P1<-1-P0
          
          P00J<-length(D$Y[which(D$O1==0 & D$A1==ATS1[1] &
                                   D$O2==0)])/
            length(D$Y[which(D$O1==0 & D$A1==ATS1[1])])
          P01J<-1-P00J
          P10J<-length(D$Y[which(D$O1==1 & D$A1==ATS1[2] &
                                   D$O2==0)])/
            length(D$Y[which(D$O1==1 & D$A1==ATS1[2])])
          P11J<-1-P10J
          
          P00K<-length(D$Y[which(D$O1==0 & D$A1==ATS2[1] &
                                   D$O2==0)])/
            length(D$Y[which(D$O1==0 & D$A1==ATS2[1])])
          P01K<-1-P00K
          P10K<-length(D$Y[which(D$O1==1 & D$A1==ATS2[2] &
                                   D$O2==0)])/
            length(D$Y[which(D$O1==1 & D$A1==ATS2[2])])
          P11K<-1-P10K
          
          u00J<-mean(DJ$Y[which(DJ$O1==0 & DJ$O2==0)])
          if (is.na(u00J)) {u00J<-0}
          # within-sequence true mean (EY)
          u01J<-mean(DJ$Y[which(DJ$O1==0 & DJ$O2==1)])
          if (is.na(u01J)) {u01J<-0}
          u10J<-mean(DJ$Y[which(DJ$O1==1 & DJ$O2==0)])
          if (is.na(u10J)) {u10J<-0}
          u11J<-mean(DJ$Y[which(DJ$O1==1 & DJ$O2==1)])
          if (is.na(u11J)) {u11J<-0}
          
          #u00K<-mean(DJ$Y[which(DK$O1==0 & DK$O2==0)])   
          u00K<-mean(DK$Y[which(DK$O1==0 & DK$O2==0)]) 
          if (is.na(u00K)) {u00K<-0}
          #u01K<-mean(DJ$Y[which(DK$O1==0 & DK$O2==1)])
          u01K<-mean(DK$Y[which(DK$O1==0 & DK$O2==1)])
          if (is.na(u01K)) {u01K<-0}
          #u10K<-mean(DJ$Y[which(DK$O1==1 & DK$O2==0)])
          u10K<-mean(DK$Y[which(DK$O1==1 & DK$O2==0)])
          if (is.na(u10K)) {u10K<-0}
          #u11K<-mean(DJ$Y[which(DK$O1==1 & DK$O2==1)])
          u11K<-mean(DK$Y[which(DK$O1==1 & DK$O2==1)])
          if (is.na(u11K)) {u11K<-0}
          
          v00J<-var(DJ$Y[which(DJ$O1==0 & DJ$O2==0)])
          if (is.na(v00J)) {v00J<-0}
          #within-sequence variance for cont outcome
          v01J<-var(DJ$Y[which(DJ$O1==0 & DJ$O2==1)])
          if (is.na(v01J)) {v01J<-0}
          v10J<-var(DJ$Y[which(DJ$O1==1 & DJ$O2==0)])
          if (is.na(v10J)) {v10J<-0}
          v11J<-var(DJ$Y[which(DJ$O1==1 & DJ$O2==1)])
          if (is.na(v11J)) {v11J<-0}
          
          bv00J<-u00J*(1-u00J)
          if (is.na(bv00J)) {bv00J<-0}
          #within-sequence var est for bin outcome
          bv01J<-u01J*(1-u01J)
          if (is.na(bv01J)) {bv01J<-0}
          bv10J<-u10J*(1-u10J)
          if (is.na(bv10J)) {bv10J<-0}
          bv11J<-u11J*(1-u11J)
          if (is.na(bv11J)) {bv11J<-0}
          comvar<-cv(data=D)
          if (is.na(comvar)) {comvar<-0}
          
          A0000<- (P0*(1-P0)*P00J*P00K/N+I[1]*P0*P0*P00J*
                     (1-P00J)/n0J)*u00J*u00K
          A0001<- (P0*(1-P0)*P00J*P01K/N-I[1]*P0*P0*P00J*
                     (1-P00J)/n0J)*u00J*u01K
          A0010<--(P0*(1-P0)*P00J*P10K/N)*u00J*u10K
          A0011<--(P0*(1-P0)*P00J*P11K/N)*u00J*u11K
          
          A0100<- (P0*(1-P0)*P01J*P00K/N-I[1]*P0*P0*P01J*
                     (1-P01J)/n0J)*u01J*u00K
          A0101<- (P0*(1-P0)*P01J*P01K/N+I[1]*P0*P0*P01J*
                     (1-P01J)/n0J)*u01J*u01K
          A0110<--(P0*(1-P0)*P01J*P10K/N)*u01J*u10K
          A0111<--(P0*(1-P0)*P01J*P11K/N)*u01J*u11K
          
          #A1000<--(P1*(1-P1)*P10J*P00K/N)*u10J*u10K
          A1000<--(P1*(1-P1)*P10J*P00K/N)*u10J*u00K 
          #A1001<--(P1*(1-P1)*P10J*P01K/N)*u10J*u11K
          A1001<--(P1*(1-P1)*P10J*P01K/N)*u10J*u01K  
          A1010<- (P1*(1-P1)*P10J*P10K/N+I[1]*P1*P1*P10J*
                     (1-P10J)/n1J)*u10J*u10K
          A1011<- (P1*(1-P1)*P10J*P11K/N-I[1]*P1*P1*P10J*
                     (1-P10J)/n1J)*u10J*u11K
          
    #A1100<--(P1*(1-P1)*P10J*P00K/N)*u10J*u10K
    A1100<--(P1*(1-P1)*P11J*P00K/N)*u11J*u00K 
    #A1101<--(P1*(1-P1)*P10J*P01K/N)*u10J*u11K
    A1101<--(P1*(1-P1)*P11J*P01K/N)*u11J*u01K 
    #A1110<- (P1*(1-P1)*P11J*P10K/N+I[1]*P1*P1*P11J*(1-P11J)/n1J)*u11J*u10K
    A1110<- (P1*(1-P1)*P11J*P10K/N-I[1]*P1*P1*P11J*(1-P11J)/n1J)*u11J*u10K  
    #A1111<- (P1*(1-P1)*P10J*P11K/N-I[1]*P1*P1*P11J*(1-P11J)/n1J)*u11J*u11K 
    A1111<- (P1*(1-P1)*P11J*P11K/N+I[1]*P1*P1*P11J*(1-P11J)/n1J)*u11J*u11K 
    A<-sum(A0000,A0001,A0010,A0011,
           A0100,A0101,A0110,A0111,
           A1000,A1001,A1010,A1011,
           A1100,A1101,A1110,A1111)
    if (FA=="binomial"){
      if (I[1]==1 && I[3]==1) {B0000<-P0*P0*P00J*P00K*bv00J/n00J}
      else {B0000<-0}
      if (I[1]==1 && I[4]==1) {B0101<-P0*P0*P01J*P01K*bv01J/n01J}
      else {B0101<-0}
      if (I[2]==1 && I[5]==1) {B1010<-P1*P1*P10J*P10K*bv10J/n10J}
      else {B1010<-0}
      if (I[2]==1 && I[6]==1) {B1111<-P1*P1*P11J*P11K*bv11J/n11J}
      else {B1111<-0}
    }
    else if (FA=="gaussian") {
      if(C){if (I[1]==1 && I[3]==1) {B0000<-P0*P0*P00J*P00K*comvar/n00J}
        else {B0000<-0}
        if (I[1]==1 && I[4]==1) {B0101<-P0*P0*P01J*P01K*comvar/n01J}
        else {B0101<-0}
        if (I[2]==1 && I[5]==1) {B1010<-P1*P1*P10J*P10K*comvar/n10J}
        else {B1010<-0}
        if (I[2]==1 && I[6]==1) {B1111<-P1*P1*P11J*P11K*comvar/n11J}
        else {B1111<-0}}
            
    else {if (I[1]==1 && I[3]==1) {B0000<-P0*P0*P00J*P00K*v00J/n00J}
      else {B0000<-0}
      if (I[1]==1 && I[4]==1) {B0101<-P0*P0*P01J*P01K*v01J/n01J}
      else {B0101<-0}
      if (I[2]==1 && I[5]==1) {B1010<-P1*P1*P10J*P10K*v10J/n10J}
      else {B1010<-0}
      if (I[2]==1 && I[6]==1) {B1111<-P1*P1*P11J*P11K*v11J/n11J}
      else {B1111<-0}
    }
    }
    else stop("invalid enter for argument 'family'")
    B<-sum(B0000,B0101,B1010,B1111)
    vc<-A+B
        }
  
  else
if (Nstage==3 && Base==0) {
  DJ<-D[which((D$A1==ATS1[1] & D$O2==0 & D$A2==ATS1[2] &
                 D$O3==0 & D$A3==ATS1[4])|
                (D$A1==ATS1[1] & D$O2==0 & D$A2==ATS1[2] & D$O3==1 &
                   D$A3==ATS1[5])|
                (D$A1==ATS1[1] & D$O2==1 & D$A2==ATS1[3] & D$O3==0 &
                   D$A3==ATS1[6])|
                (D$A1==ATS1[1] & D$O2==1 & D$A2==ATS1[3] & D$O3==1 &
                   D$A3==ATS1[7])),]
  DK<-D[which((D$A1==ATS2[1] & D$O2==0 & D$A2==ATS2[2] &
                 D$O3==0 & D$A3==ATS2[4])|
                (D$A1==ATS2[1] & D$O2==0 & D$A2==ATS2[2] &
                   D$O3==1 & D$A3==ATS2[5])|
                (D$A1==ATS2[1] & D$O2==1 & D$A2==ATS2[3] &
                   D$O3==0 & D$A3==ATS2[6])|
                (D$A1==ATS2[1] & D$O2==1 & D$A2==ATS2[3] &
                   D$O3==1 & D$A3==ATS2[7])),]
      
      nJ<-length(D$Y[which(D$A1==ATS1[1])])
      if (nJ==0|is.na(nJ)) {nJ<-1}
      #sampe size to est Phat(O2)
      
  n0J<-length(D$Y[which(D$A1==ATS1[1] & D$O2==0 & D$A1==ATS1[2])])
  if (n0J==0|is.na(n0J)) {n0J<-1}
  #sample size est Phat(O3=0)
  #sample size of A1=d1
  n1J<-length(D$Y[which(D$A1==ATS1[1] & D$O2==1 & D$A1==ATS1[3])])
  if (n1J==0|is.na(n1J)) {n1J<-1}
  #sample size est Phat(O3==1)
      
  n00J<-length(DJ$Y[which(DJ$O2==0 & DJ$O3==0)])
  if (n00J==0|is.na(n00J)) {n00J<-1}
  #seq of (O1,O2)=(0,0) under dJ
  n01J<-length(DJ$Y[which(DJ$O2==0 & DJ$O3==1)])
  if (n01J==0|is.na(n01J)) {n01J<-1}
  #seq of (O1,O2)=(0,1) under dJ
  n10J<-length(DJ$Y[which(DJ$O2==1 & DJ$O3==0)])
  if (n10J==0|is.na(n10J)) {n10J<-1}
  #seq of (O1,O2)=(1,0) under dJ
  n11J<-length(DJ$Y[which(DJ$O2==1 & DJ$O3==1)])
  if (n11J==0|is.na(n11J)) {n11J<-1}
  #seq of (O1,O2)=(1,1) under dJ
      
  P0J<-length(D$Y[which(D$A1==ATS1[1] & D$O2==0)])/
    length(D$Y[which(D$A1==ATS1[1])])
  P1J<-1-P0J
      
  P00J<-length(D$Y[which(D$A1==ATS1[1] & D$O2==0 &
                           D$A2==ATS1[2] & D$O3==0)])/
    length(D$Y[which(D$A1==ATS1[1] & D$O2==0 &
                       D$A2==ATS1[2])])
  P01J<-1-P00J
  P10J<-length(D$Y[which(D$A1==ATS1[1] & D$O2==1 &
                           D$A2==ATS1[3] & D$O3==0)])/
    length(D$Y[which(D$A1==ATS1[1] & D$O2==1 &
                       D$A2==ATS1[3])])
  P11J<-1-P10J
      
      P00K<-length(D$Y[which(D$A1==ATS2[1] & D$O2==0 &
                               D$A2==ATS2[2] & D$O3==0)])/
        length(D$Y[which(D$A1==ATS2[1] & D$O2==0 &
                           D$A2==ATS2[2])])
      P01K<-1-P00K
      P10K<-length(D$Y[which(D$A1==ATS2[1] & D$O2==1 &
                               D$A2==ATS2[3] & D$O3==0)])/
        length(D$Y[which(D$A1==ATS1[1] & D$O2==1 &
                           D$A2==ATS2[3])])
      P11K<-1-P10K
      
      u00J<-mean(DJ$Y[which(DJ$O2==0 & DJ$O3==0)])
      if (is.na(u00J)) {u00J<-0}
      # within-sequence true mean (EY)
      u01J<-mean(DJ$Y[which(DJ$O2==0 & DJ$O3==1)])
      if (is.na(u01J)) {u01J<-0}
      u10J<-mean(DJ$Y[which(DJ$O2==1 & DJ$O3==0)])
      if (is.na(u10J)) {u10J<-0}
      u11J<-mean(DJ$Y[which(DJ$O2==1 & DJ$O3==1)])
      if (is.na(u11J)) {u11J<-0}
      
      u00K<-mean(DK$Y[which(DK$O2==0 & DK$O3==0)])
      if (is.na(u00K)) {u00K<-0}
      u01K<-mean(DK$Y[which(DK$O2==0 & DK$O3==1)])
      if (is.na(u01K)) {u01K<-0}
      u10K<-mean(DK$Y[which(DK$O2==1 & DK$O3==0)])
      if (is.na(u10K)) {u10K<-0}
      u11K<-mean(DK$Y[which(DK$O2==1 & DK$O3==1)])
      if (is.na(u11K)) {u11K<-0}
      
      v00J<-var(DJ$Y[which(DJ$O2==0 & DJ$O3==0)])
      if (is.na(v00J)) {v00J<-0}
      #within-sequence variance for cont outcome
      v01J<-var(DJ$Y[which(DJ$O2==0 & DJ$O3==1)])
      if (is.na(v01J)) {v01J<-0}
      v10J<-var(DJ$Y[which(DJ$O2==1 & DJ$O3==0)])
      if (is.na(v10J)) {v10J<-0}
      v11J<-var(DJ$Y[which(DJ$O2==1 & DJ$O3==1)])
      if (is.na(v11J)) {v11J<-0}
      
      bv00J<-u00J*(1-u00J)
      if (is.na(bv00J)) {bv00J<-0}
      #within-sequence var est for bin outcome
      bv01J<-u01J*(1-u01J)
      if (is.na(bv01J)) {bv01J<-0}
      bv10J<-u10J*(1-u10J)
      if (is.na(bv10J)) {bv10J<-0}
      bv11J<-u11J*(1-u11J)
      if (is.na(bv11J)) {bv11J<-0}
      comvar<-cv(data=D)
      if (is.na(comvar)) {comvar<-0}
      #pooled variance
      
      A0000<- (I[1]*P0J*(1-P0J)*P00J*P00K/nJ+I[2]*P0J*P0J*P00J*
                 (1-P00J)/n0J)*u00J*u00K
      A0001<- (I[1]*P0J*(1-P0J)*P00J*P01K/nJ-I[2]*P0J*P0J*P00J*
                 (1-P00J)/n0J)*u00J*u01K
      A0010<--(I[1]*P0J*(1-P0J)*P00J*P10K/nJ)*u00J*u10K
      A0011<--(I[1]*P0J*(1-P0J)*P00J*P11K/nJ)*u00J*u11K
      
      A0100<- (I[1]*P0J*(1-P0J)*P01J*P00K/nJ-I[2]*P0J*P0J*P01J*
                 (1-P01J)/n0J)*u01J*u00K
      A0101<- (I[1]*P0J*(1-P0J)*P01J*P01K/nJ+I[2]*P0J*P0J*P01J*
                 (1-P01J)/n0J)*u01J*u01K
      A0110<--(I[1]*P0J*(1-P0J)*P01J*P10K/nJ)*u01J*u10K
      A0111<--(I[1]*P0J*(1-P0J)*P01J*P11K/nJ)*u01J*u11K
      
      A1000<--(I[1]*P1J*(1-P1J)*P10J*P00K/nJ)*u10J*u00K
      A1001<--(I[1]*P1J*(1-P1J)*P10J*P01K/nJ)*u10J*u01K
      A1010<- (I[1]*P1J*(1-P1J)*P10J*P10K/nJ+I[3]*P1J*P1J*
                 P10J*(1-P10J)/n1J)*u10J*u10K
      A1011<- (I[1]*P1J*(1-P1J)*P10J*P11K/nJ-I[3]*P1J*P1J*
                 P10J*(1-P10J)/n1J)*u10J*u11K
      
      A1100<--(I[1]*P1J*(1-P1J)*P11J*P00K/nJ)*u11J*u00K
      A1101<--(I[1]*P1J*(1-P1J)*P11J*P01K/nJ)*u11J*u01K
      A1110<- (I[1]*P1J*(1-P1J)*P11J*P10K/nJ-I[3]*P1J*P1J*
                 P11J*(1-P11J)/n1J)*u11J*u10K
      A1111<- (I[1]*P1J*(1-P1J)*P11J*P11K/nJ+I[3]*P1J*P1J*
                 P11J*(1-P11J)/n1J)*u11J*u11K
      A<-sum(A0000,A0001,A0010,A0011,
             A0100,A0101,A0110,A0111,
             A1000,A1001,A1010,A1011,
             A1100,A1101,A1110,A1111)
      
      if (FA=="binomial"){
        if (I[1]==1 && I[2]==1 && I[4]==1) {
          B0000<-P0J*P0J*P00J*P00K*bv00J/n00J}
        else {B0000<-0}
        if (I[1]==1 && I[2]==1 && I[5]==1) {
          B0101<-P0J*P0J*P01J*P01K*bv01J/n01J}
        else {B0101<-0}
        if (I[1]==1 && I[3]==1 && I[6]==1) {
          B1010<-P1J*P1J*P10J*P10K*bv10J/n10J}
        else {B1010<-0}
        if (I[1]==1 && I[3]==1 && I[7]==1) {
          B1111<-P1J*P1J*P11J*P11K*bv11J/n11J}
        else {B1111<-0}
      }
      else if(FA=="gaussian"){
        if (C) {
          if (I[1]==1 && I[2]==1 && I[4]==1) {
            B0000<-P0J*P0J*P00J*P00K*comvar/n00J}
          else {B0000<-0}
          if (I[1]==1 && I[2]==1 && I[5]==1) {
            B0101<-P0J*P0J*P01J*P01K*comvar/n01J}
          else {B0101<-0}
          if (I[1]==1 && I[3]==1 && I[6]==1) {
            B1010<-P1J*P1J*P10J*P10K*comvar/n10J}
          else {B1010<-0}
          if (I[1]==1 && I[3]==1 && I[7]==1) {
            B1111<-P1J*P1J*P11J*P11K*comvar/n11J}
          else {B1111<-0}
        }
        else {
          if (I[1]==1 && I[2]==1 && I[4]==1) {
            B0000<-P0J*P0J*P00J*P00K*v00J/n00J}
          else {B0000<-0}
          if (I[1]==1 && I[2]==1 && I[5]==1) {
            B0101<-P0J*P0J*P01J*P01K*v01J/n01J}
          else {B0101<-0}
          if (I[1]==1 && I[3]==1 && I[6]==1) {
            B1010<-P1J*P1J*P10J*P10K*v10J/n10J}
          else {B1010<-0}
          if (I[1]==1 && I[3]==1 && I[7]==1) {
            B1111<-P1J*P1J*P11J*P11K*v11J/n11J}
          else {B1111<-0}
        }
      }
      else
        stop("invalid enter for argument 'family'")
      B<-sum(B0000,B0101,B1010,B1111)
      vc<-A+B
    } 

  return(vc)
}




