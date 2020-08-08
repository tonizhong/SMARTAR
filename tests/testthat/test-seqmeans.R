test_that("There is warning", {
  addTaskCallback(function(...) {set.seed(1);TRUE})
  #generate a pesudo SMART data
  N<-800                       # total sample size
  O1<-A1<-O2<-A2<-O3<-A3<-Y<-rep(NA,N)
  # first number stands for stage, second number stands for
  # baseline, third number stands for bino(0) or continuous(1)
  Dat_3_0 <- data.frame(A1,O2,A2,O3,A3,Y)
  Dat_3_1 <- data.frame(O1,A1,O2,A2,O3,A3,Y)
  
  ## for baseline=0
  # stage-1 treatment
  Dat_3_0$A1<-sample(c(0,1),size=N,prob=c(1,1),replace=TRUE)
  
  # intermediate outcome
  n0<-length(Dat_3_0$A1[which(Dat_3_0$A1==0)]); n1<-N-n0
  Dat_3_0$O2[which(Dat_3_0$A1==0)]<-rbinom(n=n0,size=1,p=0.6)
  Dat_3_0$O2[which(Dat_3_0$A1==1)]<-rbinom(n=n1,size=1,p=0.4)
  
  # stage-2 treatment
  n00<-nrow(Dat_3_0[which(Dat_3_0$A1==0 & Dat_3_0$O2==0),])
  n01<-n0-n00
  n10<-nrow(Dat_3_0[which(Dat_3_0$A1==1 & Dat_3_0$O2==0),])
  n11<-n1-n10
  Dat_3_0$A2[which(Dat_3_0$A1==0 & Dat_3_0$O2==0)]<-
    sample(c(0,1),size=n00,prob=c(1,1),replace=TRUE)
  Dat_3_0$A2[which(Dat_3_0$A1==0 & Dat_3_0$O2==1)]<-
    sample(c(0,1),size=n01,prob=c(1,1),replace=TRUE)
  Dat_3_0$A2[which(Dat_3_0$A1==1 & Dat_3_0$O2==0)]<-
    sample(c(0,1),size=n10,prob=c(1,1),replace=TRUE)
  Dat_3_0$A2[which(Dat_3_0$A1==1 & Dat_3_0$O2==1)]<-
    sample(c(0,1),size=n11,prob=c(1,1),replace=TRUE)
  
  n000<-nrow(Dat_3_0[which(Dat_3_0$A1==0 & Dat_3_0$O2==0 & Dat_3_0$A2==0),])
  n001<-n00-n000
  n010<-nrow(Dat_3_0[which(Dat_3_0$A1==0 & Dat_3_0$O2==1 & Dat_3_0$A2==0),])
  n011<-n01-n010
  n100<-nrow(Dat_3_0[which(Dat_3_0$A1==1 & Dat_3_0$O2==0 & Dat_3_0$A2==0),])
  n101<-n10-n100
  n110<-nrow(Dat_3_0[which(Dat_3_0$A1==1 & Dat_3_0$O2==1 & Dat_3_0$A2==0),])
  n111<-n11-n110
  
  Dat_3_0$O3[which(Dat_3_0$A1==0 & Dat_3_0$O2==0 &
                     Dat_3_0$A2==0) ]<-rbinom(n=n000,size=1,p=0.3)
  Dat_3_0$O3[which(Dat_3_0$A1==0 & Dat_3_0$O2==0 &
                     Dat_3_0$A2==1) ]<-rbinom(n=n001,size=1,p=0.4)
  Dat_3_0$O3[which(Dat_3_0$A1==0 & Dat_3_0$O2==1 &
                     Dat_3_0$A2==0) ]<-rbinom(n=n010,size=1,p=0.5)
  Dat_3_0$O3[which(Dat_3_0$A1==0 & Dat_3_0$O2==1 &
                     Dat_3_0$A2==1) ]<-rbinom(n=n011,size=1,p=0.6)
  Dat_3_0$O3[which(Dat_3_0$A1==1 & Dat_3_0$O2==0 &
                     Dat_3_0$A2==0) ]<-rbinom(n=n100,size=1,p=0.7)
  Dat_3_0$O3[which(Dat_3_0$A1==1 & Dat_3_0$O2==0 &
                     Dat_3_0$A2==1) ]<-rbinom(n=n101,size=1,p=0.5)
  Dat_3_0$O3[which(Dat_3_0$A1==1 & Dat_3_0$O2==1 &
                     Dat_3_0$A2==0) ]<-rbinom(n=n110,size=1,p=0.6)
  Dat_3_0$O3[which(Dat_3_0$A1==1 & Dat_3_0$O2==1 &
                     Dat_3_0$A2==1) ]<-rbinom(n=n111,size=1,p=0.7)
  
  n0000<-nrow(Dat_3_0[which(Dat_3_0$A1==0 & Dat_3_0$O2==0 &
                              Dat_3_0$A2==0 & Dat_3_0$O3==0),])
  n0001<-n000-n0000
  n0100<-nrow(Dat_3_0[which(Dat_3_0$A1==0 & Dat_3_0$O2==1 &
                              Dat_3_0$A2==0 & Dat_3_0$O3==0),])
  n0101<-n010-n0100
  n1000<-nrow(Dat_3_0[which(Dat_3_0$A1==1 & Dat_3_0$O2==0 &
                              Dat_3_0$A2==0 & Dat_3_0$O3==0),])
  n1001<-n100-n1000
  n1100<-nrow(Dat_3_0[which(Dat_3_0$A1==1 & Dat_3_0$O2==1 &
                              Dat_3_0$A2==0 & Dat_3_0$O3==0),])
  n1101<-n110-n1100
  n0010<-nrow(Dat_3_0[which(Dat_3_0$A1==0 & Dat_3_0$O2==0 &
                              Dat_3_0$A2==1 & Dat_3_0$O3==0),])
  n0011<-n001-n0010
  n0110<-nrow(Dat_3_0[which(Dat_3_0$A1==0 & Dat_3_0$O2==1 &
                              Dat_3_0$A2==1 & Dat_3_0$O3==0),])
  n0111<-n011-n0110
  n1010<-nrow(Dat_3_0[which(Dat_3_0$A1==1 & Dat_3_0$O2==0 &
                              Dat_3_0$A2==1 & Dat_3_0$O3==0),])
  n1011<-n101-n1010
  n1110<-nrow(Dat_3_0[which(Dat_3_0$A1==1 & Dat_3_0$O2==1 &
                              Dat_3_0$A2==1 & Dat_3_0$O3==0),])
  n1111<-n111-n1110
  
  Dat_3_0$A3[which(Dat_3_0$A1==0 & Dat_3_0$O2==0 &
                     Dat_3_0$A2==0 & Dat_3_0$O3==0)]<-
    0
  Dat_3_0$A3[which(Dat_3_0$A1==0 & Dat_3_0$O2==0 &
                     Dat_3_0$A2==0 & Dat_3_0$O3==1)]<-
    0
  Dat_3_0$A3[which(Dat_3_0$A1==0 & Dat_3_0$O2==1 &
                     Dat_3_0$A2==0 & Dat_3_0$O3==0)]<-
    1
  Dat_3_0$A3[which(Dat_3_0$A1==0 & Dat_3_0$O2==1 &
                     Dat_3_0$A2==0 & Dat_3_0$O3==1)]<-
    1
  Dat_3_0$A3[which(Dat_3_0$A1==1 & Dat_3_0$O2==0 &
                     Dat_3_0$A2==0 & Dat_3_0$O3==0)]<-
    0
  Dat_3_0$A3[which(Dat_3_0$A1==1 & Dat_3_0$O2==0 &
                     Dat_3_0$A2==0 & Dat_3_0$O3==1)]<-
    0
  Dat_3_0$A3[which(Dat_3_0$A1==1 & Dat_3_0$O2==1 &
                     Dat_3_0$A2==0 & Dat_3_0$O3==0)]<-
    1
  Dat_3_0$A3[which(Dat_3_0$A1==1 & Dat_3_0$O2==1 &
                     Dat_3_0$A2==0 & Dat_3_0$O3==1)]<-
    1
  Dat_3_0$A3[which(Dat_3_0$A1==0 & Dat_3_0$O2==0 &
                     Dat_3_0$A2==1 & Dat_3_0$O3==0)]<-
    0
  Dat_3_0$A3[which(Dat_3_0$A1==0 & Dat_3_0$O2==0 &
                     Dat_3_0$A2==1 & Dat_3_0$O3==1)]<-
    0
  Dat_3_0$A3[which(Dat_3_0$A1==0 & Dat_3_0$O2==1 &
                     Dat_3_0$A2==1 & Dat_3_0$O3==0)]<-
    1
  Dat_3_0$A3[which(Dat_3_0$A1==0 & Dat_3_0$O2==1 &
                     Dat_3_0$A2==1 & Dat_3_0$O3==1)]<-
    1
  Dat_3_0$A3[which(Dat_3_0$A1==1 & Dat_3_0$O2==0 &
                     Dat_3_0$A2==1 & Dat_3_0$O3==0)]<-
    0
  Dat_3_0$A3[which(Dat_3_0$A1==1 & Dat_3_0$O2==0 &
                     Dat_3_0$A2==1 & Dat_3_0$O3==1)]<-
    0
  Dat_3_0$A3[which(Dat_3_0$A1==1 & Dat_3_0$O2==1 &
                     Dat_3_0$A2==1 & Dat_3_0$O3==0)]<-
    1
  Dat_3_0$A3[which(Dat_3_0$A1==1 & Dat_3_0$O2==1 &
                     Dat_3_0$A2==1 & Dat_3_0$O3==1)]<-
    1
  # final primary outcome
  Dat_3_0$Y[which(Dat_3_0$A1==0 & Dat_3_0$O2==0 &
                    Dat_3_0$A2==0 & Dat_3_0$O3==0)]<-
    rnorm(n=n0000,mean = 7,sd=7)
  Dat_3_0$Y[which(Dat_3_0$A1==0 & Dat_3_0$O2==0 &
                    Dat_3_0$A2==0 & Dat_3_0$O3==1)]<-
    rnorm(n=n0001,mean = 6,sd=8)
  Dat_3_0$Y[which(Dat_3_0$A1==0 & Dat_3_0$O2==0 &
                    Dat_3_0$A2==1 & Dat_3_0$O3==0)]<-
    rnorm(n=n0010,mean = 6,sd=7)
  Dat_3_0$Y[which(Dat_3_0$A1==0 & Dat_3_0$O2==0 &
                    Dat_3_0$A2==1 & Dat_3_0$O3==1)]<-
    rnorm(n=n0011,mean = 5,sd=7)
  Dat_3_0$Y[which(Dat_3_0$A1==0 & Dat_3_0$O2==1 &
                    Dat_3_0$A2==0 & Dat_3_0$O3==0)]<-
    rnorm(n=n0100,mean = 7,sd=8)
  Dat_3_0$Y[which(Dat_3_0$A1==0 & Dat_3_0$O2==1 &
                    Dat_3_0$A2==1 & Dat_3_0$O3==0)]<-
    rnorm(n=n0110,mean = 5,sd=7)
  Dat_3_0$Y[which(Dat_3_0$A1==0 & Dat_3_0$O2==1 &
                    Dat_3_0$A2==1 & Dat_3_0$O3==1)]<-
    rnorm(n=n0111,mean = 5,sd=10)
  Dat_3_0$Y[which(Dat_3_0$A1==0 & Dat_3_0$O2==1 &
                    Dat_3_0$A2==0 & Dat_3_0$O3==1)]<-
    rnorm(n=n0101,mean = 6,sd=6)
  Dat_3_0$Y[which(Dat_3_0$A1==1 & Dat_3_0$O2==0 &
                    Dat_3_0$A2==0 & Dat_3_0$O3==0)]<-
    rnorm(n=n1000,mean = 6,sd=8)
  Dat_3_0$Y[which(Dat_3_0$A1==1 & Dat_3_0$O2==0 &
                    Dat_3_0$A2==0 & Dat_3_0$O3==1)]<-
    rnorm(n=n1001,mean = 7,sd=10)
  Dat_3_0$Y[which(Dat_3_0$A1==1 & Dat_3_0$O2==0 &
                    Dat_3_0$A2==1 & Dat_3_0$O3==0)]<-
    rnorm(n=n1010,mean = 7,sd=8)
  Dat_3_0$Y[which(Dat_3_0$A1==1 & Dat_3_0$O2==0 &
                    Dat_3_0$A2==1 & Dat_3_0$O3==1)]<-
    rnorm(n=n1011,mean = 4,sd=8)
  Dat_3_0$Y[which(Dat_3_0$A1==1 & Dat_3_0$O2==1 &
                    Dat_3_0$A2==0 & Dat_3_0$O3==0)]<-
    rnorm(n=n1100,mean = 4,sd=7)
  Dat_3_0$Y[which(Dat_3_0$A1==1 & Dat_3_0$O2==1 &
                    Dat_3_0$A2==0 & Dat_3_0$O3==1)]<-
    rnorm(n=n1101,mean = 5,sd=9)
  Dat_3_0$Y[which(Dat_3_0$A1==1 & Dat_3_0$O2==1 &
                    Dat_3_0$A2==1 & Dat_3_0$O3==0)]<-
    rnorm(n=n1110,mean = 10,sd=8)
  Dat_3_0$Y[which(Dat_3_0$A1==1 & Dat_3_0$O2==1 &
                    Dat_3_0$A2==1 & Dat_3_0$O3==1)]<-
    rnorm(n=n1111,mean = 10,sd=7)
  
  Dat_1_0_0 <- as.data.frame(cbind(Dat_3_0$A1,rbinom(n=N,size=1,p=0.5)))
  colnames(Dat_1_0_0)<-c("A1","Y")
  
  Dat_1_0_1 <- as.data.frame(cbind(Dat_3_0$A1,rnorm(n=N,mean=7,sd=7)))
  colnames(Dat_1_0_1)<-c("A1","Y")
  
  Dat_2_0_0 <- as.data.frame(cbind(Dat_3_0$A1,Dat_3_0$O2,
                                   Dat_3_0$A2,
                                   rbinom(n=N,size=1,p=0.5)))
  colnames(Dat_2_0_0)<-c("A1","O2","A2","Y")
  
  Dat_2_0_0$Y[which(Dat_2_0_0$A1==0 & Dat_2_0_0$O2==0 & Dat_2_0_0$A2==0)]<-
    rbinom(n=n000,size=1,p=0.5)
  Dat_2_0_0$Y[which(Dat_2_0_0$A1==0 & Dat_2_0_0$O2==0 & Dat_2_0_0$A2==1)]<-
    rbinom(n=n001,size=1,p=0.3)
  Dat_2_0_0$Y[which(Dat_2_0_0$A1==0 & Dat_2_0_0$O2==1 & Dat_2_0_0$A2==0)]<-
    rbinom(n=n010,size=1,p=0.1)
  Dat_2_0_0$Y[which(Dat_2_0_0$A1==1 & Dat_2_0_0$O2==0 & Dat_2_0_0$A2==0)]<-
    rbinom(n=n100,size=1,p=0.7)
  Dat_2_0_0$Y[which(Dat_2_0_0$A1==0 & Dat_2_0_0$O2==1 & Dat_2_0_0$A2==1)]<-
    rbinom(n=n011,size=1,p=0.2)
  Dat_2_0_0$Y[which(Dat_2_0_0$A1==1 & Dat_2_0_0$O2==0 & Dat_2_0_0$A2==1)]<-
    rbinom(n=n101,size=1,p=0.4)
  Dat_2_0_0$Y[which(Dat_2_0_0$A1==1 & Dat_2_0_0$O2==1 & Dat_2_0_0$A2==0)]<-
    rbinom(n=n110,size=1,p=0.6)
  Dat_2_0_0$Y[which(Dat_2_0_0$A1==1 & Dat_2_0_0$O2==1 & Dat_2_0_0$A2==1)]<-
    rbinom(n=n111,size=1,p=0.8)
  
  Dat_2_0_1 <- as.data.frame(cbind(Dat_3_0$A1,Dat_3_0$O2,
                                   Dat_3_0$A2,
                                   rnorm(n=N,mean=7,sd=7)))
  colnames(Dat_2_0_1)<-c("A1","O2","A2","Y")
  Dat_2_0_1$Y[which(Dat_2_0_1$A1==0 & Dat_2_0_1$O2==0 & Dat_2_0_1$A2==0)]<-
    rnorm(n=n000,mean=2,sd=8)
  Dat_2_0_1$Y[which(Dat_2_0_1$A1==0 & Dat_2_0_1$O2==0 & Dat_2_0_1$A2==1)]<-
    rnorm(n=n001,mean=4,sd=7)
  Dat_2_0_1$Y[which(Dat_2_0_1$A1==0 & Dat_2_0_1$O2==1 & Dat_2_0_1$A2==0)]<-
    rnorm(n=n010,mean=5,sd=8)
  Dat_2_0_1$Y[which(Dat_2_0_1$A1==1 & Dat_2_0_1$O2==0 & Dat_2_0_1$A2==0)]<-
    rnorm(n=n100,mean=7,sd=9)
  Dat_2_0_1$Y[which(Dat_2_0_1$A1==0 & Dat_2_0_1$O2==1 & Dat_2_0_1$A2==1)]<-
    rnorm(n=n011,mean=9,sd=7)
  Dat_2_0_1$Y[which(Dat_2_0_1$A1==1 & Dat_2_0_1$O2==0 & Dat_2_0_1$A2==1)]<-
    rnorm(n=n101,mean=11,sd=8)
  Dat_2_0_1$Y[which(Dat_2_0_1$A1==1 & Dat_2_0_1$O2==1 & Dat_2_0_1$A2==0)]<-
    rnorm(n=n110,mean=6,sd=9)
  Dat_2_0_1$Y[which(Dat_2_0_1$A1==1 & Dat_2_0_1$O2==1 & Dat_2_0_1$A2==1)]<-
    rnorm(n=n111,mean=8,sd=10)
  
  Dat_3_0_0 <- as.data.frame(cbind(Dat_3_0$A1,Dat_3_0$O2,
                                   Dat_3_0$A2,Dat_3_0$O3,Dat_3_0$A3,
                                   rbinom(n=N,size=1,p=0.7)))
  colnames(Dat_3_0_0)<-c("A1","O2","A2","O3","A3","Y")
  Dat_3_0_0$Y[which(Dat_3_0$A1==0 & Dat_3_0$O2==0 &
                      Dat_3_0$A2==0 & Dat_3_0$O3==0)]<-
    rbinom(n=n0000,size=1,p=0.1)
  Dat_3_0_0$Y[which(Dat_3_0$A1==0 & Dat_3_0$O2==0 &
                      Dat_3_0$A2==0 & Dat_3_0$O3==1)]<-
    rbinom(n=n0001,size=1,p=0.2)
  Dat_3_0_0$Y[which(Dat_3_0$A1==0 & Dat_3_0$O2==0 &
                      Dat_3_0$A2==1 & Dat_3_0$O3==0)]<-
    rbinom(n=n0010,size=1,p=0.3)
  Dat_3_0_0$Y[which(Dat_3_0$A1==0 & Dat_3_0$O2==0 &
                      Dat_3_0$A2==1 & Dat_3_0$O3==1)]<-
    rbinom(n=n0011,size=1,p=0.4)
  Dat_3_0_0$Y[which(Dat_3_0$A1==0 & Dat_3_0$O2==1 &
                      Dat_3_0$A2==0 & Dat_3_0$O3==0)]<-
    rbinom(n=n0100,size=1,p=0.5)
  Dat_3_0_0$Y[which(Dat_3_0$A1==0 & Dat_3_0$O2==1 &
                      Dat_3_0$A2==1 & Dat_3_0$O3==0)]<-
    rbinom(n=n0110,size=1,p=0.6)
  Dat_3_0_0$Y[which(Dat_3_0$A1==0 & Dat_3_0$O2==1 &
                      Dat_3_0$A2==1 & Dat_3_0$O3==1)]<-
    rbinom(n=n0111,size=1,p=0.7)
  Dat_3_0_0$Y[which(Dat_3_0$A1==0 & Dat_3_0$O2==1 &
                      Dat_3_0$A2==0 & Dat_3_0$O3==1)]<-
    rbinom(n=n0101,size=1,p=0.8)
  Dat_3_0_0$Y[which(Dat_3_0$A1==1 & Dat_3_0$O2==0 &
                      Dat_3_0$A2==0 & Dat_3_0$O3==0)]<-
    rbinom(n=n1000,size=1,p=0.9)
  Dat_3_0_0$Y[which(Dat_3_0$A1==1 & Dat_3_0$O2==0 &
                      Dat_3_0$A2==0 & Dat_3_0$O3==1)]<-
    rbinom(n=n1001,size=1,p=0.97)
  Dat_3_0_0$Y[which(Dat_3_0$A1==1 & Dat_3_0$O2==0 &
                      Dat_3_0$A2==1 & Dat_3_0$O3==0)]<-
    rbinom(n=n1010,size=1,p=0.87)
  Dat_3_0_0$Y[which(Dat_3_0$A1==1 & Dat_3_0$O2==0 &
                      Dat_3_0$A2==1 & Dat_3_0$O3==1)]<-
    rbinom(n=n1011,size=1,p=0.77)
  Dat_3_0_0$Y[which(Dat_3_0$A1==1 & Dat_3_0$O2==1 &
                      Dat_3_0$A2==0 & Dat_3_0$O3==0)]<-
    rbinom(n=n1100,size=1,p=0.67)
  Dat_3_0_0$Y[which(Dat_3_0$A1==1 & Dat_3_0$O2==1 &
                      Dat_3_0$A2==0 & Dat_3_0$O3==1)]<-
    rbinom(n=n1101,size=1,p=0.57)
  Dat_3_0_0$Y[which(Dat_3_0$A1==1 & Dat_3_0$O2==1 &
                      Dat_3_0$A2==1 & Dat_3_0$O3==0)]<-
    rbinom(n=n1110,size=1,p=0.47)
  Dat_3_0_0$Y[which(Dat_3_0$A1==1 & Dat_3_0$O2==1 &
                      Dat_3_0$A2==1 & Dat_3_0$O3==1)]<-
    rbinom(n=n1111,size=1,p=0.37)
  
  Dat_3_0_1 <- as.data.frame(cbind(Dat_3_0$A1,Dat_3_0$O2,
                                   Dat_3_0$A2,Dat_3_0$O3,Dat_3_0$A3,
                                   Dat_3_0$Y))
  colnames(Dat_3_0_1)<-c("A1","O2","A2","O3","A3","Y")
  
  
  
  ## for baseline=1
  # stage-1 outcome
  Dat_3_1$O1<-rbinom(n=N,size=1,p=0.3)
  
  # stage-1 treatment
  n0<-length(Dat_3_1$O1[which(Dat_3_1$O1==0)]); n1<-N-n0
  Dat_3_1$A1[which(Dat_3_1$O1==0)]<-sample(c(0,1),
                                           size=n0,prob=c(1,1),replace=TRUE)
  Dat_3_1$A1[which(Dat_3_1$O1==1)]<-sample(c(0,1),
                                           size=n1,prob=c(1,1),replace=TRUE)
  
  # stage-2 outcome
  n00<-nrow(Dat_3_1[which(Dat_3_1$O1==0 & Dat_3_1$A1==0),])
  n01<-n0-n00
  n10<-nrow(Dat_3_1[which(Dat_3_1$O1==1 & Dat_3_1$A1==0),])
  n11<-n1-n10
  Dat_3_1$O2[which(Dat_3_1$O1==0 & Dat_3_1$A1==0)]<-
    rbinom(n=n00,size=1,p=0.3)
  Dat_3_1$O2[which(Dat_3_1$O1==0 & Dat_3_1$A1==1)]<-
    rbinom(n=n01,size=1,p=0.4)
  Dat_3_1$O2[which(Dat_3_1$O1==1 & Dat_3_1$A1==0)]<-
    rbinom(n=n10,size=1,p=0.5)
  Dat_3_1$O2[which(Dat_3_1$O1==1 & Dat_3_1$A1==1)]<-
    rbinom(n=n11,size=1,p=0.6)
  
  n000<-nrow(Dat_3_1[which(Dat_3_1$O1==0 & Dat_3_1$A1==0 &
                             Dat_3_1$O2==0),])
  n001<-n00-n000
  n010<-nrow(Dat_3_1[which(Dat_3_1$O1==0 & Dat_3_1$A1==1 &
                             Dat_3_1$O2==0),])
  n011<-n01-n010
  n100<-nrow(Dat_3_1[which(Dat_3_1$O1==1 & Dat_3_1$A1==0 &
                             Dat_3_1$O2==0),])
  n101<-n10-n100
  n110<-nrow(Dat_3_1[which(Dat_3_1$O1==1 & Dat_3_1$A1==1 &
                             Dat_3_1$O2==0),])
  n111<-n11-n110
  
  Dat_3_1$A2[which(Dat_3_1$O1==0 & Dat_3_1$A1==0 &
                     Dat_3_1$O2==0) ]<-
    sample(c(0,1), size=n000,prob=c(1,1),replace=TRUE)
  
  Dat_3_1$A2[which(Dat_3_1$O1==0 & Dat_3_1$A1==0 &
                     Dat_3_1$O2==1) ]<-
    sample(c(0,1), size=n001,prob=c(1,1),replace=TRUE)
  
  Dat_3_1$A2[which(Dat_3_1$O1==0 & Dat_3_1$A1==1 &
                     Dat_3_1$O2==0) ]<-
    sample(c(0,1), size=n010,prob=c(1,1),replace=TRUE)
  
  Dat_3_1$A2[which(Dat_3_1$O1==0 & Dat_3_1$A1==1 &
                     Dat_3_1$O2==1) ]<-
    sample(c(0,1), size=n011,prob=c(1,1),replace=TRUE)
  
  Dat_3_1$A2[which(Dat_3_1$O1==1 & Dat_3_1$A1==0 &
                     Dat_3_1$O2==0) ]<-
    sample(c(0,1), size=n100,prob=c(1,1),replace=TRUE)
  
  Dat_3_1$A2[which(Dat_3_1$O1==1 & Dat_3_1$A1==0 &
                     Dat_3_1$O2==1) ]<-
    sample(c(0,1), size=n101,prob=c(1,1),replace=TRUE)
  
  Dat_3_1$A2[which(Dat_3_1$O1==1 & Dat_3_1$A1==1 &
                     Dat_3_1$O2==0) ]<-
    sample(c(0,1), size=n110,prob=c(1,1),replace=TRUE)
  
  Dat_3_1$A2[which(Dat_3_1$O1==1 & Dat_3_1$A1==1 &
                     Dat_3_1$O2==1) ]<-
    sample(c(0,1), size=n111,prob=c(1,1),replace=TRUE)
  
  n0000<-nrow(Dat_3_1[which(Dat_3_1$O1==0 & Dat_3_1$A1==0 &
                              Dat_3_1$O2==0 & Dat_3_1$A2==0),])
  n0001<-n000-n0000
  n0010<-nrow(Dat_3_1[which(Dat_3_1$O1==0 & Dat_3_1$A1==0 &
                              Dat_3_1$O2==1 & Dat_3_1$A2==0),])
  n0011<-n001-n0010
  n0100<-nrow(Dat_3_1[which(Dat_3_1$O1==0 & Dat_3_1$A1==1 &
                              Dat_3_1$O2==0 & Dat_3_1$A2==0),])
  n0101<-n010-n0100
  n0110<-nrow(Dat_3_1[which(Dat_3_1$O1==0 & Dat_3_1$A1==1 &
                              Dat_3_1$O2==1 & Dat_3_1$A2==0),])
  n0111<-n011-n0110
  n1000<-nrow(Dat_3_1[which(Dat_3_1$O1==1 & Dat_3_1$A1==0 &
                              Dat_3_1$O2==0 & Dat_3_1$A2==0),])
  n1001<-n100-n1000
  n1010<-nrow(Dat_3_1[which(Dat_3_1$O1==1 & Dat_3_1$A1==0 &
                              Dat_3_1$O2==1 & Dat_3_1$A2==0),])
  n1011<-n101-n1010
  n1100<-nrow(Dat_3_1[which(Dat_3_1$O1==1 & Dat_3_1$A1==1 &
                              Dat_3_1$O2==0 & Dat_3_1$A2==0),])
  n1101<-n110-n1100
  n1110<-nrow(Dat_3_1[which(Dat_3_1$O1==1 & Dat_3_1$A1==1 &
                              Dat_3_1$O2==1 & Dat_3_1$A2==0),])
  n1111<-n111-n1110
  
  
  Dat_3_1$O3[which(Dat_3_1$O1==0 & Dat_3_1$A1==0 &
                     Dat_3_1$O2==0 & Dat_3_1$A2==1)]<-
    rbinom(n=n0001,size=1,p=0.3)
  Dat_3_1$O3[which(Dat_3_1$O1==0 & Dat_3_1$A1==0 &
                     Dat_3_1$O2==0 & Dat_3_1$A2==0)]<-
    rbinom(n=n0000,size=1,p=0.4)
  Dat_3_1$O3[which(Dat_3_1$O1==0 & Dat_3_1$A1==0 &
                     Dat_3_1$O2==1 & Dat_3_1$A2==1)]<-
    rbinom(n=n0011,size=1,p=0.4)
  Dat_3_1$O3[which(Dat_3_1$O1==0 & Dat_3_1$A1==0 &
                     Dat_3_1$O2==1 & Dat_3_1$A2==0)]<-
    rbinom(n=n0010,size=1,p=0.5)
  Dat_3_1$O3[which(Dat_3_1$O1==0 & Dat_3_1$A1==1 &
                     Dat_3_1$O2==0 & Dat_3_1$A2==1)]<-
    rbinom(n=n0101,size=1,p=0.5)
  Dat_3_1$O3[which(Dat_3_1$O1==0 & Dat_3_1$A1==1 &
                     Dat_3_1$O2==0 & Dat_3_1$A2==0)]<-
    rbinom(n=n0100,size=1,p=0.6)
  Dat_3_1$O3[which(Dat_3_1$O1==0 & Dat_3_1$A1==1 &
                     Dat_3_1$O2==1 & Dat_3_1$A2==1)]<-
    rbinom(n=n0111,size=1,p=0.6)
  Dat_3_1$O3[which(Dat_3_1$O1==0 & Dat_3_1$A1==1 &
                     Dat_3_1$O2==1 & Dat_3_1$A2==0)]<-
    rbinom(n=n0110,size=1,p=0.7)
  Dat_3_1$O3[which(Dat_3_1$O1==1 & Dat_3_1$A1==0 &
                     Dat_3_1$O2==0 & Dat_3_1$A2==0)]<-
    rbinom(n=n1000,size=1,p=0.3)
  Dat_3_1$O3[which(Dat_3_1$O1==1 & Dat_3_1$A1==0 &
                     Dat_3_1$O2==0 & Dat_3_1$A2==1)]<-
    rbinom(n=n1001,size=1,p=0.4)
  Dat_3_1$O3[which(Dat_3_1$O1==1 & Dat_3_1$A1==0 &
                     Dat_3_1$O2==1 & Dat_3_1$A2==0)]<-
    rbinom(n=n1010,size=1,p=0.4)
  Dat_3_1$O3[which(Dat_3_1$O1==1 & Dat_3_1$A1==0 &
                     Dat_3_1$O2==1 & Dat_3_1$A2==1)]<-
    rbinom(n=n1011,size=1,p=0.5)
  Dat_3_1$O3[which(Dat_3_1$O1==1 & Dat_3_1$A1==1 &
                     Dat_3_1$O2==0 & Dat_3_1$A2==0)]<-
    rbinom(n=n1100,size=1,p=0.5)
  Dat_3_1$O3[which(Dat_3_1$O1==1 & Dat_3_1$A1==1 &
                     Dat_3_1$O2==0 & Dat_3_1$A2==1)]<-
    rbinom(n=n1101,size=1,p=0.7)
  Dat_3_1$O3[which(Dat_3_1$O1==1 & Dat_3_1$A1==1 &
                     Dat_3_1$O2==1 & Dat_3_1$A2==0)]<-
    rbinom(n=n1110,size=1,p=0.6)
  Dat_3_1$O3[which(Dat_3_1$O1==1 & Dat_3_1$A1==1 &
                     Dat_3_1$O2==1 & Dat_3_1$A2==1)]<-
    rbinom(n=n1111,size=1,p=0.6)
  
  Dat_3_1$A3[which(Dat_3_1$O1==0 & Dat_3_1$A1==0 & Dat_3_1$O2==0 &
                     Dat_3_1$A2==1 & Dat_3_1$O3==0) ]<-1
  Dat_3_1$A3[which(Dat_3_1$O1==0 & Dat_3_1$A1==0 & Dat_3_1$O2==0 &
                     Dat_3_1$A2==1 & Dat_3_1$O3==1) ]<-1
  
  Dat_3_1$A3[which(Dat_3_1$O1==0 & Dat_3_1$A1==0 & Dat_3_1$O2==0 &
                     Dat_3_1$A2==0 & Dat_3_1$O3==0) ]<-1
  Dat_3_1$A3[which(Dat_3_1$O1==0 & Dat_3_1$A1==0 & Dat_3_1$O2==0 &
                     Dat_3_1$A2==0 & Dat_3_1$O3==1) ]<-1
  
  Dat_3_1$A3[which(Dat_3_1$O1==0 & Dat_3_1$A1==0 & Dat_3_1$O2==1 &
                     Dat_3_1$A2==1 & Dat_3_1$O3==0) ]<-1
  Dat_3_1$A3[which(Dat_3_1$O1==0 & Dat_3_1$A1==0 & Dat_3_1$O2==1 &
                     Dat_3_1$A2==1 & Dat_3_1$O3==1) ]<-1
  
  Dat_3_1$A3[which(Dat_3_1$O1==0 & Dat_3_1$A1==0 & Dat_3_1$O2==1 &
                     Dat_3_1$A2==0 & Dat_3_1$O3==0) ]<-1
  Dat_3_1$A3[which(Dat_3_1$O1==0 & Dat_3_1$A1==0 & Dat_3_1$O2==1 &
                     Dat_3_1$A2==0 & Dat_3_1$O3==1) ]<-1
  
  Dat_3_1$A3[which(Dat_3_1$O1==0 & Dat_3_1$A1==1 & Dat_3_1$O2==0 &
                     Dat_3_1$A2==1 & Dat_3_1$O3==0) ]<-1
  Dat_3_1$A3[which(Dat_3_1$O1==0 & Dat_3_1$A1==1 & Dat_3_1$O2==0 &
                     Dat_3_1$A2==1 & Dat_3_1$O3==1) ]<-1
  
  Dat_3_1$A3[which(Dat_3_1$O1==0 & Dat_3_1$A1==1 & Dat_3_1$O2==0 &
                     Dat_3_1$A2==0 & Dat_3_1$O3==0) ]<-1
  Dat_3_1$A3[which(Dat_3_1$O1==0 & Dat_3_1$A1==1 & Dat_3_1$O2==0 &
                     Dat_3_1$A2==0 & Dat_3_1$O3==1) ]<-1
  
  Dat_3_1$A3[which(Dat_3_1$O1==0 & Dat_3_1$A1==1 & Dat_3_1$O2==1 &
                     Dat_3_1$A2==1 & Dat_3_1$O3==0) ]<-1
  Dat_3_1$A3[which(Dat_3_1$O1==0 & Dat_3_1$A1==1 & Dat_3_1$O2==1 &
                     Dat_3_1$A2==1 & Dat_3_1$O3==1) ]<-1
  
  Dat_3_1$A3[which(Dat_3_1$O1==0 & Dat_3_1$A1==1 & Dat_3_1$O2==1 &
                     Dat_3_1$A2==0 & Dat_3_1$O3==0) ]<-1
  Dat_3_1$A3[which(Dat_3_1$O1==0 & Dat_3_1$A1==1 & Dat_3_1$O2==1 &
                     Dat_3_1$A2==0 & Dat_3_1$O3==1) ]<-1
  
  Dat_3_1$A3[which(Dat_3_1$O1==1 & Dat_3_1$A1==0 & Dat_3_1$O2==0 &
                     Dat_3_1$A2==0 & Dat_3_1$O3==0) ]<-0
  Dat_3_1$A3[which(Dat_3_1$O1==1 & Dat_3_1$A1==0 & Dat_3_1$O2==0 &
                     Dat_3_1$A2==0 & Dat_3_1$O3==1) ]<-0
  
  Dat_3_1$A3[which(Dat_3_1$O1==1 & Dat_3_1$A1==0 & Dat_3_1$O2==0 &
                     Dat_3_1$A2==1 & Dat_3_1$O3==0) ]<-0
  Dat_3_1$A3[which(Dat_3_1$O1==1 & Dat_3_1$A1==0 & Dat_3_1$O2==0 &
                     Dat_3_1$A2==1 & Dat_3_1$O3==1) ]<-0
  
  Dat_3_1$A3[which(Dat_3_1$O1==1 & Dat_3_1$A1==0 & Dat_3_1$O2==1 &
                     Dat_3_1$A2==0 & Dat_3_1$O3==0) ]<-0
  Dat_3_1$A3[which(Dat_3_1$O1==1 & Dat_3_1$A1==0 & Dat_3_1$O2==1 &
                     Dat_3_1$A2==0 & Dat_3_1$O3==1) ]<-0
  
  Dat_3_1$A3[which(Dat_3_1$O1==1 & Dat_3_1$A1==0 & Dat_3_1$O2==1 &
                     Dat_3_1$A2==1 & Dat_3_1$O3==0) ]<-0
  Dat_3_1$A3[which(Dat_3_1$O1==1 & Dat_3_1$A1==0 & Dat_3_1$O2==1 &
                     Dat_3_1$A2==1 & Dat_3_1$O3==1) ]<-0
  
  Dat_3_1$A3[which(Dat_3_1$O1==1 & Dat_3_1$A1==1 & Dat_3_1$O2==0 &
                     Dat_3_1$A2==0 & Dat_3_1$O3==0) ]<-0
  Dat_3_1$A3[which(Dat_3_1$O1==1 & Dat_3_1$A1==1 & Dat_3_1$O2==0 &
                     Dat_3_1$A2==0 & Dat_3_1$O3==1) ]<-0
  
  Dat_3_1$A3[which(Dat_3_1$O1==1 & Dat_3_1$A1==1 & Dat_3_1$O2==0 &
                     Dat_3_1$A2==1 & Dat_3_1$O3==0) ]<-0
  Dat_3_1$A3[which(Dat_3_1$O1==1 & Dat_3_1$A1==1 & Dat_3_1$O2==0 &
                     Dat_3_1$A2==1 & Dat_3_1$O3==1) ]<-0
  
  Dat_3_1$A3[which(Dat_3_1$O1==1 & Dat_3_1$A1==1 & Dat_3_1$O2==1 &
                     Dat_3_1$A2==0 & Dat_3_1$O3==0) ]<-0
  Dat_3_1$A3[which(Dat_3_1$O1==1 & Dat_3_1$A1==1 & Dat_3_1$O2==1 &
                     Dat_3_1$A2==0 & Dat_3_1$O3==1) ]<-0
  
  Dat_3_1$A3[which(Dat_3_1$O1==1 & Dat_3_1$A1==1 & Dat_3_1$O2==1 &
                     Dat_3_1$A2==1 & Dat_3_1$O3==0) ]<-0
  Dat_3_1$A3[which(Dat_3_1$O1==1 & Dat_3_1$A1==1 & Dat_3_1$O2==1 &
                     Dat_3_1$A2==1 & Dat_3_1$O3==1) ]<-0
  
  n00000<-nrow(Dat_3_1[which(Dat_3_1$O1==0 & Dat_3_1$A1==0 &
                               Dat_3_1$O2==0 & Dat_3_1$A2==0 &
                               Dat_3_1$O3==0),])
  n00001<-n0000-n00000
  n00010<-nrow(Dat_3_1[which(Dat_3_1$O1==0 & Dat_3_1$A1==0 &
                               Dat_3_1$O2==0 & Dat_3_1$A2==1 &
                               Dat_3_1$O3==0),])
  n00011<-n0001-n00010
  n00100<-nrow(Dat_3_1[which(Dat_3_1$O1==0 & Dat_3_1$A1==0 &
                               Dat_3_1$O2==1 & Dat_3_1$A2==0 &
                               Dat_3_1$O3==0),])
  n00101<-n0010-n00100
  n01000<-nrow(Dat_3_1[which(Dat_3_1$O1==0 & Dat_3_1$A1==1 &
                               Dat_3_1$O2==0 & Dat_3_1$A2==0 &
                               Dat_3_1$O3==0),])
  n01001<-n0100-n01000
  n10000<-nrow(Dat_3_1[which(Dat_3_1$O1==1 & Dat_3_1$A1==0 &
                               Dat_3_1$O2==0 & Dat_3_1$A2==0 &
                               Dat_3_1$O3==0),])
  n10001<-n1000-n10000
  n00110<-nrow(Dat_3_1[which(Dat_3_1$O1==0 & Dat_3_1$A1==0 &
                               Dat_3_1$O2==1 & Dat_3_1$A2==1 &
                               Dat_3_1$O3==0),])
  n00111<-n0011-n00110
  n01010<-nrow(Dat_3_1[which(Dat_3_1$O1==0 & Dat_3_1$A1==1 &
                               Dat_3_1$O2==0 & Dat_3_1$A2==1 &
                               Dat_3_1$O3==0),])
  n01011<-n0101-n01010
  n10010<-nrow(Dat_3_1[which(Dat_3_1$O1==1 & Dat_3_1$A1==0 &
                               Dat_3_1$O2==0 & Dat_3_1$A2==1 &
                               Dat_3_1$O3==0),])
  n10011<-n1001-n10010
  n01100<-nrow(Dat_3_1[which(Dat_3_1$O1==0 & Dat_3_1$A1==1 &
                               Dat_3_1$O2==1 & Dat_3_1$A2==0 &
                               Dat_3_1$O3==0),])
  n01101<-n0110-n01100
  n10100<-nrow(Dat_3_1[which(Dat_3_1$O1==1 & Dat_3_1$A1==0 &
                               Dat_3_1$O2==1 & Dat_3_1$A2==0 &
                               Dat_3_1$O3==0),])
  n10101<-n1010-n10100
  n11000<-nrow(Dat_3_1[which(Dat_3_1$O1==1 & Dat_3_1$A1==1 &
                               Dat_3_1$O2==0 & Dat_3_1$A2==0 &
                               Dat_3_1$O3==0),])
  n11001<-n1100-n11000
  n01110<-nrow(Dat_3_1[which(Dat_3_1$O1==0 & Dat_3_1$A1==1 &
                               Dat_3_1$O2==1 & Dat_3_1$A2==1 &
                               Dat_3_1$O3==0),])
  n01111<-n0111-n01110
  n10110<-nrow(Dat_3_1[which(Dat_3_1$O1==1 & Dat_3_1$A1==0 &
                               Dat_3_1$O2==1 & Dat_3_1$A2==1 &
                               Dat_3_1$O3==0),])
  n10111<-n1011-n10110
  n11010<-nrow(Dat_3_1[which(Dat_3_1$O1==1 & Dat_3_1$A1==1 &
                               Dat_3_1$O2==0 & Dat_3_1$A2==1 &
                               Dat_3_1$O3==0),])
  n11011<-n1101-n11010
  n11100<-nrow(Dat_3_1[which(Dat_3_1$O1==1 & Dat_3_1$A1==1 &
                               Dat_3_1$O2==1 & Dat_3_1$A2==0 &
                               Dat_3_1$O3==0),])
  n11101<-n1110-n11100
  n11110<-nrow(Dat_3_1[which(Dat_3_1$O1==1 & Dat_3_1$A1==1 &
                               Dat_3_1$O2==1 & Dat_3_1$A2==1 &
                               Dat_3_1$O3==0),])
  n11111<-n1111-n11110
  
  # final primary outcome
  Dat_3_1$Y[which(Dat_3_1$O1==0 & Dat_3_1$A1==0 & Dat_3_1$O2==0 &
                    Dat_3_1$A2==0 & Dat_3_1$O3==0)]<-
    rnorm(n=n00000,mean = 4,sd=8)
  Dat_3_1$Y[which(Dat_3_1$O1==0 & Dat_3_1$A1==0 & Dat_3_1$O2==0 &
                    Dat_3_1$A2==0 & Dat_3_1$O3==1)]<-
    rnorm(n=n00001,mean = 7,sd=9)
  Dat_3_1$Y[which(Dat_3_1$O1==0 & Dat_3_1$A1==0 & Dat_3_1$O2==0 &
                    Dat_3_1$A2==1 & Dat_3_1$O3==0)]<-
    rnorm(n=n00010,mean = 1,sd=8)
  Dat_3_1$Y[which(Dat_3_1$O1==0 & Dat_3_1$A1==0 & Dat_3_1$O2==1 &
                    Dat_3_1$A2==0 & Dat_3_1$O3==0)]<-
    rnorm(n=n00100,mean = 8,sd=9)
  Dat_3_1$Y[which(Dat_3_1$O1==0 & Dat_3_1$A1==1 & Dat_3_1$O2==0 &
                    Dat_3_1$A2==0 & Dat_3_1$O3==0)]<-
    rnorm(n=n01000,mean = 9,sd=8)
  Dat_3_1$Y[which(Dat_3_1$O1==1 & Dat_3_1$A1==0 & Dat_3_1$O2==0 &
                    Dat_3_1$A2==0 & Dat_3_1$O3==0)]<-
    rnorm(n=n10000,mean = 5,sd=9)
  Dat_3_1$Y[which(Dat_3_1$O1==0 & Dat_3_1$A1==0 & Dat_3_1$O2==0 &
                    Dat_3_1$A2==1 & Dat_3_1$O3==1)]<-
    rnorm(n=n00011,mean = 5,sd=9)
  
  Dat_3_1$Y[which(Dat_3_1$O1==0 & Dat_3_1$A1==0 & Dat_3_1$O2==1 &
                    Dat_3_1$A2==0 & Dat_3_1$O3==1)]<-
    rnorm(n=n00101,mean = 2,sd=7)
  
  Dat_3_1$Y[which(Dat_3_1$O1==0 & Dat_3_1$A1==1 & Dat_3_1$O2==0 &
                    Dat_3_1$A2==0 & Dat_3_1$O3==1)]<-
    rnorm(n=n01001,mean = 3,sd=9)
  
  Dat_3_1$Y[which(Dat_3_1$O1==1 & Dat_3_1$A1==0 & Dat_3_1$O2==0 &
                    Dat_3_1$A2==0 & Dat_3_1$O3==1)]<-
    rnorm(n=n10001,mean = 6,sd=8)
  Dat_3_1$Y[which(Dat_3_1$O1==0 & Dat_3_1$A1==0 & Dat_3_1$O2==1 &
                    Dat_3_1$A2==1 & Dat_3_1$O3==0)]<-
    rnorm(n=n00110,mean = 4,sd=8)
  Dat_3_1$Y[which(Dat_3_1$O1==0 & Dat_3_1$A1==1 & Dat_3_1$O2==0 &
                    Dat_3_1$A2==1 & Dat_3_1$O3==0)]<-
    rnorm(n=n01010,mean = 11,sd=10)
  Dat_3_1$Y[which(Dat_3_1$O1==1 & Dat_3_1$A1==0 & Dat_3_1$O2==0 &
                    Dat_3_1$A2==1 & Dat_3_1$O3==0)]<-
    rnorm(n=n10010,mean = 10,sd=9)
  Dat_3_1$Y[which(Dat_3_1$O1==0 & Dat_3_1$A1==1 & Dat_3_1$O2==1 &
                    Dat_3_1$A2==0 & Dat_3_1$O3==0)]<-
    rnorm(n=n01100,mean = 5,sd=9)
  Dat_3_1$Y[which(Dat_3_1$O1==1 & Dat_3_1$A1==0 & Dat_3_1$O2==1 &
                    Dat_3_1$A2==0 & Dat_3_1$O3==0)]<-
    rnorm(n=n10100,mean = 8,sd=8)
  Dat_3_1$Y[which(Dat_3_1$O1==1 & Dat_3_1$A1==1 & Dat_3_1$O2==0 &
                    Dat_3_1$A2==0 & Dat_3_1$O3==0)]<-
    rnorm(n=n11000,mean = 7,sd=9)
  Dat_3_1$Y[which(Dat_3_1$O1==0 & Dat_3_1$A1==0 & Dat_3_1$O2==1 &
                    Dat_3_1$A2==1 & Dat_3_1$O3==1)]<-
    rnorm(n=n00111,mean = 8,sd=7)
  Dat_3_1$Y[which(Dat_3_1$O1==0 & Dat_3_1$A1==1 & Dat_3_1$O2==1 &
                    Dat_3_1$A2==1 & Dat_3_1$O3==0)]<-
    rnorm(n=n01110,mean = 9,sd=8)
  Dat_3_1$Y[which(Dat_3_1$O1==1 & Dat_3_1$A1==1 & Dat_3_1$O2==1 &
                    Dat_3_1$A2==0 & Dat_3_1$O3==0)]<-
    rnorm(n=n11100,mean = 5,sd=10)
  Dat_3_1$Y[which(Dat_3_1$O1==0 & Dat_3_1$A1==1 & Dat_3_1$O2==0 &
                    Dat_3_1$A2==1 & Dat_3_1$O3==1)]<-
    rnorm(n=n01011,mean = 8,sd=7)
  Dat_3_1$Y[which(Dat_3_1$O1==1 & Dat_3_1$A1==0 & Dat_3_1$O2==0 &
                    Dat_3_1$A2==1 & Dat_3_1$O3==1)]<-
    rnorm(n=n10011,mean = 8,sd=9)
  Dat_3_1$Y[which(Dat_3_1$O1==1 & Dat_3_1$A1==0 & Dat_3_1$O2==1 &
                    Dat_3_1$A2==1 & Dat_3_1$O3==0)]<-
    rnorm(n=n10110,mean = 4,sd=7)
  Dat_3_1$Y[which(Dat_3_1$O1==0 & Dat_3_1$A1==1 & Dat_3_1$O2==1 &
                    Dat_3_1$A2==0 & Dat_3_1$O3==1)]<-
    rnorm(n=n01101,mean = 5,sd=10)
  Dat_3_1$Y[which(Dat_3_1$O1==1 & Dat_3_1$A1==1 & Dat_3_1$O2==0 &
                    Dat_3_1$A2==1 & Dat_3_1$O3==0)]<-
    rnorm(n=n11010,mean = 5,sd=10)
  Dat_3_1$Y[which(Dat_3_1$O1==1 & Dat_3_1$A1==1 & Dat_3_1$O2==0 &
                    Dat_3_1$A2==0 & Dat_3_1$O3==1)]<-
    rnorm(n=n11001,mean = 5,sd=10)
  Dat_3_1$Y[which(Dat_3_1$O1==1 & Dat_3_1$A1==0 & Dat_3_1$O2==1 &
                    Dat_3_1$A2==0 & Dat_3_1$O3==1)]<-
    rnorm(n=n10101,mean = 5,sd=9)
  Dat_3_1$Y[which(Dat_3_1$O1==0 & Dat_3_1$A1==1 & Dat_3_1$O2==1 &
                    Dat_3_1$A2==1 & Dat_3_1$O3==1)]<-
    rnorm(n=n01111,mean = 6,sd=9)
  Dat_3_1$Y[which(Dat_3_1$O1==1 & Dat_3_1$A1==0 & Dat_3_1$O2==1 &
                    Dat_3_1$A2==1 & Dat_3_1$O3==1)]<-
    rnorm(n=n10111,mean = 7,sd=7)
  Dat_3_1$Y[which(Dat_3_1$O1==1 & Dat_3_1$A1==1 & Dat_3_1$O2==0 &
                    Dat_3_1$A2==1 & Dat_3_1$O3==1)]<-
    rnorm(n=n11011,mean = 6,sd=8)
  Dat_3_1$Y[which(Dat_3_1$O1==1 & Dat_3_1$A1==1 & Dat_3_1$O2==1 &
                    Dat_3_1$A2==0 & Dat_3_1$O3==1)]<-
    rnorm(n=n11101,mean = 8,sd=9)
  Dat_3_1$Y[which(Dat_3_1$O1==1 & Dat_3_1$A1==1 & Dat_3_1$O2==1 &
                    Dat_3_1$A2==1 & Dat_3_1$O3==0)]<-
    rnorm(n=n11110,mean = 8,sd=9)
  Dat_3_1$Y[which(Dat_3_1$O1==1 & Dat_3_1$A1==1 & Dat_3_1$O2==1 &
                    Dat_3_1$A2==1 & Dat_3_1$O3==1)]<-
    rnorm(n=n11111,mean = 8,sd=9)
  
  Dat_1_1_0 <- as.data.frame(cbind(Dat_3_1$O1,Dat_3_1$A1,
                                   rbinom(n=N,size=1,p=0.5)))
  colnames(Dat_1_1_0)<-c("O1","A1","Y")
  
  Dat_1_1_1 <- as.data.frame(cbind(Dat_3_1$O1,Dat_3_1$A1,
                                   rnorm(n=N,mean=7,sd=7)))
  colnames(Dat_1_1_1)<-c("O1","A1","Y")
  
  
  Dat_2_1_0 <- as.data.frame(cbind(Dat_3_1$O1,Dat_3_1$A1,
                                   Dat_3_1$O2,Dat_3_1$A2,
                                   rbinom(n=N,size=1,p=0.5)))
  colnames(Dat_2_1_0)<-c("O1","A1","O2","A2","Y")
  Dat_2_1_0$Y[which(Dat_2_1_0$O1==0 & Dat_2_1_0$A1==0 &
                      Dat_2_1_0$O2==0 & Dat_2_1_0$A2==0)]<-
    rbinom(n=n0000,size=1,p=0.15)
  Dat_2_1_0$Y[which(Dat_2_1_0$O1==0 & Dat_2_1_0$A1==0 &
                      Dat_2_1_0$O2==0 & Dat_2_1_0$A2==1)]<-
    rbinom(n=n0001,size=1,p=0.2)
  Dat_2_1_0$Y[which(Dat_2_1_0$O1==0 & Dat_2_1_0$A1==0 &
                      Dat_2_1_0$O2==1 & Dat_2_1_0$A2==0)]<-
    rbinom(n=n0010,size=1,p=0.35)
  Dat_2_1_0$Y[which(Dat_2_1_0$O1==0 & Dat_2_1_0$A1==1 &
                      Dat_2_1_0$O2==0 & Dat_2_1_0$A2==0)]<-
    rbinom(n=n0100,size=1,p=0.45)
  Dat_2_1_0$Y[which(Dat_2_1_0$O1==1 & Dat_2_1_0$A1==0 &
                      Dat_2_1_0$O2==0 & Dat_2_1_0$A2==0)]<-
    rbinom(n=n1000,size=1,p=0.6)
  Dat_2_1_0$Y[which(Dat_2_1_0$O1==0 & Dat_2_1_0$A1==0 &
                      Dat_2_1_0$O2==1 & Dat_2_1_0$A2==1)]<-
    rbinom(n=n0011,size=1,p=0.8)
  Dat_2_1_0$Y[which(Dat_2_1_0$O1==0 & Dat_2_1_0$A1==1 &
                      Dat_2_1_0$O2==0 & Dat_2_1_0$A2==1)]<-
    rbinom(n=n0101,size=1,p=0.7)
  Dat_2_1_0$Y[which(Dat_2_1_0$O1==1 & Dat_2_1_0$A1==0 &
                      Dat_2_1_0$O2==0 & Dat_2_1_0$A2==1)]<-
    rbinom(n=n1001,size=1,p=0.9)
  Dat_2_1_0$Y[which(Dat_2_1_0$O1==0 & Dat_2_1_0$A1==1 &
                      Dat_2_1_0$O2==1 & Dat_2_1_0$A2==0)]<-
    rbinom(n=n0110,size=1,p=0.1)
  Dat_2_1_0$Y[which(Dat_2_1_0$O1==1 & Dat_2_1_0$A1==0 &
                      Dat_2_1_0$O2==1 & Dat_2_1_0$A2==0)]<-
    rbinom(n=n1010,size=1,p=0.25)
  Dat_2_1_0$Y[which(Dat_2_1_0$O1==1 & Dat_2_1_0$A1==1 &
                      Dat_2_1_0$O2==0 & Dat_2_1_0$A2==0)]<-
    rbinom(n=n1100,size=1,p=0.48)
  Dat_2_1_0$Y[which(Dat_2_1_0$O1==0 & Dat_2_1_0$A1==1 & 
                      Dat_2_1_0$O2==1 & Dat_2_1_0$A2==1)]<-
    rbinom(n=n0111,size=1,p=0.3)
  Dat_2_1_0$Y[which(Dat_2_1_0$O1==1 & Dat_2_1_0$A1==1 &
                      Dat_2_1_0$O2==1 & Dat_2_1_0$A2==0)]<-
    rbinom(n=n1110,size=1,p=0.6)
  Dat_2_1_0$Y[which(Dat_2_1_0$O1==1 & Dat_2_1_0$A1==0 &
                      Dat_2_1_0$O2==1 & Dat_2_1_0$A2==1)]<-
    rbinom(n=n1011,size=1,p=0.39)
  Dat_2_1_0$Y[which(Dat_2_1_0$O1==1 & Dat_2_1_0$A1==1 & 
                      Dat_2_1_0$O2==0 & Dat_2_1_0$A2==1)]<-
    rbinom(n=n1101,size=1,p=0.42)
  Dat_2_1_0$Y[which(Dat_2_1_0$O1==1 & Dat_2_1_0$A1==1 & 
                      Dat_2_1_0$O2==1 & Dat_2_1_0$A2==1)]<-
    rbinom(n=n1111,size=1,p=0.97)
  
  Dat_2_1_1 <- as.data.frame(cbind(Dat_3_1$O1,Dat_3_1$A1,
                                   Dat_3_1$O2,Dat_3_1$A2,
                                   rnorm(n=N,mean=7,sd=7)))
  colnames(Dat_2_1_1)<-c("O1","A1","O2","A2","Y")
  Dat_2_1_1$Y[which(Dat_2_1_1$O1==0 & Dat_2_1_1$A1==0 &
                      Dat_2_1_1$O2==0 & Dat_2_1_1$A2==0)]<-
    rnorm(n=n0000,mean=3,sd=7)
  Dat_2_1_1$Y[which(Dat_2_1_1$O1==0 & Dat_2_1_1$A1==0 &
                      Dat_2_1_1$O2==0 & Dat_2_1_1$A2==1)]<-
    rnorm(n=n0001,mean=4,sd=7)
  Dat_2_1_1$Y[which(Dat_2_1_1$O1==0 & Dat_2_1_1$A1==0 &
                      Dat_2_1_1$O2==1 & Dat_2_1_1$A2==0)]<-
    rnorm(n=n0010,mean=5,sd=8)
  Dat_2_1_1$Y[which(Dat_2_1_1$O1==0 & Dat_2_1_1$A1==1 &
                      Dat_2_1_1$O2==0 & Dat_2_1_1$A2==0)]<-
    rnorm(n=n0100,mean=6,sd=9)
  Dat_2_1_1$Y[which(Dat_2_1_1$O1==1 & Dat_2_1_1$A1==0 &
                      Dat_2_1_1$O2==0 & Dat_2_1_1$A2==0)]<-
    rnorm(n=n1000,mean=7,sd=7)
  Dat_2_1_1$Y[which(Dat_2_1_1$O1==0 & Dat_2_1_1$A1==0 &
                      Dat_2_1_1$O2==1 & Dat_2_1_1$A2==1)]<-
    rnorm(n=n0011,mean=8,sd=8)
  Dat_2_1_1$Y[which(Dat_2_1_1$O1==0 & Dat_2_1_1$A1==1 &
                      Dat_2_1_1$O2==0 & Dat_2_1_1$A2==1)]<-
    rnorm(n=n0101,mean=9,sd=9)
  Dat_2_1_1$Y[which(Dat_2_1_1$O1==1 & Dat_2_1_1$A1==0 &
                      Dat_2_1_1$O2==0 & Dat_2_1_1$A2==1)]<-
    rnorm(n=n1001,mean=10,sd=7)
  Dat_2_1_1$Y[which(Dat_2_1_1$O1==0 & Dat_2_1_1$A1==1 &
                      Dat_2_1_1$O2==1 & Dat_2_1_1$A2==0)]<-
    rnorm(n=n0110,mean=11,sd=8)
  Dat_2_1_1$Y[which(Dat_2_1_1$O1==1 & Dat_2_1_1$A1==0 &
                      Dat_2_1_1$O2==1 & Dat_2_1_1$A2==0)]<-
    rnorm(n=n1010,mean=12,sd=9)
  Dat_2_1_1$Y[which(Dat_2_1_1$O1==1 & Dat_2_1_1$A1==1 &
                      Dat_2_1_1$O2==0 & Dat_2_1_1$A2==0)]<-
    rnorm(n=n1100,mean=13,sd=7)
  Dat_2_1_1$Y[which(Dat_2_1_1$O1==0 & Dat_2_1_1$A1==1 &
                      Dat_2_1_1$O2==1 & Dat_2_1_1$A2==1)]<-
    rnorm(n=n0111,mean=14,sd=8)
  Dat_2_1_1$Y[which(Dat_2_1_1$O1==1 & Dat_2_1_1$A1==1 &
                      Dat_2_1_1$O2==1 & Dat_2_1_1$A2==0)]<-
    rnorm(n=n1110,mean=15,sd=9)
  Dat_2_1_1$Y[which(Dat_2_1_1$O1==1 & Dat_2_1_1$A1==0 &
                      Dat_2_1_1$O2==1 & Dat_2_1_1$A2==1)]<-
    rnorm(n=n1011,mean=16,sd=7)
  Dat_2_1_1$Y[which(Dat_2_1_1$O1==1 & Dat_2_1_1$A1==1 &
                      Dat_2_1_1$O2==0 & Dat_2_1_1$A2==1)]<-
    rnorm(n=n1101,mean=17,sd=8)
  Dat_2_1_1$Y[which(Dat_2_1_1$O1==1 & Dat_2_1_1$A1==1 & 
                      Dat_2_1_1$O2==1 & Dat_2_1_1$A2==1)]<-
    rnorm(n=n1111,mean=2,sd=9)
  
  
  Dat_3_1_0 <- as.data.frame(cbind(Dat_3_1$O1,Dat_3_1$A1,
                                   Dat_3_1$O2,Dat_3_1$A2,Dat_3_1$O3,
                                   Dat_3_1$A3,
                                   rbinom(n=N,size=1,p=0.7)))
  colnames(Dat_3_1_0)<-c("O1","A1","O2","A2","O3","A3","Y")
  
  Dat_3_1_0$Y[which(Dat_3_1$O1==0 & Dat_3_1$A1==0 & Dat_3_1$O2==0 &
                      Dat_3_1$A2==0 & Dat_3_1$O3==0)]<-
    rbinom(n=n00000,size=1,p=0.1)
  Dat_3_1_0$Y[which(Dat_3_1$O1==0 & Dat_3_1$A1==0 & Dat_3_1$O2==0 &
                      Dat_3_1$A2==0 & Dat_3_1$O3==1)]<-
    rbinom(n=n00001,size=1,p=0.2)
  Dat_3_1_0$Y[which(Dat_3_1$O1==0 & Dat_3_1$A1==0 & Dat_3_1$O2==0 &
                      Dat_3_1$A2==1 & Dat_3_1$O3==0)]<-
    rbinom(n=n00010,size=1,p=0.3)
  Dat_3_1_0$Y[which(Dat_3_1$O1==0 & Dat_3_1$A1==0 & Dat_3_1$O2==1 &
                      Dat_3_1$A2==0 & Dat_3_1$O3==0)]<-
    rbinom(n=n00100,size=1,p=0.4)
  Dat_3_1_0$Y[which(Dat_3_1$O1==0 & Dat_3_1$A1==1 & Dat_3_1$O2==0 &
                      Dat_3_1$A2==0 & Dat_3_1$O3==0)]<-
    rbinom(n=n01000,size=1,p=0.5)
  Dat_3_1_0$Y[which(Dat_3_1$O1==1 & Dat_3_1$A1==0 & Dat_3_1$O2==0 &
                      Dat_3_1$A2==0 & Dat_3_1$O3==0)]<-
    rbinom(n=n10000,size=1,p=0.6)
  Dat_3_1_0$Y[which(Dat_3_1$O1==0 & Dat_3_1$A1==0 & Dat_3_1$O2==0 &
                      Dat_3_1$A2==1 & Dat_3_1$O3==1)]<-
    rbinom(n=n00011,size=1,p=0.7)
  
  Dat_3_1_0$Y[which(Dat_3_1$O1==0 & Dat_3_1$A1==0 & Dat_3_1$O2==1 &
                      Dat_3_1$A2==0 & Dat_3_1$O3==1)]<-
    rbinom(n=n00101,size=1,p=0.8)
  
  Dat_3_1_0$Y[which(Dat_3_1$O1==0 & Dat_3_1$A1==1 & Dat_3_1$O2==0 &
                      Dat_3_1$A2==0 & Dat_3_1$O3==1)]<-
    rbinom(n=n01001,size=1,p=0.9)
  
  Dat_3_1_0$Y[which(Dat_3_1$O1==1 & Dat_3_1$A1==0 & Dat_3_1$O2==0 &
                      Dat_3_1$A2==0 & Dat_3_1$O3==1)]<-
    rbinom(n=n10001,size=1,p=0.15)
  Dat_3_1_0$Y[which(Dat_3_1$O1==0 & Dat_3_1$A1==0 & Dat_3_1$O2==1 &
                      Dat_3_1$A2==1 & Dat_3_1$O3==0)]<-
    rbinom(n=n00110,size=1,p=0.25)
  Dat_3_1_0$Y[which(Dat_3_1$O1==0 & Dat_3_1$A1==1 & Dat_3_1$O2==0 &
                      Dat_3_1$A2==1 & Dat_3_1$O3==0)]<-
    rbinom(n=n01010,size=1,p=0.35)
  Dat_3_1_0$Y[which(Dat_3_1$O1==1 & Dat_3_1$A1==0 & Dat_3_1$O2==0 &
                      Dat_3_1$A2==1 & Dat_3_1$O3==0)]<-
    rbinom(n=n10010,size=1,p=0.45)
  Dat_3_1_0$Y[which(Dat_3_1$O1==0 & Dat_3_1$A1==1 & Dat_3_1$O2==1 &
                      Dat_3_1$A2==0 & Dat_3_1$O3==0)]<-
    rbinom(n=n01100,size=1,p=0.55)
  Dat_3_1_0$Y[which(Dat_3_1$O1==1 & Dat_3_1$A1==0 & Dat_3_1$O2==1 &
                      Dat_3_1$A2==0 & Dat_3_1$O3==0)]<-
    rbinom(n=n10100,size=1,p=0.65)
  Dat_3_1_0$Y[which(Dat_3_1$O1==1 & Dat_3_1$A1==1 & Dat_3_1$O2==0 &
                      Dat_3_1$A2==0 & Dat_3_1$O3==0)]<-
    rbinom(n=n11000,size=1,p=0.75)
  Dat_3_1_0$Y[which(Dat_3_1$O1==0 & Dat_3_1$A1==0 & Dat_3_1$O2==1 &
                      Dat_3_1$A2==1 & Dat_3_1$O3==1)]<-
    rbinom(n=n00111,size=1,p=0.85)
  Dat_3_1_0$Y[which(Dat_3_1$O1==0 & Dat_3_1$A1==1 & Dat_3_1$O2==1 &
                      Dat_3_1$A2==1 & Dat_3_1$O3==0)]<-
    rbinom(n=n01110,size=1,p=0.95)
  Dat_3_1_0$Y[which(Dat_3_1$O1==1 & Dat_3_1$A1==1 & Dat_3_1$O2==1 &
                      Dat_3_1$A2==0 & Dat_3_1$O3==0)]<-
    rbinom(n=n11100,size=1,p=0.07)
  Dat_3_1_0$Y[which(Dat_3_1$O1==0 & Dat_3_1$A1==1 & Dat_3_1$O2==0 &
                      Dat_3_1$A2==1 & Dat_3_1$O3==1)]<-
    rbinom(n=n01011,size=1,p=0.17)
  Dat_3_1_0$Y[which(Dat_3_1$O1==1 & Dat_3_1$A1==0 & Dat_3_1$O2==0 &
                      Dat_3_1$A2==1 & Dat_3_1$O3==1)]<-
    rbinom(n=n10011,size=1,p=0.27)
  Dat_3_1_0$Y[which(Dat_3_1$O1==1 & Dat_3_1$A1==0 & Dat_3_1$O2==1 &
                      Dat_3_1$A2==1 & Dat_3_1$O3==0)]<-
    rbinom(n=n10110,size=1,p=0.37)
  Dat_3_1_0$Y[which(Dat_3_1$O1==0 & Dat_3_1$A1==1 & Dat_3_1$O2==1 &
                      Dat_3_1$A2==0 & Dat_3_1$O3==1)]<-
    rbinom(n=n01101,size=1,p=0.47)
  Dat_3_1_0$Y[which(Dat_3_1$O1==1 & Dat_3_1$A1==1 & Dat_3_1$O2==0 &
                      Dat_3_1$A2==1 & Dat_3_1$O3==0)]<-
    rbinom(n=n11010,size=1,p=0.57)
  Dat_3_1_0$Y[which(Dat_3_1$O1==1 & Dat_3_1$A1==1 & Dat_3_1$O2==0 &
                      Dat_3_1$A2==0 & Dat_3_1$O3==1)]<-
    rbinom(n=n11001,size=1,p=0.67)
  Dat_3_1_0$Y[which(Dat_3_1$O1==1 & Dat_3_1$A1==0 & Dat_3_1$O2==1 &
                      Dat_3_1$A2==0 & Dat_3_1$O3==1)]<-
    rbinom(n=n10101,size=1,p=0.77)
  Dat_3_1_0$Y[which(Dat_3_1$O1==0 & Dat_3_1$A1==1 & Dat_3_1$O2==1 &
                      Dat_3_1$A2==1 & Dat_3_1$O3==1)]<-
    rbinom(n=n01111,size=1,p=0.87)
  Dat_3_1_0$Y[which(Dat_3_1$O1==1 & Dat_3_1$A1==0 & Dat_3_1$O2==1 &
                      Dat_3_1$A2==1 & Dat_3_1$O3==1)]<-
    rbinom(n=n10111,size=1,p=0.97)
  Dat_3_1_0$Y[which(Dat_3_1$O1==1 & Dat_3_1$A1==1 & Dat_3_1$O2==0 &
                      Dat_3_1$A2==1 & Dat_3_1$O3==1)]<-
    rbinom(n=n11011,size=1,p=0.12)
  Dat_3_1_0$Y[which(Dat_3_1$O1==1 & Dat_3_1$A1==1 & Dat_3_1$O2==1 &
                      Dat_3_1$A2==0 & Dat_3_1$O3==1)]<-
    rbinom(n=n11101,size=1,p=0.22)
  Dat_3_1_0$Y[which(Dat_3_1$O1==1 & Dat_3_1$A1==1 & Dat_3_1$O2==1 &
                      Dat_3_1$A2==1 & Dat_3_1$O3==0)]<-
    rbinom(n=n11110,size=1,p=0.32)
  Dat_3_1_0$Y[which(Dat_3_1$O1==1 & Dat_3_1$A1==1 & Dat_3_1$O2==1 &
                      Dat_3_1$A2==1 & Dat_3_1$O3==1)]<-
    rbinom(n=n11111,size=1,p=0.42)
  
  Dat_3_1_1 <- as.data.frame(cbind(Dat_3_1$O1,Dat_3_1$A1,
                                   Dat_3_1$O2,Dat_3_1$A2,Dat_3_1$O3,
                                   Dat_3_1$A3,
                                   Dat_3_1$Y))
  colnames(Dat_3_1_1)<-c("O1","A1","O2","A2","O3","A3","Y")
  
  

  expect_warning(seqmeans(Dat_1_0_0,family = "binomial",
                          digits = 2,
                          color = c("yellow", "lightblue"),
                          pch = c(18,15),
                          title = "Design",
                          xlab = "treatment",
                          ylab="outcome",
                          xtext = 1:2,
                          legend = 1:2,
                          reference = FALSE),NA)
  
  expect_warning(seqmeans(Dat_1_0_0,family = "binomial",
                          digits = 2,
                          color = c("yellow", "lightblue"),
                          pch = c(18,15),
                          title = "Design",
                          xlab = "treatment",
                          ylab="outcome",
                          xtext = 1:2,
                          legend = 1:2,
                          reference = TRUE),NA)
  expect_warning(seqmeans(Dat_2_0_0,family = "binomial",digits = 2),NA)
  expect_warning(seqmeans(Dat_3_0_0,family = "binomial",digits = 2),NA)
  expect_warning(seqmeans(Dat_1_1_0,family = "binomial",digits = 2),NA)
  expect_warning(seqmeans(Dat_2_1_0,family = "binomial",digits = 2),NA)
  expect_warning(seqmeans(Dat_1_0_1),NA)
  expect_warning(seqmeans(Dat_1_1_1),NA)
  expect_warning(seqmeans(Dat_2_0_1),NA)
  expect_warning(seqmeans(Dat_2_1_1),NA)
  expect_warning(seqmeans(Dat_3_0_1),NA)

  expect_warning(seqmeans(Dat_1_0_0,family = "binomial",
                          digits = 2,plot = "s",
                          legend = 1,xlab = "treatment",
                          xtext = 1:2,ylab = "outcome"),NA)
  expect_warning(seqmeans(Dat_1_1_0,family = "binomial",
                          digits = 2,plot = "s",title = "Primary Outcome",
                          color = c("yellow", "lightblue"),pch=c(18,14),
                          xlab = "treatment",
                          ylab="Outcome",xtext = 1:4,
                          legend = c("evaluation", "treatment"),
                          reference = FALSE),NA)
  expect_warning(seqmeans(Dat_2_1_0,family = "binomial",
                          digits = 2,plot = "s"),NA)
  expect_warning(seqmeans(Dat_2_0_0,family = "binomial",
                          digits = 2,plot = "s"),NA)
  expect_warning(seqmeans(Dat_3_0_0,family = "binomial",
                          digits = 2,plot = "s"),NA)
  expect_warning(seqmeans(Dat_1_0_1,plot = "s",
                          xlab="SEQ",family = "gaussian"),NA)
  expect_warning(seqmeans(Dat_1_1_1,plot = "s",
                          xlab="SEQ",family = "gaussian"),NA)
  expect_warning(seqmeans(Dat_2_0_1,plot = "s",
                          xlab="SEQ",family = "gaussian"),NA)
  expect_warning(seqmeans(Dat_2_1_1,plot = "s",
                          xlab="SEQ",family = "gaussian"),NA)
  expect_warning(seqmeans(Dat_3_0_1,plot = "s",
                          xlab="SEQ",family = "gaussian"),NA)
  a<-seqmeans(Dat_2_1_0,family = "binomial",
             digits = 2,plot = "s")
  summary(a)
  expect_warning(summary(a),NA)
})
