#getdf() is the function to calculate the
#degrees of freedom of chisq test for SMART data
#allow either input a SMART data or sequence information matrix (SIM)

getdf<-function(data=NULL){
      D<-as.data.frame(data)
      if (is.null(D$O1)) {Base<-0} else {Base<-1}
      Nstage<-nstage(data=D)

      if (Nstage==1 && Base==0) {I<-0
                                Ivec<-sort(unique(D$A1))
                                I<-length(Ivec)
                                df<-I-1} else
      if (Nstage==1 && Base==1) {H<-I<-0
                                Hvec<-sort(unique(D$O1))
                                H<-length(Hvec)
                                for (h in Hvec){
                                     Ivec<-sort(unique(D$A1[which(D$O1==h)]))
                                     I<-I+length(Ivec)}
                                df<-I-H-1} else
      if (Nstage==2 && Base==0) {
        I<-J<-K<-0
        Ivec<-sort(unique(D$A1))
        #values of stage-1 treatment
        I<-length(Ivec)
        for (i in Ivec){
             Jvec<-sort(unique(D$O2[which(D$A1==i)]))
             #values of intermediate response
             J<-J+length(Jvec)
             for (j in Jvec){
                  Kvec<-sort(unique(D$A2[which(D$A1==i & D$O2==j)]))
                  #values of stage-2 treatment
                  K<-K+length(Kvec)}}
        df<-K-J+I-1} else
      if (Nstage==2 && Base==1) {
        H<-I<-J<-K<-0
        Hvec<-sort(unique(D$O1))
        H<-length(Hvec)
        for (h in Hvec){
             Ivec<-sort(unique(D$A1[which(D$O1==h)]))
             I<-I+length(Ivec)
             for (i in Ivec){
                  Jvec<-sort(unique(D$O2[which(D$O1==h & D$A1==i)]))
                  J<-J+length(Jvec)
                  for (j in Jvec){
                       Kvec<-sort(unique(D$A2[which(D$O1==h &
                                                     D$A1==i &
                                                     D$O2==j)]))
                       K<-K+length(Kvec)}}}
        df<-K-J+I-H-1} else
      if (Nstage==3 && Base==0) {
        I<-J<-K<-L<-M<-0
        Ivec<-sort(unique(D$A1))
        #values of stage-1 treatment
        I<-length(Ivec)
        for (i in Ivec){
          Jvec<-sort(unique(D$O2[which(D$A1==i)]))
          #values of intermediate response
          J<-J+length(Jvec)
          for (j in Jvec){
            Kvec<-sort(unique(D$A2[which(D$A1==i &
                                          D$O2==j)]))
            #values of stage-2 treatment
            K<-K+length(Kvec)
            for (k in Kvec){
              Lvec<-sort(unique(D$O3[which(D$A1==i &
                                            D$O2==j & D$A2==k)]))
              L<-L+length(Lvec)
              for (l in Lvec){
                   Mvec<-sort(unique(D$A3[which(D$A1==i &
                                                 D$O2==j &
                                                 D$A2==k & D$O3==l)]))
                   M<-M+length(Mvec)}}}}
        df<-M-L+K-J+I-1} 

      return(df)
}



