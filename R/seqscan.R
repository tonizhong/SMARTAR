#function seqscan() return a sequence-specific
#information matrix (SIM) w.r.t a SMART design
#each row represents a specific treatment sequence,
#indicated as a series of variable (O1,A1,O2,A2,O3,A3,Y)

seqscan<-function(data){
        D<-as.data.frame(data)
        if (is.null(D$O1)) {Base<-0} else {Base<-1}
        nstage<-nstage(data=D)
        N<-nrow(D)

        smat<-NULL
        seq<-0
        if (nstage==1 && Base==0){
          Ivec<-sort(unique(D$A1))
          #values of stage-1 treatment
          for (i in Ivec){
               sn<-length(D$Y[which(D$A1==i)])
               #sequence-specific frequency
               seq<-seq+1
               smat<-rbind(smat,c(seq,i,sn))
               }
          smat<-as.data.frame(smat)
          colnames(smat)<-c("SEQ","A1","N")} else
        if (nstage==1 && Base==1){Hvec<-sort(unique(D$O1))
        #values of baseline screen outcome
        for (h in Hvec){
             Ivec<-sort(unique(D$A1[which(D$O1==h)]))
             #values of stage-1 treatment
             for (i in Ivec){
             sn<-length(D$Y[which(D$O1==h & D$A1==i)])
             #sequence-specific frequency
             seq<-seq+1
             smat<-rbind(smat,c(seq,h,i,sn))
             }}
        smat<-as.data.frame(smat)
        colnames(smat)<-c("SEQ","O1","A1","N")} else
        if (nstage==2 && Base==0){Ivec<-sort(unique(D$A1))
        #values of stage-1 treatment
        for (i in Ivec){
             Jvec<-sort(unique(D$O2[which(D$A1==i)]))
             for (j in Jvec){
                  Kvec<-sort(unique(D$A2[which(D$A1==i &
                                                D$O2==j)]))
                  for (k in Kvec){
                       sn<-length(D$Y[which(D$A1==i &
                                             D$O2==j &
                                             D$A2==k)])
                       #sequence-specific frequency
                       seq<-seq+1
                       smat<-rbind(smat,c(seq,i,j,k,sn))
                       }}}
        smat<-as.data.frame(smat)
        colnames(smat)<-c("SEQ","A1","O2","A2","N")} else
        if (nstage==2 && Base==1){
          Hvec<-sort(unique(D$O1))
          #values of baseline screen outcome
          for (h in Hvec){
            Ivec<-sort(unique(D$A1[which(D$O1==h)]))
            #values of stage-1 treatment
            for (i in Ivec){
                 Jvec<-sort(unique(D$O2[which(D$O1==h &
                                               D$A1==i)]))
                 for (j in Jvec){
                      Kvec<-sort(unique(D$A2[which(D$O1==h &
                                                    D$A1==i &
                                                    D$O2==j)]))
                      for (k in Kvec){
                           sn<-length(D$Y[which(D$O1==h &
                                                 D$A1==i &
                                                 D$O2==j &
                                                 D$A2==k)])
                           #sequence-specific frequency
                           seq<-seq+1
                           smat<-rbind(smat,c(seq,h,i,j,k,sn))
                           }}}}
          smat<-as.data.frame(smat)
          colnames(smat)<-c("SEQ","O1","A1","O2","A2","N")} else
        if (nstage==3 && Base==0){
          Ivec<-sort(unique(D$A1))
        #values of stage-1 treatment
          for (i in Ivec){
            Jvec<-sort(unique(D$O2[which(D$A1==i)]))
            for (j in Jvec){
              Kvec<-sort(unique(D$A2[which(D$A1==i &
                                            D$O2==j)]))
              for (k in Kvec){
                   Lvec<-sort(unique(D$O3[which(D$A1==i &
                                                 D$O2==j &
                                                 D$A2==k)]))
                   for (l in Lvec){
                        Mvec<-sort(unique(D$A3[which(D$A1==i &
                                                      D$O2==j &
                                                      D$A2==k &
                                                      D$O3==l)]))
                        for (m in Mvec){
                             sn<-length(D$Y[which(D$A1==i &
                                                   D$O2==j &
                                                   D$A2==k & D$O3==l &
                                                   D$A3==m)])
                             #sequence-specific frequency
                             seq<-seq+1
                             smat<-rbind(smat,c(seq,i,j,k,l,m,sn))
                             }}}}}
          smat<-as.data.frame(smat)
          colnames(smat)<-c("SEQ","A1","O2","A2","O3","A3","N")}
       
 
        return(smat)
}
