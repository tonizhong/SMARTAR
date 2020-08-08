#getag() gives tag of all pairwise comparison,
#which presented as "ats.I vs. ats.J" (I,J defined in atsscan)

getag<-function(nats){
      tag<-NULL
      for (i in seq(nats)){
           for (j in seq(nats)){
                if (i!=j) {tag<-rbind(tag,c(i,j))}
                }}
      colnames(tag)<-c("I","J")
      return(tag)
}

