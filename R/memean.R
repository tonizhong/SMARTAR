memean<-function(data,ats=NULL){
       D<-as.data.frame(data)
       ATS<-ats
       if (is.null(D$O1)) {Base<-0} else {Base<-1}
       Nstage<-nstage(data=D)
       N<-nrow(D)
       IPW<-weight(data=D)
       IND<-markats(data=D,ats=ATS)
       D<-data.frame(D,IPW,IND)
       D$weight<-D$IPW*D$IND
       eu<-sum(D$weight*D$Y)/sum(D$weight)
       return(eu)
}
