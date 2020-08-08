#function nstage() count the number of stages
#embedded in sequential randomized design

nstage<-function(data){
       D<-as.data.frame(data)
       A1<-D$A1; A2<-D$A2; A3<-D$A3
       if (!is.null(A1) &&  is.null(A2) &&  is.null(A3)) {nstage<-1} else
       if (!is.null(A1) && !is.null(A2) &&  is.null(A3)) {nstage<-2} else
       if (!is.null(A1) && !is.null(A2) && !is.null(A3)) {nstage<-3}
       return(nstage)
}
