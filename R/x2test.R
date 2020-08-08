#' @importFrom MASS ginv
#' @importFrom stats pchisq

x2test<-function(data,family="gaussian",method="Gest",common=FALSE){
       D<-as.data.frame(data); Ma<-method; FA<-family; Com<-common
       if (is.null(D$O1)) {Base<-0} else {Base<-1}
       N<-nrow(D)
       DF<-getdf(data=D)
       Val<-em(data=D,method=Ma)$value
       Umat<-matrix(Val,ncol=1)
       G<-length(Val)
       Vmat<-evcmat(data=D,family=FA,method=Ma,common=Com)
       Amat<-getcmat(control=1,nats=G)
       MA<-Amat%*%Vmat%*%t(Amat)
       MAT<-MASS::ginv(MA)
       TS<-t(Amat%*%Umat)%*%MAT%*%(Amat%*%Umat)
       Pvalue<-pchisq(TS,df=DF,lower.tail=FALSE)
       x2out<-cbind(N,G,DF,TS,Pvalue)
       x2out<-as.data.frame(x2out)
       colnames(x2out)<-c("size","nATS","df","chisq","Pvalue")
       return(x2out)
}


