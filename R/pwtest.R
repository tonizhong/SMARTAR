#' @importFrom stats pnorm

pwtest<-function(data,family=c("gaussian","binomial"),
                method=c("Gest","IPW"),alpha=0.05,
                common=FALSE,adjust=NULL,ntest=NULL){
       D<-as.data.frame(data)
       FA<-family
       Ma<-method
       Com<-common
       Nt<-ntest
       if (is.null(D$O1)) {Base<-0} else {Base<-1}
       Dmat<-atsscan(data=D)
       G<-nrow(Dmat)
       label<-getag(nats=G)
       Val<-em(data=D,method=Ma)$value
       Umat<-matrix(Val,ncol=1)
       Vmat<-evcmat(data=D,family=FA,method=Ma,common=Com)
       PEmat<-LOmat<-UPmat<-Zmat<-Pmat<-matrix(rep(NA,(G-1)*G),ncol=G)
       if (is.null(adjust)) {Q<-qnorm(1-alpha/2)} else
       if (adjust=="Bon" && is.null(Nt)) {Q<-qnorm(1-alpha/((G-1)*G))} else
       if (adjust=="Bon" && Nt>0) {Q<-qnorm(1-alpha/Nt)}
       for (j in seq(G)){
            Cmat<-getcmat(control=j,nats=G)
            PEmat[,j]<-Cmat%*%Umat
            DVD<-Cmat%*%Vmat%*%t(Cmat)
            VAR.ig<-diag(DVD)
            LOmat[,j]<-PEmat[,j]-Q*sqrt(VAR.ig)
            UPmat[,j]<-PEmat[,j]+Q*sqrt(VAR.ig)
            Zmat[,j]<-PEmat[,j]/sqrt(VAR.ig)
            Pmat[,j]<-(1-pnorm(abs(Zmat[,j])))*2
            }
       Lab<-paste(label[,1],"vs.",label[,2])
       pwout<-as.data.frame(cbind(as.vector(PEmat),
                                 as.vector(LOmat),
                                 as.vector(UPmat),
                                 as.vector(Zmat),
                                 as.vector(Pmat)))
       pwout<-cbind.data.frame(Lab,pwout)
       colnames(pwout)<-c("label","diff","lower.CI","upper.CI","Z","Pvalue")
       return(pwout)
}
