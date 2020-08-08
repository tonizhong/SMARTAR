#atsci() calculate the confidence interval
#of single ATS value assuming normality
#' @importFrom stats qnorm

atsci<-function(eumat,evmat,alpha=0.05){
      Umat<-eumat             #Amat and Umat
      Vmat<-evmat             #variance-covariance matrix
      Al<-alpha               #type I error for calculating confidence interval
      V<-diag(Vmat)
      Q<-qnorm(1-Al/2)
      Ran<-Q*sqrt(V)
      PE<-Umat$value
      lower<-PE-Ran
      upper<-PE+Ran
      uimat<-as.data.frame(cbind(lower,upper))
      return(uimat)
}
