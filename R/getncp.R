#' get non-centralized parameter
#'
#' Return the value of non-centralized parameter
#' for the chi-square distribution given type I,
#' II error and degrees of freedom.
#'
#' @param alpha Type I error rate of chi-square test. The default alpha=0.05
#' @param beta Type II error rate of chi-square test. The default beta=0.20
#' @param df Degrees of freedom of chi-square test
#' @param d Critical value of distance of the searching
#' procedure. The search of non-centralized parameter value
#' stops at the absolute distance between the actual power and
#' the target power less than the value of d. The default value
#' of d=0.0001
#' @param start Initial value of searching the non-centralized parameter.
#' @return The value of non-centralized parameter for the
#'  chi-square distribution
#' @importFrom stats pchisq qchisq
#' @export

#algorithem similar to the Newton-Ralphson procedure
getncp<-function(df,alpha=0.05,beta=0.20,d=0.0001,start=5){
       DF<-df; NCP<-start; diff<-1
       i<-1
       while(diff>=d){
             NCP<-NCP+0.0001
             CP<-pchisq(q=qchisq(p=1-alpha,df=DF),df=DF,ncp=NCP)
             diff<-abs(beta-CP)
             i<-i+1
             }
  return(NCP)
}
