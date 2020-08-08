#' Identify adaptive treatment strategy and estimate strategy values
#'
#' Return a message that lists all the
#' adaptive treatment strategy embedded in SMART design.
#' It also gives the estiamted strategy values and the
#' variance-covariance matrix of estimated values.
#' @param data Input data frame of the sequential
#' randomized trial (SMART) data used for analysis.
#' The data should include the variables of stage-specific
#'  treatments (At; t=1,2,3), intermediate evaluation
#'  (Ot; t=1,2,3) and final primary outcome (Y),
#'  where t represent the number of stages embedded in design.
#'   If stage-1 treatment (A1) takes into account the information
#'   of baseline evaluation, O1 needed to be include in data,
#'   otherwise not.
#' @param family A character string to specify the
#' type of final primary outcome. The default is
#' family=“gaussian”, which refers to the continuous
#'  primary outcome. If family=”binomial” then the
#'  primary outcome will be treated as binary variable.
#' @param method A character string to specify the
#' method of estimation. If method="Gest" then
#' G-computation method is used. If method="IPW" then
#' Inversed Probability Weighting method is used.
#' @param digits An integer indicating the number
#' of decimal places for sequence-specific mean and variance.
#'  Default is digits=NULL.
#' @param common If common=TRUE, the pooled variance across
#' all the treatment sequences are used in estimation.
#' Otherwise use the sequence-specific variance. The default is common=FALSE.
#' @param conf If conf=TRUE, output confidence intervals
#' for estimate strategy values. The default is conf=TRUE.
#' @param alpha Type I error rate control for confidence
#' interval. The default is alpha=0.05.
#' @param plot If plot=TRUE, output the graphs of
#'  treatment effects with CIs. The default is plot=TRUE.
#' @param title Characters indicating the title of the graphs.
#' Default is “Strategy values with confidence intervals”.
#' @param color Characters indicating the color of the graphs.
#' Default is color=“forestgreen”.
#' @param ylab Characters to specify the label of the 
#' vertical axis of the output figure. 
#' Default is “Strategy value”.
#' @param xlab characters to specify the label of the horizontal 
#' axis of the output figure.
#' @param xtext Specification for the text of the horizontal axis of the graphs.
#' @param pch An integer to specify the shape of points in the graphs. 
#' The default is pch=15.
#' @param cex An integer to specify the amount by which plotting symbols 
#' should be magnified. The default is cex=2.
#' @param lwd An integer to specify the line width,
#' The lines refer to the width of the confidence interval. 
#' The default is lwd=1.
#' @param ylim Integers to specify the maximum and minimum value of 
#' y axis.
#' @param mar A numerical vector of the form c(bottom, left, top, right) 
#' which gives the number of lines of margin to be specified 
#' on the four sides of the plot. 
#' @param cex.axis The magnification to be used for the horizontal axis 
#' annotation relative to the current setting of cex.
#' @param line Specifying a value for line overrides 
#' the default placement of label 
#' of the horizontal axis of the graphs. 
#' @return
#'
#' An object of ``value” is return, which contain
#' the index of all the adaptive treatment strategies,
#' strategy-specific sample sizes and estimated values
#' with standardized errors.
##' \itemize{
##'    \item ATS: Index of adaptive treatment strategy
##'    from 1 to G, where G is total number of strategies
##'     defined in SMART
##'    \item ds: Stage-specific decision makings given
##'     certain histories corresponding to each strategy.
##'     The number of columns of ``ds'' is defined by strategy
##'      and details are shown in the output.
##'    \item N: Number of subjects following a strategy.
##'    \item value: Estimated strategy values.
##'    \item se: standard errors of estimation
##'    \item lower.CI: Lower bound of (1-alpha) level confidence
##'    interval for strategy values
##'    \item upper.CI: Upper bound of (1-alpha) level confidence
##'    interval for strategy values
##'    }
#' An object of ``vmat'' is return, which is variance-covariance matrix of
#' estimated strategy values
#'
#' @references Lavori P.W. and Dawson R. (2007). Improving the
#' efficiency of estimation in randomization trials of adaptive
#' treatment strategies. \emph{Clinical Trials}, 4: 297-308.
#' @references Ko and Wahed A.S. (2015). Design of sequentially
#' randomization trials for testing adaptive treatment strategies.
#' \emph{Statistics in Medicine}, 31, 812-830.
#'
#' @examples
#'
#' atsmeans(data=codiacs,family="gaussian",method="Gest",
#' conf=TRUE,common=TRUE,alpha=0.05,plot=TRUE,pch=12,lwd=2)
#'
#' @export

#atsmeans=function(data,family="normal",method="Gest",
#common=FALSE,conf=TRUE,alpha=0.05,plot=FALSE){
atsmeans<-function(data,family=c("gaussian","binomial")[1],
                   method=c("Gest","IPW")[1],
                  digits=NULL,common=FALSE,conf=TRUE,
                  alpha=0.05,plot=FALSE,
title="Strategy values with confidence interval",color="forestgreen",
                  ylab="Strategy value",
                  xlab=NULL,xtext=NULL,pch=15,cex=2,lwd=3,
ylim=NULL,mar=NULL,cex.axis=1,line=NULL){
  D<-as.data.frame(data)
  FA<-family
  Ma<-method
  Com<-common
  Al<-alpha
  if (is.null(D$O1)) {Base<-0} else {Base<-1}
  Nstage<-nstage(data=D)

  Umat<-em(data=D,method=Ma)
  Val<-Umat$value
  Vmat<-evcmat(data=D,family=FA,
              method=Ma,common=Com)
  se<-sqrt(diag(Vmat))
  CIs<-atsci(eumat=Umat,evmat=Vmat,alpha=Al)
  if (conf==FALSE) {Umat<-data.frame(Umat,se)} else
  if (conf==TRUE) {Umat<-data.frame(Umat,se,CIs)}
  message(paste("$value: estimated strategy values
                (with confidence intervals)",
                "$vmat: variance-covariance matrix
                of estimated strategy values \n",
                sep="\n"))
  if (Nstage==1 && Base==0) {
   message("A strategy is
            defined as a single-stage
            decision making (d0) for A1 at baseline")
    opar<-par(mar=c(4,4,4,3))
    on.exit(par(opar))} else
  if (Nstage==1 && Base==1) {
    message(paste("A strategy is defined as a vector of
                  single-stage decision makings (d0,d1),",
                  "each of which corresponds to a
                  possible outcome of baseline evulation (O1). \n",
                  "d0 is the stage-1 decision making
                  for A1, conditioning on O1=0",
                  "d1 is the stage-1 decision making
                  for A1, conditioning on O1=1",
                  sep="\n"))
      opar<-par(mar=c(5,4,4,3))
      on.exit(par(opar))
      } else
  if (Nstage==2 && Base==0) {
    message(paste("A strategy is defined as a vector of
                  decision makings (d0;d00,d01) for 2 stages \n",
                  "d0 is the stage-1 decision making for A1",
                  "d00 is the stage-2 decision making for A2,
                  conditioning on A1=d0 and O2=0",
                  "d01 is the stage-2 decision making for A2,
                  conditioning on A1=d0 and O2=0",
                  sep="\n"))
             opar<-par(mar=c(6,4,4,3))
             on.exit(par(opar))
             } else

  if (Nstage==2 && Base==1) {
    message(paste("A strategy is defined as a vector of decision
                  makings (d0,d1;d00,d01,d10,d11) \n",
                  "d0 is the stage-1 decision making conditioning on O1=0",
                  "d1 is the stage-1 decision making conditioning on O1=1",
                  "d00 is the stage-2 decision making conditioning
                  on A1=d0 and O2=0",
                  "d01 is the stage-2 decision making conditioning
                  on A1=d0 and O2=0",
                  "d00 is the stage-2 decision making conditioning
                  on A1=d1 and O2=0",
                  "d01 is the stage-2 decision making conditioning
                  on A1=d1 and O2=0",
                  sep="\n"))
      opar<-par(mar=c(7,4,4,3))
      on.exit(par(opar))
      } else
  if (Nstage==3 && Base==0) {
    message(paste("A strategy is defined
                  as a vector of decision makings
                  (d0;d00,d01;d000,d001,d010,d111) \n",
                  "d0 is the stage-1 decision making",
                  "d00 is the stage-2 decision making conditioning on
                  A1=d0 and O2=0",
                  "d01 is the stage-2 decision making conditioning on
                  A1=d0 and O2=0",
                  "d000 is the stage-3 decision making conditioning on
                  A1=d0, O2=0, A3=d00 and O3=0",
                  "d001 is the stage-3 decision making conditioning on
                  A1=d0, O2=0, A3=d00 and O3=1",
                  "d010 is the stage-3 decision making conditioning on
                  A1=d0, O2=1, A3=d01 and O3=0",
                  "d011 is the stage-3 decision making conditioning on
                  A1=d0, O2=1, A3=d01 and O3=1",
                  sep="\n"))
      opar<-par(mar=c(8,4,4,3))
      on.exit(par(opar))
      } 

         if (plot==TRUE) {atsciplot(uimat=Umat,
                                    nstage=Nstage,baseline=Base,title=title,
                                    col=color,ylab=ylab,xlab=xlab,
                                    pch=pch,cex=cex,lwd=lwd,xtext=xtext,
                                    lim=ylim,mar=mar,cex.axis=cex.axis,
                                    line=line)}
         if (is.null(digits)) {
           outcome<-list(value=Umat,vmat=Vmat)
           attr(outcome,'class') <- c('myclass','list')
           return(outcome)}
         else {
           outcome<-list(value=round(Umat,digits),
                         vmat=round(Vmat,digits))
           attr(outcome,'class') <- c('myclass','list')
           return(outcome)}
}
