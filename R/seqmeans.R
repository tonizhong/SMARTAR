#' Summarize sequence-specific descriptive statistics
#'
#' Return a message that lists all the treatment
#'  sequence embedded in SMART design and summarizes all
#'   the sequence-specific descriptive statistics. It also
#'   provide design diagram of SMART and graphs of sequence-specific
#'    descriptive statistics (boxplot for continuous primary outcome
#'     and barchart for binary primary outcome).
#' @importFrom stats var
#' @param data Input data frame of the sequential randomized
#' trial (SMART) data used for analysis. The data should include
#'  the variables of stage-specific treatments (At; t=1,2,3),
#'  intermediate evaluation (Ot; t=1,2,3) and final primary outcome (Y),
#'   where t represent the number of stages embedded in design.
#'   If stage-1 treatment (A1) takes into account the information
#'   of baseline evaluation, O1 needed to be include in data,
#'   otherwise not.
#' @param family A character string to specify the
#' type of final primary outcome. The default is
#' family=“gaussian”, which refers to the continuous
#' primary outcome. If family=”binomial” then the primary
#' outcome will be treated as binary variable.
#' @param plot A character string to specify the output figure.
#'  If plot=”d” then output the design diagram of SMART;
#'  If plot=”s” then output boxplot for continuous primary
#'  outcome or bar plot for binary primary outcome by sequence.
#'  The default is plot=”d”.
#' @param digits An integer indicating the number of decimal places
#' for sequence-specific mean and variance. Default is digits=NULL.
#' @param color Characters indicating the color of
#' boxplot for continuous primary outcome and barplot for binomial
#' primary outcome. The first Default is “yellow", the second default
#' is c("yellow","forestgreen").
#' @param pch Two integer indicating the point shape of
#' design diagram of SMART. Default is c(19,15).
#' @param title An character indicating the title of boxplot, barplot
#' for primary outcome or design diagram of SMART. For primary
#' outcome, the default is "Primary outcome by treatment
#' sequence (O1,A1,O2...)", for design diagram, the default is
#' "Design diagram of SMART".
#' @param ylab Characters to specify the label of the vertical 
#' axis of the output figure.
#' @param xlab Characters to specify the label of the horizontal
#'  axis of the output figure.
#' @param xtext Characters indicating the text of x axis boxplot.
#' @param legend Characters to specify the legend of the design 
#' diagram of SMART. Default is legend=c("Evaluation","Treatment").
#' @param reference Logic argument to add a reference line to the 
#' graph of descriptive statistics of the primary outcome. 
#' The value of the reference line is equal to the average 
#' of all sequence-specific means. If TRUE, add a reference 
#' line (mean of outcome) to boxplot of the primary outcome, 
#' otherwise do not add reference line. The default is reference=FALSE.
#' @return
#' an object of information of all the treatment sequences
#'  and sequences-specific descriptive statistics
#'  defined in a SMART data
##' \itemize{
##'    \item SEQ: Index of treatment sequences.
##'    \item O1: Baseline evaluation outcome.
##'    \item A1: Action of stage-1 treatment.
##'    \item O2: Intermeidate outcome evaluated at the end of stage 1.
##'    \item A2: Action of stage-2 treatment.
##'    \item O3: Intermeidate outcome evaluated at the end of stage 2.
##'    \item A3: Action of stage-3 treatment.
##'    \item N: Number of subjects following a certain treatment sequence.
##'    \item MEAN: Sequence-specified sample mean.
##'    \item VAR: Sequence-specified sample variance.
##'    }
#' @references Thall P., Millikan R. and Sung H.G. (2000),
#'  ``Evaluating multiple treatment courses in clinical trials,''
#'  \emph{Statistics in Medicine}, 19, 1011-1028
#' @references Lavori P. W. and Dawson R. (2000),
#'  ``A design for testing clinical strategies: biased
#'  adaptive within-subject randomization,''
#'  \emph{Journal of the Royal Statistical Society, Series A}, 163, 29-38.
#' @references Murphy, S. A. (2005), ``An experimental
#' design for the development of adaptive treatment strategies,”
#' \emph{Statistics in Medicine}, 24, 1455-1481.
#'@examples
#'
#' #get descriptive statistics
#' seqmeans(data=codiacs,family="gaussian",plot="d",digits=2,pch=c(18,14),
#' xtext=1:4,xlab="SMARTAR design")
#' seqmeans(data=codiacs, family="gaussian", plot="s",digits=2,
#' color="lightblue",
#' title="Primary outcome",ylab="Primary outcome")
#'
#' @export

seqmeans<-function(data,family=c("gaussian","binomial")[1],plot="d",digits=NULL,
                   color=c("yellow","forestgreen"),
                   pch=c(19,15),title=NULL, xlab=NULL,
                   ylab=NULL,
                   xtext=NULL,legend=c("Evaluation","Treatment"),
                   reference=TRUE){
         D<-data.frame(data)
         FA<-family
         if (is.null(D$O1)) {Base<-0} else {Base<-1}
         Smat<-seqscan(data=D)

         N<-nrow(D)
         Nstage<-nstage(data=D)
         ns<-nrow(Smat)

         M<-V<-sd<-rep(NA,ns)
         Smat<-data.frame(Smat,M,V,sd)

         SEQ<-rep(NA,N)
         #assign sequence index for each subject in input dataset
         D<-cbind(D,SEQ)

         message("Each subject followed one of the below
                 treatment sequences during the trial.")
         if (Nstage==1 && Base==0) {
           for (j in seq(ns)) {
             D$SEQ[which(D$A1==Smat[j,2])]<-j}
            message("A treatment sequence is defined as
                    a scalar of (A1). \n")} else
         if (Nstage==1 && Base==1) {
           for (j in seq(ns)) {
             D$SEQ[which(D$O1==Smat[j,2] & D$A1==Smat[j,3])]<-j}
            message("A treatment sequence is defined as
                    a vector of values (O1,A1). \n")}else
         if (Nstage==2 && Base==0) {
           for (j in seq(ns)) {
             D$SEQ[which(D$A1==Smat[j,2] &
                           D$O2==Smat[j,3] & D$A2==Smat[j,4])]<-j}
            message("A treatment sequence is defined
                    as a vector of values (A1,O2,A2). \n")} else
         if (Nstage==2 && Base==1) {
           for (j in seq(ns)) {
             D$SEQ[which(D$O1==Smat[j,2] & D$A1==Smat[j,3] &
                           D$O2==Smat[j,4] & D$A2==Smat[j,5])]<-j}
            message("A treatment sequence is defined as
                    a vector of values (O1,A1,O2,A2). \n")} else
         if (Nstage==3 && Base==0) {
           for (j in seq(ns)) {
             D$SEQ[which(D$A1==Smat[j,2] & D$O2==Smat[j,3] &
                           D$A2==Smat[j,4] &
                           D$O3==Smat[j,5] & D$A3==Smat[j,6])]<-j}
            message("A treatment sequence is defined as a
                    vector of values (A1,O2,A2,O3,A3). \n")} 


         if (plot=="s") {seqplots(data=D,family=FA,color=color,title=title,
                                 ylab=ylab,xlab=xlab,xtext=xtext,
                                  reference=reference)}
         else {ddplot(data=D,pch=pch,title=title,
                      xlab=xlab,xtext=xtext,legend=legend)}
         for (s in seq(ns)){
           sm<-mean(D$Y[which(D$SEQ==s)])
                   sv<-var(D$Y[which(D$SEQ==s)])
                   sd<-sqrt(var(D$Y[which(D$SEQ==s)]))
                   if (family=="gaussian") {
                     Smat$M[which(Smat$SEQ==s)]<-sm
                     Smat$V[which(Smat$SEQ==s)]<-sv
                     Smat$sd[which(Smat$SEQ==s)]<-sd} else
                   if (family=="binomial") {
                     Smat$M[which(Smat$SEQ==s)]<-sm
                     Smat$V[which(Smat$SEQ==s)]<-sm*(1-sm)
                     Smat$sd[which(Smat$SEQ==s)]<-sqrt(sm*(1-sm))}else
                       stop("invalid enter for argument 'family'")}
         colnames(Smat)[c(ncol(Smat)-2,
                          ncol(Smat)-1,ncol(Smat))]<-c("MEAN","VAR","SD")
         if (is.null(digits)) {
           attr(Smat,'class') <- c('myclass','data.frame')
           return(Smat)} else
            {Smat$MEAN<-round(Smat$MEAN,digits)
             Smat$VAR<-round(Smat$VAR,digits)
             Smat$SD<-round(Smat$SD,digits)
             attr(Smat,'class') <- c('myclass','data.frame')
             return(Smat)}
         #if (plot=="s") {seqplots(data=D,family=FA)} else {ddplot(data=D)}
}
