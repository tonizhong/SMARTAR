#' Summarize the dataframe got from the package
#' 
#' Summarize the result of sequential primary outcome in `seqmeans`
#' and the estimated strategy values in `atsmeans`
#' @param object A dataframe to be summarized
#' @param ... other arguments in summary generic function
#' @return
#' Descriptive table of the dataframe is returned
#' @examples
#' ats_outcome=atsmeans(data=codiacs,conf=TRUE,
#'  alpha=0.05,digits = 2,pch=18,xlab="abc")
#' summary(ats_outcome)
#' 
#' @rdname summary
#' @export summary.myclass
#' @export
    
summary.myclass <- function(object,...)
{
  if(!is.null(object$MEAN)){
  mean_primary_outcome<-summary(object$MEAN)
  return(mean_primary_outcome)
  }else
    if(!is.null(object[[1]]$value)){
      mean_summary<-summary(object[[1]]$value)
      lower_summary<-summary(object[[1]]$value)
      upper_summary<-summary(object[[1]]$value)
      return(list(mean_summary=mean_summary,lower_summary=mean_summary
                  ,upper_summary=mean_summary))
    }else
      message("Not valid enter")
}
