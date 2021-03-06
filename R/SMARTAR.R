#' SMARTAR: Sequential Multiple Assignment Randomized Trial and Adaptive
#' Randomization.
#'
#' Primary data analysis for sequential multiple assignment
#' randomization trial (SMART)
#' and calibration tools for clinical trial planning
#' purposes. It has several innovative features:
#' \itemize{
#'   \item it supports exploratory data analysis (EDA);
#'   \item it is the first R package that can construct
#'   and directly output simultaneous confidence intervals for ATS comparisons;
#'   \item it provides the results of sample size calculation
#'   based on varying published statistical methods in two different fashions,
#'           the global test and the pairwise test fashion.
#' }
#'
#' As of right now, \code{SMARTAR} exports five major functions:
#' \itemize{
#'   \item{\code{seqmeans}} - design diagram, descriptive
#'   statistics, and summarized graphs at sequence level;
#'   \item{\code{atsmeans}} - descriptive statistics and
#'   summarized graphs at adaptive treatment strategy level;
#'   \item{\code{smartest}} - results of global test and
#'   pairwise tests; output simultaneous CIs for ATS comparison;
#'   \item{\code{smartsize}} - results of sample size calculation;
#'   \item{\code{getncp}} - value of non-centralized
#'   parameter for the chi-square distribution.
#' }
#'
#' @docType package
#' @name SMARTAR
#' @author \strong{Maintainer}: Tony Zhong \email{xiaobo.zhong@mountsinai.org}
#'
#' Authors:
#' \itemize{
#' \item  Tony Zhong \email{xiaobo.zhong@mountsinai.org}
#' \item  Xinru Wang \email{xw2676@cumc.columbia.edu}
#' \item  Bin Cheng \email{bc2159@cumc.columbia.edu}
#' \item  Ying Kuen Cheung \email{yc632@cumc.columbia.edu}
#' }
#'
#' @seealso
#' Useful links:
#' \itemize{
#'  \item
#'  \url{https://CRAN.R-project.org/package=SMARTAR}
#'  \item
#'  \url{https://github.com/tonizhong/SMARTAR/}
#'  \item Report bugs at: \url{https://github.com/tonizhong/SMARTAR/issues/}
#'  }
NULL
