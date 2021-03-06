% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getncp.R
\name{getncp}
\alias{getncp}
\title{get non-centralized parameter}
\usage{
getncp(df, alpha = 0.05, beta = 0.2, d = 1e-04, start = 5)
}
\arguments{
\item{df}{Degrees of freedom of chi-square test}

\item{alpha}{Type I error rate of chi-square test. The default alpha=0.05}

\item{beta}{Type II error rate of chi-square test. The default beta=0.20}

\item{d}{Critical value of distance of the searching
procedure. The search of non-centralized parameter value
stops at the absolute distance between the actual power and
the target power less than the value of d. The default value
of d=0.0001}

\item{start}{Initial value of searching the non-centralized parameter.}
}
\value{
The value of non-centralized parameter for the
 chi-square distribution
}
\description{
Return the value of non-centralized parameter
for the chi-square distribution given type I,
II error and degrees of freedom.
}
