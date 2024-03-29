% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setComparisons.R
\name{setComparisons_assign}
\alias{setComparisons_assign}
\alias{setComparisons<-}
\alias{setComparisons<-.bcea}
\alias{setComparisons<-.default}
\title{Set Comparison Group}
\usage{
setComparisons(he) <- value

\method{setComparisons}{bcea}(he) <- value

\method{setComparisons}{default}(he) <- value
}
\arguments{
\item{he}{A \code{bcea} object containing the results of the Bayesian
modelling and the economic evaluation.}

\item{value}{Comparison}
}
\value{
bcea-type object
}
\description{
One of the alternative way to set (e,c) comparison group.
}
\seealso{
\code{\link[=setComparisons]{setComparisons()}}
}
