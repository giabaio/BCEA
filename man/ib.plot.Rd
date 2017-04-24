\name{ib.plot}
\alias{ib.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Incremental Benefit (IB) distribution plot
}
\description{
Plots the distribution of the Incremental Benefit (IB) for a given value of the 
willingness to pay threshold
}
\usage{
ib.plot(he, comparison = NULL, wtp = 25000, bw = nbw, n = 512, 
  xlim = NULL, graph=c("base","ggplot2"))
}
\arguments{
  \item{he}{
A \code{bcea} object containing the results of the Bayesian modelling and the economic 
evaluation.
}
  \item{comparison}{
In the case of multiple interventions, specifies the one to be used in comparison with 
the reference. Default value of \code{NULL} forces R to consider the first non-reference 
intervention as the comparator.
}
  \item{wtp}{
The value of the willingness to pay threshold. Default value at \code{25000}.
}
  \item{bw}{
Identifies the smoothing bandwith used to construct the kernel estimation of the IB 
density.
}
  \item{n}{
The number of equally spaced points at which the density is to be estimated.
}
  \item{xlim}{
The limits of the plot on the x-axis.
}
  \item{graph}{
A string used to select the graphical engine to use for plotting. Should (partial-)match 
the two options \code{"base"} or \code{"ggplot2"}. Default value is \code{"base"}.
}
}
\value{
\item{ib}{
A ggplot object containing the requested plot. Returned only if \code{graph="ggplot2"}.
}
The function produces a plot of the distribution of the Incremental Benefit for a given 
value of the willingness to pay parameter. The dashed area indicates the positive part 
of the distribution (ie when the reference is more cost-effective than the comparator).
}
\references{
Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity Analysis in Health Economics. 
Statistical Methods in Medical Research doi:10.1177/0962280211419832.

Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall, London
}
\author{
Gianluca Baio, Andrea Berardi
}
\seealso{
\code{\link{bcea}},
\code{\link{ib.plot}},
\code{\link{ceplane.plot}}
}
\keyword{Health economic evaluation}
