\name{evi.plot}
\alias{evi.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Expected Value of Information (EVI) plot
}
\description{
Plots the Expected Value of Information (EVI) against the willingness to pay
}
\usage{
evi.plot(he, graph=c("base","ggplot2"))
}
\arguments{
  \item{he}{
A \code{bcea} object containing the results of the Bayesian modelling and the economic 
evaluation.
}
  \item{graph}{
A string used to select the graphical engine to use for plotting. Should 
(partial-)match the two options \code{"base"} or \code{"ggplot2"}. Default value 
is \code{"base"}.
  }
}
\value{
\item{evi}{
A ggplot object containing the requested plot. Returned only if \code{graph="ggplot2"}.
}
The function produces a plot of the Expected Value of Information as a function of the 
discrete grid approximation of the willingness to pay parameter. The break even point(s) 
(i.e. the point in which the EIB=0, ie when the optimal decision changes from one 
intervention to another) is(are) also showed.
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
\code{\link{ceac.plot}},
\code{\link{ceplane.plot}}
}
\keyword{Health economic evaluation}
\keyword{Expected value of information}

