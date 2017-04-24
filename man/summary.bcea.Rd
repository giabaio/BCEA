\name{summary.bcea}
\alias{summary.bcea}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summary method for objects in the class \code{bcea}
}
\description{
Produces a table printout with some summary results of the health economic 
evaluation
}
\usage{
\method{summary}{bcea}(object, wtp = 25000, ...)
%%summary(object, wtp = 25000, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
A \code{bcea} object containing the results of the Bayesian modelling and the 
economic evaluation.
}
  \item{wtp}{
The value of the willingness to pay threshold to be used in the summary table.
}
\item{...}{
Additional arguments affecting the summary produced.
}
}

\value{
Prints a summary table with some information on the health economic output and 
synthetic information on the economic measures (EIB, CEAC, EVPI).
}
\references{
Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity Analysis in Health Economics. 
Statistical Methods in Medical Research doi:10.1177/0962280211419832.

Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall, London
}
\author{
Gianluca Baio
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{bcea}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Health economic evaluation}% __ONLY ONE__ keyword per line
