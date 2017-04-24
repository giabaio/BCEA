\name{info.rank}
\alias{info.rank}
\title{
Info-rank plot
}
\description{
Produces a plot similar to a Tornado-plot, but based on the analysis of the EVPPI.
For each parameter and value of the willingness-to-pay threshold, a barchart is 
plotted to describe the ratio of EVPPI (specific to that parameter) to EVPI. This
represents the relative 'importance' of each parameter in terms of the expected 
value of information. 
}
\usage{
info.rank(parameter,input,he,wtp=he$k[min(which(he$k>=he$ICER))],...)
}
\arguments{
  \item{parameter}{
A vector of parameters for which the individual EVPPI should be calculated. This can 
be given as a string (or vector of strings) of names or a numeric vector, 
corresponding to the column numbers of important parameters.
}
  \item{input}{
A matrix containing the simulations for all the parameters monitored by the call to 
JAGS or BUGS. The matrix should have column names matching the names of the parameters 
and the values in the vector parameter should match at least one of those values.
  }
  \item{he}{
A \code{bcea} object (the result of the call to the function \code{\link{bcea}}).
}
  \item{wtp}{
A value of the wtp for which the analysis should be performed. If not 
specified then the break-even point for the current model will be used. 
  }
  \item{...}{
Additional options. These include graphical parameters that the user can specify.
xlim = limits of the x-axis; ca = font size for the axis label (default = 0.7 of
full size); cn = font size for the parameter names vector (default = 0.7 of full 
size); mai = margins of the graph (default = c(1.36,1.5,1,1)); rel = logical 
argument that specifies whether the ratio of EVPPI to EVPI (\code{rel=TRUE}, 
default) or the absolute value of the EVPPI should be used for the analysis.
  }
}
\value{
\item{res}{
A data.frame containing the ranking of the parameters with the value of the 
selected summary, for the chosen wtp.
}
The function produces a 'Info-rank' plot. This is an extension of standard 
'Tornado plots' and presents a ranking of the model parameters in terms of
their impact on the expected value of information. For each parameter, the
specific individual EVPPI is computed and used to measure the impact of 
uncertainty in that parameter over the decision-making process, in terms of
how large the expected value of gaining more information is.
}
\references{
Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity Analysis in Health Economics. 
Statistical Methods in Medical Research doi:10.1177/0962280211419832.

Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall, London
}

\author{
Anna Heath, Gianluca Baio
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{bcea}},
\code{\link{evppi}}
}
\keyword{Health economic evaluation}
\keyword{Probabilistic sensitivity analysis}
