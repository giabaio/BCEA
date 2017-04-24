\name{ceac.plot}
\alias{ceac.plot}
\title{
Cost-Effectiveness Acceptability Curve (CEAC) plot
}
\description{
Produces a plot of the Cost-Effectiveness Acceptability Curve (CEAC) against the 
willingness to pay threshold
}
\usage{
ceac.plot(he, comparison = NULL, pos = c(1,0), 
          graph = c("base","ggplot2"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{he}{
A \code{bcea} object containing the results of the Bayesian modelling and the economic 
evaluation.
}
  \item{comparison}{
Selects the comparator, in case of more than two interventions being analysed. Default 
as NULL plots all the comparisons together. Any subset of the possible comparisons can 
be selected (e.g., \code{comparison=c(1,3)} or 
\code{comparison=2}).
  }
    \item{pos}{
Parameter to set the position of the legend (only relevant for multiple interventions, 
ie more than 2 interventions being compared). Can be given in form of a string 
\code{(bottom|top)(right|left)} for base graphics and \code{bottom}, \code{top}, 
\code{left} or \code{right} for ggplot2. It can be a two-elements vector, which 
specifies the relative position on the x and y axis respectively, or alternatively it 
can be in form of a logical variable, with \code{FALSE} indicating to use the default 
position and \code{TRUE} to place it on the bottom of the plot. Default value is 
\code{c(1,0)}, that is the bottomright corner inside the plot area.}
  \item{graph}{
A string used to select the graphical engine to use for plotting. Should 
(partial-)match the two options \code{"base"} or \code{"ggplot2"}. Default value is 
\code{"base"}.}
}
\value{
\item{ceac}{
  A ggplot object containing the plot. Returned only if \code{graph="ggplot2"}.
}
The function produces a plot of the cost-effectiveness acceptability curve against the 
discrete grid of possible values for the willingness to pay parameter. Values of the CEAC 
closer to 1 indicate that uncertainty in the cost-effectiveness of the reference 
intervention is very low. Similarly, values of the CEAC closer to 0 indicate that uncertainty 
in the cost-effectiveness of the comparator is very low.
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
\code{\link{bcea}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Health economic evaluation}
\keyword{Cost Effectiveness Acceptability Curve}% __ONLY ONE__ keyword per line
