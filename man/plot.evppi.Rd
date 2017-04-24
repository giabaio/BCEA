\name{plot.evppi}
\alias{plot.evppi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot.evppi}
\description{
Plots a graph of the Expected Value of Partial Information with respect
to a set of parameters
}
\usage{
\method{plot}{evppi}(x, pos = c(0, 0.8), graph = c("base", "ggplot2"), 
col = NULL,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object in the class \code{evppi}, obtained by the call to the function 
\code{\link{evppi}}.
}
  \item{pos}{
Parameter to set the position of the legend. Can be given in form of a string 
\code{(bottom|top)(right|left)} for base graphics and \code{bottom|top|left|right} 
for ggplot2. It can be a two-elements vector, which specifies the relative position on 
the x and y axis respectively, or alternatively it can be in form of a logical variable, 
with \code{FALSE} indicating to use the default position and \code{TRUE} to place it on 
the bottom of the plot. Default value is \code{c(0,1)}, that is in the topleft corner 
inside the plot area.
}
  \item{graph}{
A string used to select the graphical engine to use for plotting. Should 
(partial-)match the two options \code{"base"} or \code{"ggplot2"}. Default value is 
\code{"base"}.
}
  \item{col}{
Sets the color for the lines depicted in the graph.
}
\item{...}{
Arguments to be passed to methods, such as graphical parameters (see 
\code{\link{par}}).
}
}
\references{
Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall, London
}
\author{
Gianluca Baio, Andrea Berardi}
\seealso{
\code{\link{bcea}}, 
\code{\link{evppi}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Health economic evaluation}
\keyword{Expected value of information}

