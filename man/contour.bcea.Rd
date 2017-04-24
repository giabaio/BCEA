\name{contour.bcea}
\alias{contour.bcea}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Contour method for objects in the class \code{bcea}
}
\description{
Produces a scatterplot of the cost-effectiveness plane, with a contour-plot of the 
bivariate density of the differentials of cost (y-axis) and effectiveness (x-axis)
}
\usage{
\method{contour}{bcea}(x, comparison = 1, scale = 0.5, nlevels = 4, levels = NULL, 
  pos = c(1,0), xlim=NULL, ylim=NULL, graph=c("base","ggplot2"), ...)
}

\arguments{
  \item{x}{
A \code{bcea} object containing the results of the Bayesian modelling and the economic 
evaluation
}
  \item{comparison}{
In case of more than 2 interventions being analysed, selects which plot should be made. 
By default the first comparison among the possible ones will be plotted. If 
\code{graph="ggplot2"} any subset of the possible comparisons can be selected, and 
\code{comparison=NULL} will yield a plot of all the possible comparisons together.
}
  \item{scale}{
Scales the plot as a function of the observed standard deviation.
}
  \item{levels}{
Numeric vector of levels at which to draw contour lines. Will be ignored using 
\code{graph="ggplot2"}.
}
  \item{nlevels}{
Number of levels to be plotted in the contour.
}
  \item{pos}{
Parameter to set the position of the legend. Can be given in form of a string 
\code{(bottom|top)(right|left)} for base graphics and \code{bottom}, \code{top}, 
\code{left} or \code{right} for ggplot2. It can be a two-elements vector, which 
specifies the relative position on the x and y axis respectively, or alternatively it
 can be in form of a logical variable, with \code{FALSE} indicating to use the default 
position and \code{TRUE} to place the legend on the bottom of the plot. Default value is 
\code{c(1,0)}, that is the bottomright corner inside the plot area.
  }
  \item{graph}{
  A string used to select the graphical engine to use for plotting. Should (partial-)match 
the two options \code{"base"} or \code{"ggplot2"}. Default value is \code{"base"}.
  }
  \item{xlim}{The range of the plot along the x-axis. If NULL (default) it is determined
by the range of the simulated values for \code{delta.e}}
  \item{ylim}{The range of the plot along the y-axis. If NULL (default) it is determined
by the range of the simulated values for \code{delta.c}}
\item{...}{
Additional arguments to 'plot.window', 'title', 'Axis' and 'box', typically graphical 
parameters such as 'cex.axis'. Will be ignored if \code{graph="ggplot2"}.
}
}
\value{
\item{ceplane}{
A ggplot object containing the plot. Returned only if \code{graph="ggplot2"}.
}
Plots the cost-effectiveness plane with a scatterplot of all the simulated values from 
the (posterior) bivariate distribution of (Delta_e,Delta_c), the differentials of 
effectiveness and costs; superimposes a contour of the distribution and prints the 
estimated value of the probability of each quadrant (combination of positive/negative 
values for both Delta_e and Delta_c) 
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
\code{\link{ceplane.plot}},
\code{\link{contour2}}
}
\keyword{Health economic evaluation}
\keyword{Bayesian model}
