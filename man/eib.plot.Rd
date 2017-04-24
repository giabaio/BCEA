\name{eib.plot}
\alias{eib.plot}
\title{
Expected Incremental Benefit (EIB) plot
}
\description{
Produces a plot of the Expected Incremental Benefit (EIB) as a function of the 
willingness to pay
}
\usage{
eib.plot(he, comparison=NULL, pos=c(1,0), size=NULL, 
         plot.cri = NULL, graph=c("base","ggplot2"), ...)
}
\arguments{
  \item{he}{
A \code{bcea} object containing the results of the Bayesian modelling and the economic
 evaluation.
}
  \item{comparison}{
Selects the comparator, in case of more than two interventions being analysed. 
Default as \code{NULL} plots all the comparisons together. Any subset of the possible 
comparisons can be selected (e.g., \code{comparison=c(1,3)} or \code{comparison=2}).
  }
  \item{pos}{
Parameter to set the position of the legend; for a single comparison plot, the ICER 
legend position. Can be given in form of a string \code{(bottom|top)(right|left)} 
for base graphics and \code{bottom|top|left|right} for ggplot2. It can be a 
two-elements vector, which specifies the relative position on the x and y axis 
respectively, or alternatively it can be in form of a logical variable, with 
\code{FALSE} indicating to use the default position and \code{TRUE} to place it 
on the bottom of the plot. Default value is \code{c(1,0)}, that is the bottomright 
corner inside the plot area.
}
  \item{size}{
Value (in millimetres) of the size of the willingness to pay label. Used only if 
\code{graph="ggplot2"}, otherwise it will be ignored with a message. If set to 
\code{NA}, the break-even point line(s) and label(s) are suppressed, with both base 
graphics and ggplot2.
  }
  \item{plot.cri}{
Logical value. Should the credible intervals be plotted along with the expected 
incremental benefit? Default as \code{NULL} draws the 95\% credible intervals if 
only one comparison is selected, and does not include them for multiple comparisons. 
Setting \code{plot.cri=TRUE} or \code{plot.cri=FALSE} forces the function to add 
the intervals or not. The level of the intervals can be also set, see \ldots for more 
details.
  }
  \item{graph}{
A string used to select the graphical engine to use for plotting. Should (partial-)match 
the two options \code{"base"} or \code{"ggplot2"}. Default value is \code{"base"}.
  }
  \item{...}{
If \code{graph="ggplot2"} and a named theme object is supplied, it will be added to 
the ggplot object. If \code{plot.cri=TRUE} the level of the interval can be set using 
the argument \code{alpha}, with default at \code{alpha=0.05}. Additionally the method 
of calculation of the credible intervals can be chosen with the option 
\code{cri.quantile}: the default value \code{TRUE} indicates that the credible intervals 
are defined as the interval between the \code{alpha/2}-th and \code{1-alpha/2}-th 
quantiles of the IB distribution. Setting \code{cri.quantile=FALSE} will use a 
normal approximation on the IB distribution to calculate the intervals.
  }
}
\value{
\item{eib}{
A ggplot object containing the requested plot. Returned only if \code{graph="ggplot2"}.
}
The function produces a plot of the Expected Incremental Benefit as a function of the 
discrete grid approximation of the willingness to pay parameter. The break even point 
(i.e. the point in which the EIB=0, ie when the optimal decision changes from one 
intervention to another) is also showed by default. The value k* is the discrete 
grid approximation of the ICER.
}
\references{
Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity Analysis in Health Economics. 
Statistical Methods in Medical Research doi:10.1177/0962280211419832.

Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall, London
}

\author{
Gianluca Baio, Andrea Berardi
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{bcea}},
\code{\link{ib.plot}},
\code{\link{ceplane.plot}}
}
\keyword{Health economic evaluation}
\keyword{Expected Incremental Benefit}
