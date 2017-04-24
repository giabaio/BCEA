\name{ceplane.plot}
\alias{ceplane.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cost-effectiveness plane plot
}
\description{
Produces a scatter plot of the cost-effectiveness plane, together with the sustainability
 area, as a function of the selected willingness to pay threshold
}
\usage{
ceplane.plot(he, comparison = NULL, wtp = 25000, pos=c(1,1), 
size=NULL, graph=c("base","ggplot2"), 
xlim=NULL, ylim=NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{he}{
A \code{bcea} object containing the results of the Bayesian modelling and the economic 
evaluation.
}
  \item{comparison}{
Selects the comparator, in case of more than two interventions being analysed. Default as 
\code{NULL} plots all the comparisons together. Any subset of the possible comparisons can 
be selected (e.g., \code{comparison=c(1,3)} or 
\code{comparison=2}).
}
  \item{wtp}{
The value of the willingness to pay parameter. Not used iff \code{graph="base"} for 
multiple comparisons.
}
  \item{pos}{
Parameter to set the position of the legend; for a single comparison plot, the ICER 
legend position. Can be given in form of a string \code{(bottom|top)(right|left)} for 
base graphics and \code{bottom|top|left|right} for ggplot2. It can be a two-elements 
vector, which specifies the relative position on the x and y axis respectively, or 
alternatively it can be in form of a logical variable, with \code{FALSE} indicating to 
use the default position and \code{TRUE} to place it on the bottom of the plot. Default 
value is \code{c(1,1)}, that is the topright corner inside the plot area.
  }
  \item{size}{
Value (in millimetres) of the size of the willingness to pay label. Used only if \code{graph="ggplot2"}, otherwise is ignored with a message.
  }
  \item{graph}{
A string used to select the graphical engine to use for plotting. Should 
(partial-)match the two options \code{"base"} or \code{"ggplot2"}. Default value is 
\code{"base"}.
  }
  \item{xlim}{The range of the plot along the x-axis. If NULL (default) it is determined
by the range of the simulated values for \code{delta.e}}
  \item{ylim}{The range of the plot along the y-axis. If NULL (default) it is determined
by the range of the simulated values for \code{delta.c}}
  \item{...}{
If \code{graph="ggplot2"} and a named theme object is supplied, it will be added to the 
ggplot object. In addition, if \code{graph="ggplot2"}, \code{ICER.size} can be used to 
resize the red dot representing the ICER (i.e. showing them if multiple comparisons are 
selected), and \code{label.pos=FALSE} will place the willingness to pay label in a 
different position at the bottom of the graph.
  }
}
\value{
\item{ceplane}{
A ggplot object containing the plot. Returned only if \code{graph="ggplot2"}.
}
The function produces a plot of the cost-effectiveness plane. Grey dots show the 
simulated values for the joint distribution of the effectiveness and cost differentials. 
The larger red dot shows the ICER and the grey area identifies the sustainability area, 
i.e. the part of the plan for which the simulated values are below the willingness to pay 
threshold. The proportion of points in the sustainability area effectively represents the 
CEAC for a given value of the willingness to pay. If the comparators are more than 2 and 
no pairwise comparison is specified, all scatterplots are graphed using different colors.
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
\code{\link{bcea}}
}
\examples{
### create the bcea object m for the smoking cessation example
data(Smoking)
m <- bcea(e,c,ref=4,Kmax=500,interventions=treats)
### produce the plot
ceplane.plot(m,wtp=200,graph="base")
### select only one comparator
ceplane.plot(m,wtp=200,graph="base",comparator=3)
### or use ggplot2 instead
if(requireNamespace("ggplot2")){
ceplane.plot(m,wtp=200,pos="right",ICER.size=2,graph="ggplot2")
}
}
\keyword{Health economic evaluation}
\keyword{Cost Effectiveness Plane}
