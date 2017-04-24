\name{ceef.plot}
\alias{ceef.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cost-Effectiveness Efficiency Frontier (CEAF) plot
}
\description{
Produces a plot of the Cost-Effectiveness Efficiency Frontier (CEEF)
}
\usage{
ceef.plot(he, comparators = NULL, pos = c(1, 1), 
start.from.origins = TRUE, threshold = NULL, flip = FALSE, 
dominance = TRUE, relative = FALSE, print.summary = TRUE, 
graph = c("base", "ggplot2"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{he}{
A \code{bcea} object containing the results of the Bayesian modelling and the 
economic evaluation. The list needs to include the \code{e} and \code{c} matrices 
used to generate the object; see Details.
}
  \item{comparators}{
Vector specifying the comparators to be included in the frontier analysis. Must be 
of length > 1. Default as \code{NULL} includes all the available comparators.
}
  \item{pos}{
Parameter to set the position of the legend. Can be given in form of a string 
\code{(bottom|top)(right|left)} for base graphics and \code{bottom}, \code{top}, 
\code{left} or \code{right} for ggplot2. It can be a two-elements vector, which 
specifies the relative position on the x and y axis respectively, or alternatively 
it can be in form of a logical variable, with \code{FALSE} indicating to use the 
default position and \code{TRUE} to place it on the bottom of the plot. Default 
value is \code{c(1,1)}, that is the topright corner inside the plot area.}
  \item{start.from.origins}{
Logical. Should the frontier start from the origins of the axes? The argument is 
reset to \code{FALSE} if the average effectiveness and/or costs of at least one 
comparator are negative.
}
  \item{threshold}{
Specifies if the efficiency should be defined based on a willingness-to-pay threshold 
value. If set to \code{NULL} (the default), no conditions are included on the slope 
increase. If a positive value is passed as argument, to be efficient an intervention 
also requires to have an ICER for the comparison versus the last efficient strategy 
not greater than the specified threshold value. A negative value will be ignored with 
a warning.
}
  \item{flip}{
Logical. Should the axes of the plane be inverted?
}
  \item{dominance}{
Logical. Should the dominance regions be included in the plot?
}
  \item{relative}{
Logical. Should the plot display the absolute measures (the default as \code{FALSE}) 
or the differential outcomes versus the reference comparator?
  }
  \item{print.summary}{
Logical. Should the efficiency frontier summary be printed along with the graph? 
See Details for additional information.
  }
  \item{graph}{
A string used to select the graphical engine to use for plotting. Should (partial-)match 
the two options \code{"base"} or \code{"ggplot2"}. Default value is \code{"base"}.
}
  \item{\dots}{
If \code{graph="ggplot2"} and a named theme object is supplied, it will be added to 
the ggplot object. Ignored if \code{graph="base"}. Setting the optional argument 
\code{include.ICER} to \code{TRUE} will print the ICERs in the summary tables, 
if produced.}
}
\details{
The \code{bcea} objects did not include the generating \code{e} and \code{c} matrices 
in BCEA versions <2.1-0. This function is not compatible with objects created with 
previous versions. The matrices can be appended to \code{bcea} objects obtained using 
previous versions, making sure that the class of the object remains unaltered.

The argument \code{print.summary} allows for printing a brief summary of the efficiency 
frontier, with default to \code{TRUE}. Two tables are plotted, one for the interventions 
included in the frontier and one for the dominated interventions. The average costs and 
clinical benefits are included for each intervention. The frontier table includes the 
slope for the increase in the frontier and the non-frontier table displays the dominance 
type of each dominated intervention. Please note that the slopes are defined as the 
increment in the costs for a unit increment in the benefits even if \code{flip = TRUE} 
for consistency with the ICER definition. The angle of increase is in radians and depends 
on the definition of the axes, i.e. on the value given to the \code{flip} argument.

If the argument \code{relative} is set to \code{TRUE}, the graph will not display the 
absolute measures of costs and benefits. Instead the axes will represent differential 
costs and benefits compared to the reference intervention (indexed by \code{ref} in 
the \code{\link{bcea}} function). 
}
\value{
\item{ceplane}{
A ggplot object containing the plot. Returned only if \code{graph="ggplot2"}.
}
The function produces a plot of the cost-effectiveness efficiency frontier. The dots 
show the simulated values for the intervention-specific distributions of the 
effectiveness and costs. The circles indicate the average of each bivariate 
distribution, with the numbers referring to each included intervention. The numbers 
inside the circles are black if the intervention is included in the frontier and grey 
otherwise. If the option \code{dominance} is set to \code{TRUE}, the dominance regions 
are plotted, indicating the areas of dominance. Interventions in the areas between 
the dominance region and the frontier are in a situation of extended dominance.
}
\references{
Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall, London.

IQWIG (2009). General methods for the Assessment of the Relation of Benefits to Cost, 
Version 1.0. IQWIG, November 2009.
}
\author{
Andrea Berardi, Gianluca Baio
}
\seealso{
\code{\link{bcea}}
}
\examples{
### create the bcea object m for the smoking cessation example
data(Smoking)
m <- bcea(e,c,ref=4,Kmax=500,interventions=treats)
### produce the plot
ceef.plot(m,graph="base")
\donttest{
### tweak the options
ceef.plot(m,flip=TRUE,dominance=FALSE,start.from.origins=FALSE,
          print.summary=FALSE,graph="base")
### or use ggplot2 instead
if(require(ggplot2)){
ceef.plot(m,dominance=TRUE,start.from.origins=FALSE,pos=TRUE,
          print.summary=FALSE,graph="ggplot2")
}
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Health economic evaluation}
\keyword{Multiple comparisons}
