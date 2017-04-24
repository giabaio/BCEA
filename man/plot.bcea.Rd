\name{plot.bcea}
\alias{plot.bcea}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summary plot of the health economic analysis
}
\description{
Plots in a single graph the Cost-Effectiveness plane, the Expected Incremental Benefit, 
the CEAC and the EVPI
}
\usage{
\method{plot}{bcea}(x, comparison=NULL, wtp=25000, pos=FALSE, 
graph=c("base","ggplot2"), ...)
}
\arguments{
\item{x}{
A \code{bcea} object containing the results of the Bayesian modelling and the economic 
evaluation.
}
\item{comparison}{
Selects the comparator, in case of more than two interventions being analysed. The value 
is passed to \code{\link{ceplane.plot}}, \code{\link{eib.plot}} and \code{\link{ceac.plot}}.
}
\item{wtp}{
The value of the willingness to pay parameter. It is passed to \code{\link{ceplane.plot}}.
}
\item{pos}{
Parameter to set the position of the legend. Can be given in form of a string, a single 
logical value, or a two-element vector with the respective relative positions on the x 
and y axis. Default as \code{FALSE} sets the legend position to the default one for each 
plot (see the details section), while \code{TRUE} puts it on the bottom of each plot. 
Changes will affect all the individual plots.
}
\item{graph}{
A string used to select the graphical engine to use for plotting. Should 
(partial-)match the two options \code{"base"} or \code{"ggplot2"}. Default value 
is \code{"base"}.
}
\item{...}{
Arguments to be passed to the methods \code{\link{ceplane.plot}} and 
\code{\link{eib.plot}}. Please see the manual pages for the individual functions. 
Arguments like \code{size}, \code{ICER.size} and \code{plot.cri} can be supplied to 
the functions in this way. In addition if \code{graph="ggplot2"} and the arguments 
are named theme objects they will be added to each plot.
}
}

\value{
The function produces a plot with four graphical summaries of the health economic 
evaluation.
}
\details{
The default position of the legend for the cost-effectiveness plane (produced by 
\code{\link{ceplane.plot}}) is set to \code{c(1,1.025)} overriding its default for 
\code{pos=FALSE}, since multiple ggplot2 plots are rendered in a slightly different 
way than single plots.

For more information see the documentation of each individual plot function.
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
\code{\link{eib.plot}},
\code{\link{ceac.plot}},
\code{\link{evi.plot}}
}
\examples{
# See Baio G., Dawid A.P. (2011) for a detailed description of the 
# Bayesian model and economic problem
#
# Load the processed results of the MCMC simulation model
data(Vaccine)
# 
# Runs the health economic evaluation using BCEA
m <- bcea(e=e,c=c,          # defines the variables of 
                            #  effectiveness and cost
      ref=2,                # selects the 2nd row of (e,c) 
                            #  as containing the reference intervention
      interventions=treats, # defines the labels to be associated 
                            #  with each intervention
      Kmax=50000,           # maximum value possible for the willingness 
                            #  to pay threshold; implies that k is chosen 
                            #  in a grid from the interval (0,Kmax)
      plot=FALSE            # does not produce graphical outputs
)
#
# Plots the summary plots for the "bcea" object m using base graphics
plot(m,graph="base")

# Plots the same summary plots using ggplot2
if(require(ggplot2)){
plot(m,graph="ggplot2")

##### Example of a customized plot.bcea with ggplot2
plot(m,
  graph="ggplot2",                                      # use ggplot2
  theme=theme(plot.title=element_text(size=rel(1.25))), # theme elements must have a name
  ICER.size=1.5,                                        # hidden option in ceplane.plot
  size=rel(2.5)                                         # modifies the size of k= labels
)                                                       #  in ceplane.plot and eib.plot
}
}
\keyword{Health economic evaluation}
