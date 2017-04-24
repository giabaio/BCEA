\name{plot.CEriskav}
\alias{plot.CEriskav}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summary plot of the health economic analysis when risk aversion is included
}
\description{
Plots the EIB and the EVPI when risk aversion is included in the utility function
}
\usage{
\method{plot}{CEriskav}(x, pos=c(0,1), graph=c("base","ggplot2"), ...)
%%%plot.CEriskav(x, y, ...)
}
\arguments{
  \item{x}{
An object of the class \code{CEriskav}, containing the results of the economic 
analysis performed accounting for a risk aversion parameter (obtained as output of 
the function \code{\link{CEriskav}}).
}
  \item{pos}{
Parameter to set the position of the legend. Can be given in form of a string 
\code{(bottom|top)(right|left)} for base graphics and \code{bottom|top|left|right} 
for ggplot2. It can be a two-elements vector, which specifies the relative position 
on the x and y axis respectively, or alternatively it can be in form of a logical 
variable, with \code{FALSE} indicating to use the default position and \code{TRUE} 
to place it on the bottom of the plot. Default value is \code{c(0,1)}, that is in 
the topleft corner inside the plot area.
  }
  \item{graph}{
A string used to select the graphical engine to use for plotting. Should 
(partial-)match the two options \code{"base"} or \code{"ggplot2"}. Default value 
is \code{"base"}.
  }
\item{...}{
Arguments to be passed to methods, such as graphical parameters (see 
\code{\link{par}}).
}
}

\value{
\item{list(eib,evi)}{A two-elements named list of the ggplot objects containing 
the requested plots. Returned only if \code{graph="ggplot2"}.}
The function produces two plots for the risk aversion analysis. The first one is 
the EIB as a function of the discrete grid approximation of the willingness parameter 
for each of the possible values of the risk aversion parameter, r. The second one is 
a similar plot for the EVPI.
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
\code{\link{CEriskav}}
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
      plot=FALSE            # inhibits graphical output
)
#
# Define the vector of values for the risk aversion parameter, r, eg:
r <- c(0.000000000001,0.005,0.020,0.035) 
#
# Run the cost-effectiveness analysis accounting for risk aversion
\donttest{
cr <- CEriskav(m,     # uses the results of the economic evalaution 
                      #  (a "bcea" object)
        r=r,          # defines the vector of values for the risk 
                      #  aversion parameter
        comparison=1  # if more than 2 interventions, selects the 
                      #  pairwise comparison 
)
}
#
# Now produce the plots
\donttest{
plot(cr # uses the results of the risk aversion 
        #  analysis (a "CEriskav" object)
)
}
### Alternative options, using ggplot2
\donttest{
plot(cr,
  graph="ggplot2",
  plot="ask"          # plot option can be specified as
                      #  "dev.new" (default), "x11" or "ask"
  )
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Health economic evaluation}% __ONLY ONE__ keyword per line
\keyword{Risk aversion}
