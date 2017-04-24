\name{plot.mixedAn}
\alias{plot.mixedAn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summary plot of the health economic analysis when the mixed analysis is considered
}
\description{
Compares the optimal scenario to the mixed case in terms of the EVPI
}
\usage{
\method{plot}{mixedAn}(x, y.limits = NULL, pos=c(0,1), graph=c("base","ggplot2"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{x}{
An object of class \code{mixedAn}, given as output of the call to the function 
\code{\link{mixedAn}}.
}
\item{y.limits}{
Range of the y-axis for the graph. The default value is \code{NULL}, in which case the 
maximum range between the optimal and the mixed analysis scenarios is considered.
}
  \item{pos}{
Parameter to set the position of the legend. Can be given in form of a string 
\code{(bottom|top)(right|left)} for base graphics and \code{bottom|top|left|right} 
for ggplot2. It can be a two-elements vector, which specifies the relative position on 
the x and y axis respectively, or alternatively it can be in form of a logical 
variable, with \code{FALSE} indicating to use the default position and \code{TRUE} to 
place it on the bottom of the plot. Default value is \code{c(0,1)}, that is in the 
topleft corner inside the plot area.
  }
  \item{graph}{
A string used to select the graphical engine to use for plotting. Should 
(partial-)match the two options \code{"base"} or \code{"ggplot2"}. Default value is 
\code{"base"}.
  }
\item{...}{
Arguments to be passed to methods, such as graphical parameters (see \code{\link{par}}).
}
}

\value{
\item{evi}{
A ggplot object containing the plot. Returned only if \code{graph="ggplot2"}.
}
The function produces a graph showing the difference between the ''optimal'' version of 
the EVPI (when only the most cost-effective intervention is included in the market) and 
the mixed strategy one (when more than one intervention is considered in the market).
}
\references{
Baio, G. and Russo, P. (2009).A decision-theoretic framework for the application of cost-effectiveness analysis in regulatory processes. Pharmacoeconomics 27(8), 645-655 
doi:10.2165/11310250

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
\code{\link{mixedAn}}
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
ma <- mixedAn(m,        # uses the results of the mixed strategy 
                        #  analysis (a "mixedAn" object)
       mkt.shares=NULL  # the vector of market shares can be defined 
                        #  externally. If NULL, then each of the T 
                        #  interventions will have 1/T market share
)
#
# Can also plot the summary graph
plot(ma,graph="base")
#
# Or with ggplot2
if(require(ggplot2)){
plot(ma,graph="ggplot2")
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Health economic evaluation}
\keyword{Mixed analysis}
