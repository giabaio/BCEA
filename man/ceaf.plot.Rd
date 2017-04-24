\name{ceaf.plot}
\alias{ceaf.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cost-Effectiveness Acceptability Frontier (CEAF) plot 
}
\description{
Produces a plot the Cost-Effectiveness Acceptability Frontier (CEAF) against the 
willingness to pay threshold
}
\usage{
ceaf.plot(mce, graph=c("base","ggplot2"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mce}{
The output of the call to the function \code{\link{multi.ce}}}
  \item{graph}{
A string used to select the graphical engine to use for plotting. Should 
(partial-)match the two options \code{"base"} or \code{"ggplot2"}. Default value 
is \code{"base"}.
  }
}
\value{
\item{ceaf}{
  A ggplot object containing the plot. Returned only if \code{graph="ggplot2"}.
}
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
\code{\link{bcea}}, \code{\link{multi.ce}}
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
\donttest{
mce <- multi.ce(m)          # uses the results of the economic analysis 
}
#
\donttest{
ceaf.plot(mce)              # plots the CEAF 
}
#
\donttest{
ceaf.plot(mce, graph="g")   # uses ggplot2 
}

\donttest{
# Use the smoking cessation dataset
data(Smoking)
m <- bcea(e,c,ref=4,intervention=treats,Kmax=500,plot=FALSE)
mce <- multi.ce(m)
ceaf.plot(mce)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Health economic evaluation}
\keyword{Multiple comparison}
