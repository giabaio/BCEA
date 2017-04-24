\name{mce.plot}
\alias{mce.plot}
\title{
Plots the probability that each intervention is the most cost-effective
}
\description{
Plots the probability that each of the n_int interventions being analysed is the most 
cost-effective.
}
\usage{
mce.plot(mce,pos=c(1,0.5),graph=c("base","ggplot2"))
}
\arguments{
  \item{mce}{
The output of the call to the function \code{\link{multi.ce}}.
}
  \item{pos}{
Parameter to set the position of the legend. Can be given in form of a string 
\code{(bottom|top)(right|left)} for base graphics and \code{bottom|top|left|right} 
for ggplot2. It can be a two-elements vector, which specifies the relative position on 
the x and y axis respectively, or alternatively it can be in form of a logical variable, 
with \code{TRUE} indicating to use the first standard and \code{FALSE} to use the second 
one. Default value is \code{c(1,0.5)}, that is on the right inside the plot area.
  }
  \item{graph}{
A string used to select the graphical engine to use for plotting. Should 
(partial-)match the two options \code{"base"} or \code{"ggplot2"}. Default value is 
\code{"base"}.
  }
}
\value{
\item{mceplot}{
A ggplot object containing the plot. Returned only if \code{graph="ggplot2"}.
}
}
\author{
Gianluca Baio, Andrea Berardi
}
\references{
Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity Analysis in Health Economics. 
Statistical Methods in Medical Research doi:10.1177/0962280211419832.

Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall, London
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{bcea}}
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
mce <- multi.ce(m)          # uses the results of the economic analysis 
#
mce.plot(mce,               # plots the probability of being most cost-effective
      graph="base")         #  using base graphics
#
if(require(ggplot2)){
mce.plot(mce,               # the same plot
      graph="ggplot2")      #  using ggplot2 instead
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Health economic evaluation}
\keyword{Multiple comparison}
