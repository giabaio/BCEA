\name{multi.ce}
\alias{multi.ce}
\title{
Cost-effectiveness analysis with multiple comparison
}
\description{
Computes and plots the probability that each of the n_int interventions being analysed 
is the most cost-effective and the cost-effectiveness acceptability frontier
}
\usage{
multi.ce(he)
}
\arguments{
  \item{he}{
A \code{bcea} object containing the results of the Bayesian modelling and the economic 
evaluation.
}
}
\value{
\item{m.ce}{A matrix including the probability that each intervention is the most 
cost-effective for all values of the willingness to pay parameter}
\item{ceaf}{A vector containing the cost-effectiveness acceptability frontier}
}
\author{
Gianluca Baio
}

\seealso{
\code{\link{bcea}},
\code{\link{mce.plot}},
\code{\link{ceaf.plot}}
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
mce <- multi.ce(m           # uses the results of the economic analysis 
)
}

\keyword{Health economic evaluation}
\keyword{Multiple comparison}
