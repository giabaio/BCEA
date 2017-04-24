\name{sim.table}
\alias{sim.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Table of simulations for the health economic model
}
\description{
Using the input in the form of MCMC simulations and after having run the health 
economic model, produces a summary table of the simulations from the cost-effectiveness 
analysis 
}
\usage{
sim.table(he, wtp = 25000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{he}{
A \code{bcea} object containing the results of the Bayesian modelling and the economic 
evaluation.
}
  \item{wtp}{
The value of the willingness to pay threshold to be used in the summary table.
}
}

\value{
Produces the following elements:
\item{Table}{A table with the simulations from the economic model}
\item{names.cols}{A vector of labels to be associated with each column of the table}
\item{wtp}{The selected value of the willingness to pay}
\item{ind.table}{The index associated with the selected value of the willingness to pay 
threshold in the grid used to run the analysis}
}
\references{
Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity Analysis in Health Economics. 
Statistical Methods in Medical Research doi:10.1177/0962280211419832.

Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall, London
}
\author{
Gianluca Baio
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
      Kmax=50000            # maximum value possible for the willingness 
                            #  to pay threshold; implies that k is chosen 
                            #  in a grid from the interval (0,Kmax)
)
#
# Now can save the simulation exercise in an object using sim.table()
st <- sim.table(m, # uses the results of the economic evalaution 
                   #  (a "bcea" object)
        wtp=25000  # selects the particular value for k
)
#
# The table can be explored. For example, checking the 
#  element 'Table' of the object 'st'
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Health economic evaluation} % __ONLY ONE__ keyword per line
