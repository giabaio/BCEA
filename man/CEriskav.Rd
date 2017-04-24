\name{CEriskav}
\alias{CEriskav}
\alias{CEriskav.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cost-effectiveness analysis including a parameter of risk aversion
}
\description{
Extends the standard cost-effectiveness analysis to modify the utility function so 
that risk aversion of the decision maker is explicitly accounted for
}
\usage{
CEriskav(he, r = NULL, comparison = 1)

\method{CEriskav}{default}(he, r = NULL, comparison = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{he}{
A \code{bcea} object containing the results of the Bayesian modelling and the 
economic evaluation.
}
  \item{r}{
A vector of values for the risk aversion parameter. If \code{NULL}, default values 
are assigned by R. The first (smallest) value (\code{r} -> 0) produces the standard 
analysis with no risk aversion.
}
  \item{comparison}{
In case of more than 2 interventions being analysed, selects which plot should be made. 
By default the first possible choice is selected as the comparator.
}
}
\value{
An object of the class \code{CEriskav} containing the following elements:
 \item{Ur}{An array containing the simulated values for all the ''known-distribution'' 
utilities for all interventions, all the values of the willingness to pay parameter and 
for all the possible values of \code{r}}
\item{Urstar}{
An array containing the simulated values for the maximum ''known-distribution'' expected 
utility for all the values of the willingness to pay parameter and for all the possible 
values of \code{r}}
\item{IBr}{
An array containing the simulated values for the distribution of the Incremental Benefit 
for all the values of the willingness to pay and for all the possible values of \code{r}}
\item{eibr}{
An array containing the Expected Incremental Benefit for each value of the willingness 
to pay parameter and for all the possible values of \code{r}}
\item{vir}{
An array containing all the simulations for the Value of Information for each value 
of the willingness to pay parameter and for all the possible values of \code{r}}
\item{evir}{
An array containing the Expected Value of Information for each value of the willingness 
to pay parameter and for all the possible values of \code{r}}
\item{R}{
The number of possible values for the parameter of risk aversion \code{r}}
\item{r}{
The vector containing all the possible values for the parameter of risk aversion \code{r}}
}
\references{
Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity Analysis in Health Economics. 
Statistical Methods in Medical Research doi:10.1177/0962280211419832.

Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall, London
}
\author{
Gianluca Baio
}
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
# Define the vector of values for the risk aversion parameter, r, eg:
r <- c(0.0000000001,0.005,0.020,0.035) 
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
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Health economic evaluation}
\keyword{Risk aversion}% __ONLY ONE__ keyword per line
