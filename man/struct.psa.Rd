\name{struct.psa}
\alias{struct.psa}

\title{
Structural PSA
}
\description{
Computes the weights to be associated with a set of competing models in order 
to perform structural PSA
}
\usage{
struct.psa(models, effect, cost, ref = 1, interventions = NULL, 
           Kmax = 50000, plot = F)
}

\arguments{
  \item{models}{
A list containing the output from either R2jags or R2OpenBUGS/R2WinBUGS for all 
the models that need to be combined in the model average
}
  \item{effect}{
A list containing the measure of effectiveness computed from the  various models 
(one matrix with n.sim x n.ints simulations for each model)
}
  \item{cost}{
A list containing the measure of costs computed from the  various models 
(one matrix with n.sim x n.ints simulations for each model)
}
  \item{ref}{
Defines which intervention is considered to be the reference strategy. The default 
value \code{ref=1} means that the intervention appearing first is the reference and 
the other(s) is(are) the comparator(s)
}
  \item{interventions}{
Defines the labels to be associated with each intervention. By default and 
if \code{NULL}, assigns labels in the form "Intervention1", ... , "Intervention T"
}
  \item{Kmax}{
Maximum value of the willingness to pay to be considered. Default value is 
\code{k=50000}. The willingness to pay is then approximated on a discrete grid in 
the interval \code{[0,Kmax]}. The grid is equal to \code{wtp} if the parameter is 
given, or composed of \code{501} elements if \code{wtp=NULL} (the default)
}
  \item{plot}{
A logical value indicating whether the function should produce the summary 
plot or not
}
}
\references{
Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall, London
}
\author{
Gianluca Baio
}
\seealso{
\code{\link{bcea}}
}
