\name{CreateInputs}
\alias{CreateInputs}
\title{
CreateInputs
}
\description{
Creates an object containing the matrix with the parameters simulated using the MCMC 
procedure (using JAGS, BUGS or Stan) and a vector of parameters (strings) that can be 
used to perform the expected value of partial information analysis.
In the process, \code{CreateInputs} also checks for linear dependency among columns
of the PSA samples or columns having constant values and removes them to only leave
the fundamental parameters (to run VoI analysis).
This also deals with simulations stored in a \code{.csv} or \code{.txt} file (eg as
obtained using bootstrapping from a non-Bayesian model)
}
\usage{
CreateInputs(x)
}
\arguments{
  \item{x}{
A \code{rjags}, \code{bugs} or \code{stanfit} object, containing the results of a call 
to either \code{jags}, (under \code{R2jags}), bugs (under \code{R2WinBUGS} or 
\code{R2OpenBUGS}), or \code{stan} (under \code{rstan}). 
}
}
\value{
\item{mat}{A data.frame contaning all the simulations for all the monitored parameters}
\item{parameters}{A character vectors listing the names of all the monitored parameters}
}
\author{
Gianluca Baio and Mark Strong
}
\seealso{
\code{\link{bcea}},
\code{\link{evppi}}
}
\keyword{R2jags}
\keyword{R2WinBUGS}
\keyword{R2OpenBUGS}
