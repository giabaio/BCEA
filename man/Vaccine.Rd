\name{Vaccine}
\alias{Vaccine}
\alias{c}
\alias{cost.GP}
\alias{cost.hosp}
\alias{cost.otc}
\alias{cost.time.off}
\alias{cost.time.vac}
\alias{cost.travel}
\alias{cost.trt1}
\alias{cost.trt2}
\alias{cost.vac}
\alias{e}
\alias{e.pts}
\alias{c.pts}
\alias{vaccine_mat}
\alias{N}
\alias{N.outcomes}
\alias{N.resources}
\alias{QALYs.adv}
\alias{QALYs.death}
\alias{QALYs.hosp}
\alias{QALYs.inf}
\alias{QALYs.pne}
\alias{treats}
\alias{vaccine}
\docType{data}
\title{
Data set for the Bayesian model for the cost-effectiveness of influenza 
vaccination
}
\description{
This data set contains the results of the Bayesian analysis used to model the 
clinical output and the costs associated with an influenza vaccination. 
}
\usage{
data(Vaccine)
}

\format{
A data list including the variables needed for the influenza vaccination. 
The variables are as follows:
 
  \describe{
    \item{\code{c}}{a matrix of simulations from the posterior distribution 
of the overall costs associated with the two treatments}
    \item{\code{cost.GP}}{a matrix of simulations from the posterior distribution 
of the costs for GP visits associated with the two treatments}
    \item{\code{cost.hosp}}{a matrix of simulations from the posterior distribution 
of the costs for hospitalisations associated with the two treatments}
    \item{\code{cost.otc}}{a matrix of simulations from the posterior distribution 
of the costs for over-the-counter medications associated with the two treatments}
    \item{\code{cost.time.off}}{a matrix of simulations from the posterior distribution 
of the costs for time off work associated with the two treatments}
    \item{\code{cost.time.vac}}{a matrix of simulations from the posterior distribution 
of the costs for time needed to get the vaccination associated with the two treatments}
    \item{\code{cost.travel}}{a matrix of simulations from the posterior distribution 
of the costs for travel to get vaccination associated with the two treatments}
    \item{\code{cost.trt1}}{a matrix of simulations from the posterior distribution 
of the overall costs for first line of treatment associated with the two interventions}
    \item{\code{cost.trt2}}{a matrix of simulations from the posterior distribution 
of the overall costs for second line of treatment associated with the two interventions}
    \item{\code{cost.vac}}{a matrix of simulations from the posterior distribution 
of the costs for vaccination}
    \item{\code{c.pts}}{a matrix of simulations from the posterior distribution of 
the clinical benefits associated with the two treatments}
    \item{\code{e}}{a matrix of simulations from the posterior distribution of 
the clinical benefits associated with the two treatments}
    \item{\code{e.pts}}{a matrix of simulations from the posterior distribution of 
the clinical benefits associated with the two treatments}
    \item{\code{N}}{the number of subjects in the reference population}
    \item{\code{N.outcomes}}{the number of clinical outcomes analysed}
    \item{\code{N.resources}}{the number of health-care resources under study}
    \item{\code{QALYs.adv}}{a vector from the posterior distribution of the QALYs 
associated with advert events}
    \item{\code{QALYs.death}}{a vector from the posterior distribution of the QALYs 
associated with death}
    \item{\code{QALYs.hosp}}{a vector from the posterior distribution of the QALYs 
associated with hospitalisation}
    \item{\code{QALYs.inf}}{a vector from the posterior distribution of the QALYs 
associated with influenza infection}
    \item{\code{QALYs.pne}}{a vector from the posterior distribution of the QALYs 
associated with penumonia}
    \item{\code{treats}}{a vector of labels associated with the two treatments}
    \item{\code{vaccine}}{a \code{rjags} object containing the simulations for the parameters
used in the original model}
    \item{\code{vaccine_mat}}{a matrix containing the simulations for the parameters
used in the original model}
  }
}

\source{
Adapted from Turner D, Wailoo A, Cooper N, Sutton A, Abrams K, Nicholson K. 
The cost-effectiveness of influenza vaccination of healthy adults 50-64 years of age. 
Vaccine. 2006;24:1035-1043.
}

\references{
Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity Analysis in Health Economics. 
Statistical Methods in Medical Research doi:10.1177/0962280211419832.
}

\examples{
data(Vaccine)

\donttest{
m=bcea(e,c,ref=1,interventions=treats)
}
}

\keyword{datasets}
