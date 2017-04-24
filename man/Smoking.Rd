\name{Smoking}
\alias{Smoking}
\alias{data}
\alias{life.years}
\alias{pi}
\alias{smoking}
\alias{smoking_output}
\docType{data}
\title{
Data set for the Bayesian model for the cost-effectiveness of smoking cessation 
interventions
}
\description{
This data set contains the results of the Bayesian analysis used to model the clinical 
output and the costs associated with the health economic evaluation of four different 
smoking cessation interventions.
}
\usage{data(Smoking)}
\format{
A data list including the variables needed for the smoking cessation cost-effectiveness 
analysis. The variables are as follows:
  \describe{
    \item{\code{c}}{a matrix of 500 simulations from the posterior distribution of the 
overall costs associated with the four strategies}
    \item{\code{data}}{a dataset containing the characteristics of the smokers in the 
UK population}
    \item{\code{e}}{a matrix of 500 simulations from the posterior distribution of the 
clinical benefits associated with the four strategies}
    \item{\code{life.years}}{a matrix of 500 simulations from the posterior distribution 
of the life years gained with each strategy}
    \item{\code{pi}}{a matrix of 500 simulations from the posterior distribution of 
the event of smoking cessation with each strategy}
    \item{\code{smoking}}{a data frame containing the inputs needed for the network 
meta-analysis model. The \code{data.frame} object contains: \code{nobs}: the record 
ID number, \code{s}: the study ID number, \code{i}: the intervention ID number, 
\code{r_i}: the number of patients who quit smoking, \code{n_i}: the total number of 
patients for the row-specific arm and \code{b_i}: the reference intervention for 
each study}
    \item{\code{smoking_output}}{a \code{rjags} object obtained by running the 
network meta-analysis model based on the data contained in the \code{smoking} object}
    \item{\code{smoking_mat}}{a matrix obtained by running the 
network meta-analysis model based on the data contained in the \code{smoking} object}
    \item{\code{treats}}{a vector of labels associated with the four strategies}
  }
}

\source{
Effectiveness data adapted from Hasselblad V. (1998). Meta-analysis of 
Multitreatment Studies. Medical Decision Making 1998;18:37-43.

Cost and population characteristics data adapted from various sources:
  \itemize{
    \item Taylor, D.H. Jr, et al. (2002). Benefits of smoking cessation on 
longevity. American Journal of Public Health 2002;92(6)
    \item ASH: Action on Smoking and Health (2013). ASH fact sheet on 
smoking statistics, \cr \code{http://ash.org.uk/files/documents/ASH_106.pdf}
    \item Flack, S., et al. (2007). Cost-effectiveness of interventions for 
smoking cessation. York Health Economics Consortium, January 2007
    \item McGhan, W.F.D., and Smith, M. (1996). Pharmacoeconomic analysis 
of smoking-cessation interventions. American Journal of Health-System Pharmacy 
1996;53:45-52 
  }
}
\references{
Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall, London
}
\examples{
data(Smoking)

\donttest{
m=bcea(e,c,ref=4,interventions=treats,Kmax=500)
}
}
\keyword{datasets}
