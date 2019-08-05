\name{diag.evppi}
\alias{diag.evppi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
diag.evppi
}
\description{
Performs diagnostic plots for the results of the EVPPI
}
\usage{
diag.evppi(x,y,diag=c("residuals","qqplot"),int=1)
}
\arguments{
\item{x}{
A \code{evppi} object obtained by running the function \code{evppi} on a \code{bcea} 
model.
}
\item{y}{
A \code{bcea} object containing the results of the Bayesian modelling and the economic 
evaluation.
}
\item{diag}{
The type of diagnostics to be performed. It can be the 'residual plot' or the 'qqplot 
plot'.
}
\item{int}{
Specifies the interventions for which diagnostic tests should be performed (if there are many
options being compared)
}
}
\value{
The function produces either a residual plot comparing the fitted values from the 
INLA-SPDE Gaussian Process regression to the residuals. This is a scatter plot of 
residuals on the y axis and fitted values (estimated responses) on the x axis. The plot 
is used to detect non-linearity, unequal error variances, and outliers. A well-behaved 
residual plot supporting the appropriateness of the simple linear regression model has 
the following characteristics:
1) The residuals bounce randomly around the 0 line. This suggests that the assumption 
that the 
relationship is linear is reasonable.
2) The residuals roughly form a horizontal band around the 0 line. This suggests that 
the variances 
of the error terms are equal.
3) None of the residual stands out from the basic random pattern of residuals. This 
suggests that there are no outliers.

The second possible diagnostic is the qqplot for the fitted value. This is a graphical 
method for comparing the fitted values distributions with the assumed underlying 
normal distribution by plotting their quantiles against each other. First, the set of 
intervals for the quantiles is chosen. A point (x,y) on the plot corresponds to one of 
the quantiles of the second distribution (y-coordinate) plotted against the same quantile 
of the first distribution (x-coordinate). If the two distributions being compared are 
identical, the Q-Q plot follows the 45 degrees line. 
}
\references{
Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity Analysis in Health Economics. 
Statistical Methods in Medical Research doi:10.1177/0962280211419832.

Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall, London
}
\author{
Gianluca Baio, Anna Heath
}
\seealso{
\code{\link{bcea}},
\code{\link{evppi}}
}
\concept{Health economic evaluation}
\concept{Value of Information}
