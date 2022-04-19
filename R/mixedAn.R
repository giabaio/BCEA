
#' @title Cost-Effectiveness Analysis When Multiple (Possibly Non-Cost-Effective)
#' Interventions are Present on the Market
#' 
#' @description Runs the cost-effectiveness analysis, but accounts for the fact that more
#' than one intervention is present on the market.
#' 
#' @aliases mixedAn mixedAn.default
#' 
#' @template args-he
#' @param value A vector of market shares associated with the interventions.
#' Its size is the same as the number of possible comparators.
#' By default, assumes uniform distribution for each intervention.
#' 
#' @return Creates an object in the class \code{mixedAn}, a subclass of \code{bcea}
#'   which contains the results of the health economic evaluation in the mixed analysis case:
#'   \item{Ubar}{An array with the simulations of the ''known-distribution''
#'   mixed utilities, for each value of the discrete grid approximation of the
#'   willingness to pay parameter}
#'   \item{OL.star}{An array with the simulations of the distribution of the
#'   Opportunity Loss for the mixed strategy, for each value of the discrete grid
#'   approximation of the willingness to pay parameter}
#'   \item{evi.star}{The Expected Value of Information for the mixed strategy,
#'   for each value of the discrete grid approximation of the willingness to pay
#'   parameter}
#'   \item{mkt.shares}{The vector of market shares associated with each available
#'   intervention}
#' 
#' @author Gianluca Baio
#' @seealso \code{\link{bcea}}
#' @references
#' Baio, G. and Russo, P. (2009). A decision-theoretic framework for
#' the application of cost-effectiveness analysis in regulatory processes.
#' Pharmacoeconomics 27(8), 645-655 doi:10.2165/11310250
#' 
#' Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity Analysis in Health
#' Economics.  Statistical Methods in Medical Research
#' doi:10.1177/0962280211419832.
#' 
#' Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall, London.
#' @examples
#' 
#' # See Baio G., Dawid A.P. (2011) for a detailed description of the 
#' # Bayesian model and economic problem
#'
#' # Load the processed results of the MCMC simulation model
#' data(Vaccine)
#' 
#' # Runs the health economic evaluation using BCEA
#' m <- bcea(e=e,c=c,          # defines the variables of 
#'                             #  effectiveness and cost
#'       ref=2,                # selects the 2nd row of (e, c) 
#'                             #  as containing the reference intervention
#'       interventions=treats, # defines the labels to be associated 
#'                             #  with each intervention
#'       Kmax=50000,           # maximum value possible for the willingness 
#'                             #  to pay threshold; implies that k is chosen 
#'                             #  in a grid from the interval (0, Kmax)
#'       plot=FALSE)           # inhibits graphical output
#'
#' mixedAn(m) <- NULL      # uses the results of the mixed strategy 
#'                         #  analysis (a "mixedAn" object)
#'                         # the vector of market shares can be defined 
#'                         #  externally. If NULL, then each of the T 
#'                         #  interventions will have 1/T market share
#'                         # produces the plots
#' evi.plot(m)
#' 
#' @export
#' 
'mixedAn<-' <- function(he, value)
  UseMethod('mixedAn<-', he)

