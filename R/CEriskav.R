
#' @name CEriskav_assign
#' @title Cost-effectiveness Analysis Including a Parameter of Risk Aversion
#' 
#' @description Extends the standard cost-effectiveness analysis to modify the utility
#' function so that risk aversion of the decision maker is explicitly accounted for. 
#' 
#' @aliases CEriskav CEriskav.default
#' 
#' @template args-he
#' @param value A vector of values for the risk aversion parameter. If `NULL`,
#' default values are assigned by R. The first (smallest) value (`r` -> 0)
#' produces the standard analysis with no risk aversion.
#' 
#' @return An object of the class `CEriskav` containing the following elements:
#' 
#' - **Ur**: An array containing the simulated values for all the 
#'   "known-distribution" utilities for all interventions, all the values of 
#'   the willingness to pay parameter, and for all the possible values of `r`.
#' - **Urstar**: An array containing the simulated values for the 
#'   maximum "known-distribution" expected utility for all the values of the 
#'   willingness to pay parameter and for all the possible values of `r`.
#' - **IBr**: An array containing the simulated values for the distribution of 
#'   the Incremental Benefit for all the values of the willingness to pay 
#'   parameter and for all the possible values of `r`.
#' - **eibr**: An array containing the Expected Incremental Benefit for each 
#'   value of the willingness to pay parameter and for all the possible values of `r`.
#' - **vir**: An array containing all the simulations for the Value of 
#'   Information for each value of the willingness to pay parameter and for all 
#'   the possible values of `r`.
#' - **evir**: An array containing the Expected Value of Information for each 
#'   value of the willingness to pay parameter and for all the possible values of `r`.
#' - **R**: The number of possible values for the parameter of risk aversion `r`.
#' - **r**: A vector containing all the possible values for the parameter of 
#'   risk aversion `r`.
#' 
#' @author Gianluca Baio
#' @seealso [bcea()]
#' @importFrom Rdpack reprompt
#' 
#' @references
#' \insertRef{Baio2011}{BCEA}
#' 
#' \insertRef{Baio2013}{BCEA}
#' 
#' @examples
#' 
#' # See Baio G., Dawid A.P. (2011) for a detailed description of the 
#' # Bayesian model and economic problem
#'
#' # Load the processed results of the MCMC simulation model
#' data(Vaccine)
#'
#' # Runs the health economic evaluation using BCEA
#' m <- bcea(e=eff,c=cost,     # defines the variables of 
#'                             #  effectiveness and cost
#'       ref=2,                # selects the 2nd row of (e, c) 
#'                             #  as containing the reference intervention
#'       interventions=treats, # defines the labels to be associated 
#'                             #  with each intervention
#'       Kmax=50000            # maximum value possible for the willingness 
#'                             #  to pay threshold; implies that k is chosen 
#'                             #  in a grid from the interval (0, Kmax)
#' )
#'
#' # Define the vector of values for the risk aversion parameter, r, eg:
#' r <- c(1e-10, 0.005, 0.020, 0.035) 
#'
#' # Run the cost-effectiveness analysis accounting for risk aversion
#' \donttest{
#' # uses the results of the economic evaluation 
#' # if more than 2 interventions, selects the 
#' #  pairwise comparison
#' 
#' CEriskav(m) <- r
#' }
#' 
#' @export
#' 
'CEriskav<-' <- function(he, value)
  UseMethod("CEriskav<-", he)

