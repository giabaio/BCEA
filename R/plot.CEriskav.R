
#' Plots EIB and EVPI for the Risk Aversion Case
#' 
#' Summary plot of the health economic analysis when risk aversion is included.
#' 
#' Plots the Expected Incremental Benefit and the Expected Value of Perfect Information
#' when risk aversion is included in the utility function.
#' 
#' @param he An object of the class \code{CEriskav}, a subclass of \code{bcea},
#' containing the results of the economic analysis performed accounting for a
#' risk aversion parameter (obtained as output of the function \code{\link{CEriskav}}).
#' @template args-pos
#' @param graph A string used to select the graphical engine to use for
#' plotting. Should (partial-)match the two options \code{"base"} or
#' \code{"ggplot2"}. Default value is \code{"base"}.
#' @param ...  Arguments to be passed to methods, such as graphical parameters
#' (see \code{\link{par}}).
#' 
#' @return \item{list(eib,evi)}{A two-elements named list of the ggplot objects
#' containing the requested plots. Returned only if \code{graph="ggplot2"}.}
#' The function produces two plots for the risk aversion analysis. The first
#' one is the EIB as a function of the discrete grid approximation of the
#' willingness parameter for each of the possible values of the risk aversion
#' parameter, \code{r}. The second one is a similar plot for the EVPI.
#' 
#' @author Gianluca Baio, Andrea Berardi
#' @seealso \code{\link{bcea}}, \code{\link{CEriskav}}
#' @references
#' Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity
#' Analysis in Health Economics.  Statistical Methods in Medical Research
#' doi:10.1177/0962280211419832.
#' 
#' Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall, London
#' @keywords Health economic evaluation Risk aversion
#' 
#' @importFrom grDevices dev.new devAskNewPage
#' @importFrom grid unit
#' @import ggplot2
#' 
#' @examples
#' 
#' # See Baio G., Dawid A.P. (2011) for a detailed description of the 
#' # Bayesian model and economic problem
#' #
#' # Load the processed results of the MCMC simulation model
#' data(Vaccine)
#' # 
#' # Runs the health economic evaluation using BCEA
#' m <- bcea(e=e,c=c,          # defines the variables of 
#'                             #  effectiveness and cost
#'       ref=2,                # selects the 2nd row of (e,c) 
#'                             #  as containing the reference intervention
#'       interventions=treats, # defines the labels to be associated 
#'                             #  with each intervention
#'       Kmax=50000,           # maximum value possible for the willingness 
#'                             #  to pay threshold; implies that k is chosen 
#'                             #  in a grid from the interval (0,Kmax)
#'       plot=FALSE            # inhibits graphical output
#' )
#' #
#' # Define the vector of values for the risk aversion parameter, r, eg:
#' r <- c(1e-10, 0.005, 0.020, 0.035) 
#' #
#' # Run the cost-effectiveness analysis accounting for risk aversion
#' \donttest{
#'    CEriskav(m) <- r
#' }
#' #
#' # produce the plots
#' \donttest{
#'    plot(m)
#' )
#' }
#' ## Alternative options, using ggplot2
#' \donttest{
#'    plot(m,
#'     graph="ggplot2"
#'   )
#' }
#' 
#' @export
#' 
plot.CEriskav <- function(he,
                          pos = c(0, 1),
                          graph = c("base", "ggplot2"),
                          ...) {
  
  graph <- match.arg(graph)
  
  ##TODO:
  # graph_params <- prep_CEriskav_params(...)
  
  if (is_baseplot(graph)) {
    
    CEriskav_plot_base(he, pos)
    
  } else {
    
    CEriskav_plot_ggplot(he, pos)
  }
}

