
#' Plots EIB and EVPI for the Risk Aversion Case
#' 
#' Summary plot of the health economic analysis when risk aversion is included.
#' 
#' Plots the Expected Incremental Benefit and the Expected Value of Perfect Information
#' when risk aversion is included in the utility function.
#' 
#' @param x An object of the class `CEriskav`, a subclass of `bcea`,
#' containing the results of the economic analysis performed accounting for a
#' risk aversion parameter (obtained as output of the function [CEriskav()]).
#' @template args-pos
#' @param graph A string used to select the graphical engine to use for
#' plotting. Should (partial-)match the options `"base"`,
#' `"ggplot2"` or `"plotly"`. Default value is `"base"`.
#' @param ... Arguments to be passed to methods, such as graphical parameters
#' (see [par()]).
#' 
#' @return \item{list(eib,evi)}{A two-elements named list of the ggplot objects
#' containing the requested plots. Returned only if `graph="ggplot2"`.}
#' The function produces two plots for the risk aversion analysis. The first
#' one is the EIB as a function of the discrete grid approximation of the
#' willingness parameter for each of the possible values of the risk aversion
#' parameter, `r`. The second one is a similar plot for the EVPI.
#' 
#' @author Gianluca Baio, Andrea Berardi
#' @seealso [bcea()], [CEriskav()]
#' @importFrom Rdpack reprompt
#' 
#' @references
#' 
#' \insertRef{Baio2011}{BCEA}
#' 
#' \insertRef{Baio2013}{BCEA}
#' 
#' @keywords hplot
#' 
#' @importFrom grDevices dev.new devAskNewPage
#' @importFrom grid unit
#' @import ggplot2
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
#' m <- bcea(e=eff, c=cost,    # defines the variables of 
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
#' 
#' # Define the vector of values for the risk aversion parameter, r, e.g.:
#' r <- c(1e-10, 0.005, 0.020, 0.035) 
#' 
#' # Run the cost-effectiveness analysis accounting for risk aversion
#' \donttest{
#'    CEriskav(m) <- r
#' }
#' 
#' # produce the plots
#' \donttest{
#'    plot(m)
#' }
#' ## Alternative options, using ggplot2
#' \donttest{
#'    plot(m, graph = "ggplot2")
#' }
#' 
#' @export
#' 
plot.CEriskav <- function(x,
                          pos = "topright",
                          graph = c("base", "ggplot2", "plotly"),
                          ...) {
  
  graph <- match.arg(graph)
  
  ##TODO:
  # graph_params <- prep_CEriskav_params(...)
  
  if (is_baseplot(graph)) {
    CEriskav_plot_base(x,
                       pos_legend = pos)
  } else if (is_ggplot(graph)) {
    CEriskav_plot_ggplot(x,
                         pos_legend = pos)
  } else if (is_plotly(graph)) {
    CEriskav_plot_plotly(x,
                         pos_legend = pos)
  }
}
