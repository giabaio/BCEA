
ceac.plot <- function(he, ...) UseMethod("ceac.plot")


#' Plots the probability that each intervention is the most cost-effective
#' 
#' A cost-effectiveness acceptability curve plot of the probability each
#' of the n_int interventions being analysed is the most cost-effective
#' against all pairwise comparisons.
#'  
#' @param he The output of the call to the function \code{\link{multi.ce}}.
#' @param pos Parameter to set the position of the legend. Can be given in form
#' of a string \code{(bottom|top)(right|left)} for base graphics and
#' \code{bottom|top|left|right} for ggplot2. It can be a two-elements vector,
#' which specifies the relative position on the x and y axis respectively, or
#' alternatively it can be in form of a logical variable, with \code{TRUE}
#' indicating to use the first standard and \code{FALSE} to use the second one.
#' Default value is \code{c(1,0.5)}, that is on the right inside the plot area.
#' @param graph A string used to select the graphical engine to use for
#' plotting. Should (partial-)match the two options \code{"base"} or
#' \code{"ggplot2"}. Default value is \code{"base"}.
#' @param ...  Optional arguments. For example, it is possible to specify the
#' colours to be used in the plot. This is done in a vector
#' \code{color=c(...)}. The length of the vector colors needs to be the same as
#' the number of comparators included in the analysis, otherwise \code{BCEA}
#' will fall back to the default values (all black, or shades of grey)
#' @return \item{heplot}{ A ggplot object containing the plot. Returned only
#' if \code{graph="ggplot2"}. }
#' @author Gianluca Baio, Andrea Berardi
#' @seealso \code{\link{bcea}}
#' @references Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity
#' Analysis in Health Economics.  Statistical Methods in Medical Research
#' doi:10.1177/0962280211419832.
#' 
#' Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall,
#' London
#' @keywords Health economic evaluation Multiple comparison
#' @import purrr
#' 
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
#'       ref=2,                # selects the 2nd row of (e, c) 
#'                             #  as containing the reference intervention
#'       interventions=treats, # defines the labels to be associated 
#'                             #  with each intervention
#'       Kmax=50000,           # maximum value possible for the willingness 
#'                             #  to pay threshold; implies that k is chosen 
#'                             #  in a grid from the interval (0,Kmax)
#'       plot=FALSE)           # inhibits graphical output
#' 
#' he <- multi.ce(m)           # uses the results of the economic analysis 
#' 
#' he.plot(he,                   # plots the probability of being most cost-effective
#'         graph="base")         #  using base graphics
#' 
#' if (require(ggplot2)) {
#'     he.plot(he,                   # the same plot
#'             graph="ggplot2")      #  using ggplot2 instead
#' }
#' 
#' @export
#' 
ceac.plot.multi <- function(he,
                            pos = c(1, 0.5),
                            graph = c("base", "ggplot2"),...) {
  
  graph <- match.arg(graph)
  alt.legend <- pos
  is_base_graphics <- pmatch(graph,c("base", "ggplot2")) != 2
  is_req_pkgs <- map_lgl(c("ggplot2","grid"), requireNamespace, quietly = TRUE)
  
  if (!all(is_req_pkgs)) {
    
    message("Falling back to base graphics\n")
    is_base_graphics <- TRUE
  }
  
  graph_params <- prepare_graph_params(...)
  
  if (is_base_graphics) {
    ceac_multi_base(he,
                    graph_params, ...)
  } else {
    ceac_multi_ggplot(he,
                      graph_params, ...)
  }
}
