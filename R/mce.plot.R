#' Plots the probability that each intervention is the most cost-effective
#' 
#' This function is deprecated. Use [ceac.plot()] instead.
#' Plots the probability that each of the n_int interventions being analysed is
#' the most cost-effective.
#' 
#' @param mce The output of the call to the function [multi.ce()].
#' @param pos Parameter to set the position of the legend. Can be given in form
#' of a string `(bottom|top)(right|left)` for base graphics and
#' `bottom|top|left|right` for ggplot2. It can be a two-elements vector,
#' which specifies the relative position on the x and y axis respectively, or
#' alternatively it can be in form of a logical variable, with `TRUE`
#' indicating to use the first standard and `FALSE` to use the second one.
#' Default value is `c(1,0.5)`, that is on the right inside the plot area.
#' @param graph A string used to select the graphical engine to use for
#' plotting. Should (partial-)match the two options `"base"` or
#' `"ggplot2"`. Default value is `"base"`. The `"plotly"` option is not 
#' implemented for this particular graph.
#' @param ...  Optional arguments. For example, it is possible to specify the
#' colours to be used in the plot. This is done in a vector
#' `color=c(...)`. The length of the vector colors needs to be the same as
#' the number of comparators included in the analysis, otherwise `BCEA`
#' will fall back to the default values (all black, or shades of grey)
#' @return \item{mceplot}{ A ggplot object containing the plot. Returned only
#' if `graph="ggplot2"`. }
#' @author Gianluca Baio, Andrea Berardi
#' @seealso [BCEA-deprecated()]
#' @importFrom Rdpack reprompt
#' 
#' @references
#' 
#' \insertRef{Baio2011}{BCEA}
#' 
#' \insertRef{Baio2013}{BCEA}
#' 
#' @keywords internal hplot
#' @examples
#' 
#' # See Baio G., Dawid A.P. (2011) for a detailed description of the 
#' # Bayesian model and economic problem
#' 
#' \dontrun{
#' # Load the processed results of the MCMC simulation model
#' data(Vaccine)
#' # 
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
#' #
#' mce <- multi.ce(m)          # uses the results of the economic analysis 
#' #
#' mce.plot(mce,               # plots the probability of being most cost-effective
#'       graph="base")         #  using base graphics
#' #
#' if(require(ggplot2)){
#' mce.plot(mce,               # the same plot
#'       graph="ggplot2")      #  using ggplot2 instead
#' }
#' }
#' 
#' @export
#' 
mce.plot <- function(mce,
                     pos = c(1, 0.5),
                     graph = c("base", "ggplot2"),
                     ...) {
  # lifecycle::deprecate_warn("2.4.1", "mce.plot()", "ceac.plot()")
  .Deprecated(new = "ceac.plot", old = "mce.plot")
  
  alt.legend <- pos
  #base.graphics <- ifelse(isTRUE(pmatch(graph,c("base","ggplot2"))==2),FALSE,TRUE) 
  
  # Matches to the possible options, but if there's an error defaults to "base"
  # graph <- match.arg(graph)
  graph = tryCatch(
    match.arg(graph),
    error = function(e) {
      message("The 'plotly' graph is not implemented yet. Defaulting to 'base'")
      "base"
    }
  )
  
  exArgs <- list(...)
  # Allows to specify colours for the plots
  # If the user doesn't specify anything, use defaults
  if(!exists("color",exArgs)) {
    color <- rep(1,(mce$n_comparators+1)); lwd <- 1
    if (mce$n_comparators>7) {
      cl <- colors()
      color <- cl[floor(seq(262,340,length.out=mce$n_comparators))]	# gray scale
      lwd <- 1.5
    }  
  } 
  # If the user specify colours, then use but check they are the right number
  if(exists("color",exArgs)) {
    color <- exArgs$color
    lwd <- 1
    if(mce$n_comparators>7) {lwd <- 1.5}
    if (length(color)!=(mce$n_comparators))	 {
      message(paste0("You need to specify ",(mce$n_comparators)," colours. Falling back to default\n"))
    }
  } 
  
  ceac.plot(mce,graph=graph,pos=pos,...)
}
