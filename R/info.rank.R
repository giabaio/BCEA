
#' @rdname info.rank
#' @title Information-Rank Plot for bcea Class
#' 
#' @importFrom dplyr slice desc
#' @importFrom graphics barplot
#' @import ggplot2
#' 
#' @export
#' 
info.rank.bcea <- function(he,
                           inp,
                           wtp = NULL,
                           howManyPars = NA,
                           graph = c("base", "ggplot2", "plotly"),
                           rel = TRUE,
                           ...) {

  graph <- match.arg(graph)
  
  extra_args <- list(...)
  
  graph_params <-
    inforank_params(he,
                    inp,
                    wtp,
                    rel,
                    howManyPars,
                    extra_args)
  
  if (is_baseplot(graph)) {
    
    info_rank_base(he, graph_params)
    
  } else if (is_ggplot(graph)) {
    
    info_rank_ggplot(he, graph_params)
    
  } else {
    
    info_rank_plotly(graph_params)
  }
}


#' @title Information-Rank Plot
#' 
#' @description Produces a plot similar to a tornado plot, but based on the analysis of the
#' EVPPI. For each parameter and value of the willingness-to-pay threshold, a
#' barchart is plotted to describe the ratio of EVPPI (specific to that
#' parameter) to EVPI. This represents the relative `importance' of each
#' parameter in terms of the expected value of information.
#' 
#' @template args-he
#' @param inp Named list from running `createInputs()` containing:
#'   \itemize{
#'   \item `parameter` = A vector of parameters for which the individual EVPPI
#' should be calculated. This can be given as a string (or vector of strings)
#' of names or a numeric vector, corresponding to the column numbers of
#' important parameters.
#'   \item `mat` = A matrix containing the simulations for all the parameters
#' monitored by the call to JAGS or BUGS. The matrix should have column names
#' matching the names of the parameters and the values in the vector parameter
#' should match at least one of those values.
#' }
#' @param wtp A value of the wtp for which the analysis should be performed. If
#' not specified then the break-even point for the current model will be used.
#' @param howManyPars Optional maximum number of parameters to be included in the bar plot. 
#' Includes all parameters by default. 
#' @param graph A string used to select the graphical engine to use for plotting.
#' Should (partial-)match one of the two options "base" or "plotly". Default value is "base"
#' @param rel Logical argument that specifies whether the ratio of
#' EVPPI to EVPI (`rel = TRUE`, default) or the absolute value of the EVPPI
#' should be used for the analysis.
#' @param ... Additional options. These include graphical parameters that the
#'   user can specify:
#'   \itemize{
#'   \item `xlim` = limits of the x-axis; ca = font size for the axis
#'   label (default = 0.7 of full size).
#'   \item `cn` = font size for the parameter names
#'   vector (default = 0.7 of full size) - base graphics only.
#'   \item `mai` = margins of the graph (default = c(1.36, 1.5, 1,1)) - base graphics only.
#'   }
#' @return With base graphics: A data.frame containing the ranking of the parameters
#'   with the value of the selected summary, for the chosen wtp; with plotly: a plotly object, 
#'   incorporating in the $rank element the data.frame as above.
#'   The function produces a 'Info-rank' plot. This is an extension of standard 'Tornado
#'   plots' and presents a ranking of the model parameters in terms of their
#'   impact on the expected value of information. For each parameter, the
#'   specific individual EVPPI is computed and used to measure the impact of
#'   uncertainty in that parameter over the decision-making process, in terms of
#'   how large the expected value of gaining more information is.
#' 
#' @author Anna Heath, Gianluca Baio, Andrea Berardi
#' @seealso [bcea()],
#'          [evppi()]
#' @importFrom Rdpack reprompt
#' 
#' @references
#' \insertRef{Baio2011}{BCEA}
#' 
#' \insertRef{Baio2013}{BCEA}
#' 
#' @keywords dplot models
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' # Load the post-processed results of the MCMC simulation model
#' # original JAGS output is can be downloaded from here
#' # https://gianluca.statistica.it/books/bcea/code/vaccine.RData
#' 
#' data("Vaccine")
#' m <- bcea(eff, cost)
#' inp <- createInputs(vaccine_mat)
#' info.rank(m, inp)
#' 
#' info.rank(m, inp, graph = "base")
#' info.rank(m, inp, graph = "plotly")
#' info.rank(m, inp, graph = "ggplot2")
#' }
#' 
info.rank <- function(he, ...) {
  UseMethod('info.rank', he)
}


# prevent BCEA::evppi from throwing messages
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

