
#' @rdname ib.plot
#'
#' @param comparison In the case of multiple interventions, specifies the one
#' to be used in comparison with the reference. Default value of `NULL`
#' forces R to consider the first non-reference intervention as the comparator.
#' Controls which comparator is used when more than 2 interventions are present
#' @param wtp The value of the willingness to pay threshold. Default value at
#' `25000`.
#' @param bw Identifies the smoothing bandwidth used to construct the kernel
#' estimation of the IB density.
#' @param n The number of equally spaced points at which the density is to be
#' estimated.
#' @param xlim The limits of the plot on the x-axis.
#' @param graph A string used to select the graphical engine to use for
#' plotting. Should (partial-) match the two options `"base"` or
#' `"ggplot2"`. Default value is `"base"`.
#' 
#' @return \item{ib}{ A ggplot object containing the requested plot. Returned
#' only if `graph="ggplot2"`. } The function produces a plot of the
#' distribution of the Incremental Benefit for a given value of the willingness
#' to pay parameter. The dashed area indicates the positive part of the
#' distribution (i.e. when the reference is more cost-effective than the
#' comparator).
#' @author Gianluca Baio, Andrea Berardi
#' @seealso [bcea()],
#'          [ceplane.plot()]
#' @references
#' 
#' \insertRef{Baio2011}{BCEA}
#' 
#' \insertRef{Baio2013}{BCEA}
#' 
#' @keywords hplot
#' @export
#' 
#' @import ggplot2
#' @importFrom Rdpack reprompt
#' 
#' @examples
#' data("Vaccine")
#' he <- BCEA::bcea(eff, cost)
#' ib.plot(he)
#' 
ib.plot.bcea <- function(he,
                         comparison = NULL,
                         wtp = 25000,
                         bw = "bcv",  ##TODO: what was nbw? previous was bigger/smoother
                         n = 512,
                         xlim = NULL,
                         graph = c("base", "ggplot2", "plotly"),
                         ...) {
  graph <- match.arg(graph)
  
  if (!is.null(comparison))
    stopifnot(comparison <= he$n_comparisons)
  
  if (is_baseplot(graph)) {
    ib_plot_base(he,
                 comparison,
                 wtp,
                 bw,
                 n,
                 xlim)
  } else if (is_ggplot(graph)) {
    ib_plot_ggplot(he,
                   comparison,
                   wtp,
                   bw,
                   n,
                   xlim)
  } else if (is_plotly(graph)) {
    ib_plot_plotly(he,
                   comparison,
                   wtp,
                   bw,
                   n,
                   xlim)
  }
}


#' Incremental Benefit (IB) Distribution Plot
#' 
#' Plots the distribution of the Incremental Benefit (IB) for a given value of
#' the willingness to pay threshold.
#' 
#' @template args-he
#' @param ... Additional arguments
#' @export
#' 
ib.plot <- function(he, ...) {
  UseMethod('ib.plot', he)
}


