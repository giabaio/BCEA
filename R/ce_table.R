
#' Cost-effectiveness summary statistics table
#' 
#' As is commonly shown in a journal paper
#' @template args-he
#' @param wtp Willingness to pay
#' @param ... Additional parameters
#' @keywords internal
#'
ce_table <- function(he,
                     wtp = 25000,
                     ...) {
  
  data.frame(
    cost = colMeans(he$c)[c(he$ref, he$comp)],
    eff = colMeans(he$e)[c(he$ref, he$comp)],
    delta.c = c(NA, colMeans(he$delta_c)),
    delta.e = c(NA, colMeans(he$delta_e)),
    ICER = c(NA, he$ICER),
    INB = c(NA, he$eib[he$k == wtp, ]))
}


#' Calculate Dataset For ICERs From bcea Object
#'
#' @template args-he
#' @param comp_label Optional vector of strings with comparison labels
#' @param ... Additional arguments
#' 
#' @return A data.frame object including mean outcomes, comparison identifier,
#'   comparison label and associated ICER
#' 
#' @export
#' 
tabulate_means <- function(he,
                           comp_label = NULL,
                           ...) {
  
  if (is.null(comp_label))
    comp_label <- 1:he$n_comparisons
  
  data.frame(
    lambda.e = sapply(1:he$n_comparisons,
                      function(x) mean(as.matrix(he$delta_e)[, x])),
    lambda.c = sapply(1:he$n_comparisons,
                      function(x) mean(as.matrix(he$delta_c)[, x])),
    comparison = as.factor(1:he$n_comparisons),
    label = comp_label,
    ICER = he$ICER)
}
