
#' Cost-effectiveness summary statistics table
#' 
#' As is commonly shown in a journal paper.
#' @template args-he
#' @param wtp Willingness to pay
#' @param ... Additional parameters
#' @keywords internal
#' @examples 
#' data(Vaccine)
#'
#' # Runs the health economic evaluation using BCEA
#' m <- bcea(
#'       e=eff,
#'       c=cost,               # defines the variables of 
#'                             #  effectiveness and cost
#'       ref=2,                # selects the 2nd row of (e, c) 
#'                             #  as containing the reference intervention
#'       interventions=treats, # defines the labels to be associated 
#'                             #  with each intervention
#'       Kmax=50000,           # maximum value possible for the willingness 
#'                             #  to pay threshold; implies that k is chosen 
#'                             #  in a grid from the interval (0, Kmax)
#' )
#' ce_table(m)
#' @export
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
#' @examples
#' data("Smoking")
#' he <- BCEA::bcea(eff, cost)
#' tabulate_means(he)
#' 
tabulate_means <- function(he,
                           comp_label = NULL,
                           ...) {
  
  comp_label <- comp_label %||% seq_len(he$n_comparisons)
  
  data.frame(
    lambda.e = vapply(1:he$n_comparisons,
                      function(x) mean(as.matrix(he$delta_e)[, x]),
                      FUN.VALUE = NA_real_),
    lambda.c = vapply(1:he$n_comparisons,
                      function(x) mean(as.matrix(he$delta_c)[, x]),
                      FUN.VALUE = NA_real_),
    comparison = as.factor(1:he$n_comparisons),
    label = comp_label,
    ICER = he$ICER)
}
