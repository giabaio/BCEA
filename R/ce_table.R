
#' Cost-effectiveness summary statistics table
#' 
#' As is commonly shown in a journal paper
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
