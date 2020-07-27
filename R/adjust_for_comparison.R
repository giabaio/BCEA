
#
adjust_for_comparison <- function(he,
                                  comparison) {
  
  he$comp <- he$comp[comparison]
  he$delta.e <- he$delta.e[, comparison]
  he$delta.c <- he$delta.c[, comparison]
  he$n_comparators <- length(comparison) + 1
  he$n_comparisons <- length(comparison)
  he$interventions <- he$interventions[sort(c(he$ref, he$comp))]
  he$ICER <- he$ICER[comparison]
  he$ib <- he$ib[, , comparison]
  he$eib <- he$eib[, comparison]
  he$U <- he$U[, , sort(c(he$ref, comparison + 1))]
  he$ceac <- he$ceac[, comparison]
  he$ref <- rank(c(he$ref, he$comp))[1]
  he$comp <- rank(c(he$ref, he$comp))[-1]
  he$mod <- TRUE
  
  he
}