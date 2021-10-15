
##TODO: I can't find where this is used?...
#
prepare_ceac_params_multi <- function(he,
                                      pos,
                                      ...) {
  
  alt.legend <- pos
  lty <- rep(1:6, ceiling(he$n_comparators/6))[1:he$n_comparators]
  label <- paste0(he$interventions)
  
  jus <- NULL
  
  if (alt.legend) {
    alt.legend <- "bottom"
    heplot <- heplot + theme(legend.direction = "vertical")
  } else {
    if (is.character(alt.legend)) {
      choices <- c("left", "right", "bottom", "top")
      alt.legend <- choices[pmatch(alt.legend, choices)]
      jus <- "center"
      
      if (is.na(alt.legend))
        alt.legend <- FALSE
    }
    
    if (length(alt.legend) > 1)
      jus <- alt.legend
    
    if (length(alt.legend) == 1 && !is.character(alt.legend)) {
      alt.legend <- c(1, 0.5)
      jus <- alt.legend
    }
  }
  
  list(jus = jus,
       alt.legend = alt.legend,
       label = label,
       lty = lty)
}