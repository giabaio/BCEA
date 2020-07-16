
#
ceac_multi_base <- function(he,
                            colour_params, ...) {
  
  if (is.numeric(alt.legend) & length(alt.legend) == 2) {
    temp <- ""
    
    if (alt.legend[2] == 0)
      temp <- paste0(temp, "bottom")
    else if (alt.legend[2] != 0.5)
      temp <- paste0(temp,"top")
    if (alt.legend[1] == 1)
      temp <- paste0(temp, "right")
    else
      temp <- paste0(temp, "left")
    alt.legend <- temp
    if (length(grep("^((bottom|top)(left|right)|right)$", temp)) == 0)
      alt.legend <- FALSE
  }
  if (is.logical(alt.legend)) {
    if (!alt.legend)
      alt.legend <- "topright"
    else
      alt.legend <- "right"
  }
  
  ##TODO: matplot?
  plot(
    x = he$k,
    y = he$m.ce[, 1],
    type = "l",
    col = color[1],
    lwd = lwd,
    lty = 1,
    xlab = "Willingness to pay",
    ylab = "Probability of most cost effectiveness",
    ylim = c(0, 1),
    main = "Cost-effectiveness acceptability curve \nfor multiple comparisons"
  )
  for (i in 2:he$n_comparators) {
    points(
      x = he$k,
      y = he$m.ce[, i],
      type = "l",
      col = color[i],
      lwd = lwd,
      lty = i)
  }
  legend(
    alt.legend,
    he$interventions,
    col = color,
    cex = 0.7,
    bty = "n",
    lty = 1:he$n_comparators)
}

