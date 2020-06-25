
#' @noRd
#' 
.ceac_plot_base <- function() {
  
  if (is.numeric(alt.legend) & length(alt.legend) == 2) {
    temp <- ""
    if (alt.legend[2] == 1)
      temp <- paste0(temp, "top")
    else
      temp <- paste0(temp, "bottom")
    if (alt.legend[1] == 0)
      temp <- paste0(temp, "left")
    else
      temp <- paste0(temp, "right")
    alt.legend <- temp
    if (length(grep("^(bottom|top)(left|right)$", temp)) == 0)
      alt.legend <- FALSE
  }
  if (is.logical(alt.legend)) {
    if (!alt.legend)
      alt.legend = "bottomright"
    else
      alt.legend = "bottomleft"
  }
  
  if (he$n.comparisons == 1) {
    plot(
      he$k, he$ceac, t = "l",
      xlab = plot_annotations$xlab, ylab = plot_annotations$ylab,
      ylim = c(0, 1), main = plot_annotations$title,
      lty = ifelse(is.null(plot_aes$line$types), 1, plot_aes$line$types[1]),
      col = plot_aes$line$colors[1])
  }
  if (he$n.comparisons > 1 & is.null(comparison)) {
    lwd = ifelse(he$n.comparisons <= 6, 1, 1.5)
    # linetype is the indicator
    if (is.null(plot_aes$line$types))
      plot_aes$line$types = rep_len(1:6, he$n.comparisons)
    # adjust provided aes lengths
    if (length(plot_aes$line$types) < he$n.comparisons)
      plot_aes$line$types <- rep_len(plot_aes$line$types, he$n.comparisons)
    if (!exists("line_colors", where = exArgs)) {
      plot_aes$line$colors <- 
        if (he$n.comparisons <= 6) rep(1,he$n.comparisons) else
          colors()[floor(seq(262, 340, length.out = he$n.comparisons))]	# gray scale
    } else {
      if (length(plot_aes$line$colors) < he$n.comparisons)
        plot_aes$line$colors <- rep_len(plot_aes$line$colors, he$n.comparisons)
    }
    plot(
      he$k, he$ceac[,1], t = "l", 
      main = plot_annotations$title,
      xlab = plot_annotations$xlab, ylab = plot_annotations$ylab,
      ylim = c(0, 1), lwd = lwd,
      lty = plot_aes$line$types[1], col = plot_aes$line$colors[1])
    for (j in 2:he$n.comparisons)
      points(he$k, he$ceac[,j], t = "l", lwd = lwd,
             col = plot_aes$line$colors[j], lty = plot_aes$line$types[j])
    text <- paste(he$interventions[he$ref]," vs ",he$interventions[he$comp])
    legend(
      alt.legend, text, cex = .7, bty = "n", 
      lty = plot_aes$line$types, col = plot_aes$line$colors)
  }
  if (he$n.comparisons > 1 & !is.null(comparison)) {
    # adjusts bcea object for the correct number of dimensions and comparators
    he$comp <- he$comp[comparison]
    he$delta.e <- he$delta.e[, comparison]
    he$delta.c <- he$delta.c[, comparison]
    he$n.comparators <- length(comparison) + 1
    he$n.comparisons <- length(comparison)
    he$interventions <- he$interventions[sort(c(he$ref, he$comp))]
    he$ICER <- he$ICER[comparison]
    he$ib <- he$ib[, , comparison]
    he$eib <- he$eib[, comparison]
    he$U <- he$U[, , sort(c(he$ref, comparison + 1))]
    he$ceac <- he$ceac[, comparison]
    he$ref <- rank(c(he$ref, he$comp))[1]
    he$comp <- rank(c(he$ref, he$comp))[-1]
    he$mod <- TRUE #
    
    ceac.plot(he, pos = alt.legend, graph = "base", ...)
  }
}
