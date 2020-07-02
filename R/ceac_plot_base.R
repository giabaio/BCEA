
#' @noRd
#' 
.ceac_plot_base <- function() {
  
  alt_legend <- FALSE
  if (is.numeric(alt_legend) & length(alt_legend) == 2) {
    temp <- ""
    
    ns <- ifelse(alt_legend[2] == 1, "top", "bottom")
    ew <- ifelse(alt_legend[1] == 1, "left", "right")
    alt_legend <- paste0(ns, ew)
  }
  
  if (is.logical(alt_legend)) {
    if (!alt_legend)
      alt_legend = "bottomright"
    else
      alt_legend = "bottomleft"
  }
  
  if (he$n.comparisons == 1) {
    plot(
      he$k, he$ceac, t = "l",
      xlab = plot_annotations$xlab, ylab = plot_annotations$ylab,
      ylim = c(0, 1), main = plot_annotations$title,
      lty = ifelse(is.null(plot_params$line$types), 1, plot_params$line$types[1]),
      col = plot_params$line$colors[1])
  }
  
  if (he$n.comparisons > 1 & is.null(comparison)) {
    lwd = ifelse(he$n.comparisons <= 6, 1, 1.5)
    
    # linetype is the indicator
    if (is.null(plot_params$line$types))
      plot_params$line$types = rep_len(1:6, he$n.comparisons)
    
    # adjust provided aes lengths
    if (length(plot_params$line$types) < he$n.comparisons)
      plot_params$line$types <- rep_len(plot_params$line$types, he$n.comparisons)
    
    if (!exists("line_colors", where = extra_params)) {
    
      plot_params$line$colors <- 
        if (he$n.comparisons <= 6)
          rep(1, he$n.comparisons)
      else
        colors()[floor(seq(262, 340, length.out = he$n.comparisons))]	# gray scale
    } else {
      if (length(plot_params$line$colors) < he$n.comparisons)
        plot_params$line$colors <- rep_len(plot_params$line$colors, he$n.comparisons)
    }
    
    plot(
      he$k,
      he$ceac[, 1],
      type = "l", 
      main = plot_annotations$title,
      xlab = plot_annotations$xlab, ylab = plot_annotations$ylab,
      ylim = c(0, 1), lwd = lwd,
      lty = plot_params$line$types[1], col = plot_params$line$colors[1])
    
    for (j in 2:he$n.comparisons)
      points(he$k,
             he$ceac[,j],
             type = "l",
             lwd = lwd,
             col = plot_params$line$colors[j],
             lty = plot_params$line$types[j])
    
    text <- paste(he$interventions[he$ref], " vs ", he$interventions[he$comp])
    legend(
      alt_legend,
      text,
      cex = 0.7,
      bty = "n", 
      lty = plot_params$line$types,
      col = plot_params$line$colors)
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
    he$mod <- TRUE
    
    ceac.plot(he, pos = alt_legend, graph = "base", ...)
  }
}
