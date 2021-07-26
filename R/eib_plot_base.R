
#' eib_plot_base
#' 
eib_plot_base <- function(he,
                          alt.legend,
                          plot_aes,
                          plot_annotations,
                          size,
                          comparison,
                          yl,
                          alpha,
                          cri,
                          plot.cri,
                          ...) {
  
  if (!is.null(size)) {
    if (!is.na(size)) {
      message("Option size will be ignored using base graphics.")
      size <- NULL
    }
  }
  
  if (is.numeric(alt.legend) & length(alt.legend) == 2) {
    temp <- ""
    if (alt.legend[2] == 0)
      temp <- paste0(temp, "bottom")
    else
      temp <- paste0(temp, "top")
    
    if (alt.legend[1] == 1)
      temp <- paste0(temp, "right")
    else
      temp <- paste0(temp, "left")
    
    alt.legend <- temp
    
    if (length(grep("^(bottom|top)(left|right)$", temp)) == 0)
      alt.legend <- FALSE
  }
  
  if (is.logical(alt.legend)) {
    if (!alt.legend)
      alt.legend <- "topleft"
    else
      alt.legend <- "topright"
  }
  
  if (he$n_comparisons == 1) {
    plot(
      NULL,
      ylim = yl,
      xlim = range(he$k),
      xlab = switch(
        as.numeric(plot_annotations$exist$xlab) + 1,
        "Willingness to pay",
        plot_annotations$xlab),
      ylab = switch(
        as.numeric(plot_annotations$exist$ylab) + 1,
        "EIB",
        plot_annotations$ylab),
      main = switch(
        as.numeric(plot_annotations$exist$title) + 1, 
        paste0("Expected Incremental Benefit",
               ifelse(
                 isTRUE(plot.cri),
                 paste0("\nand ", format((1 - alpha)*100, digits = 4),
                        "% credible intervals"),
                 "")),
        plot_annotations$title))
    ## x axis
    abline(h = 0, col = "grey")
    ## EIB
    lines(he$k,
          he$eib,
          col = plot_aes$line$colors[1],
          lty = ifelse(is.null(plot_aes$line$types),
                       yes = 1,
                       no = plot_aes$line$types[1]))
    ## CrI
    if (isTRUE(plot.cri)) {
      lines(he$k, cri$low, col = plot_aes$line$cri_colors[1], lty = 2)
      lines(he$k, cri$upp, col = plot_aes$line$cri_colors[1], lty = 2)
    }
    ### BEP
    if (length(he$kstar) > 0 & is.null(size)) {
      abline(v = he$kstar, col = "dark grey", lty = "dotted")
      text(he$kstar, min(yl), paste("k* = ", he$kstar ,sep = ""))
    }
    
    if (he$change_comp) {
      legend(
        alt.legend,
        paste0(he$interventions[he$ref], " vs ", he$interventions[he$comp]),
        cex = 0.7,
        bty = "n",
        lwd = 1,
        lty = ifelse(is.null(plot_aes$line$types),
                     yes = 1,
                     no = plot_aes$line$types[1]))
    }
  } else if (he$n_comparisons > 1 & is.null(comparison)) {
    lwd <- ifelse(he$n_comparisons > 6, 1.5, 1)
    plot(
      NULL,
      ylim = yl,
      xlim = range(he$k),
      xlab = switch(
        as.numeric(plot_annotations$exist$xlab) + 1,
        "Willingness to pay",
        plot_annotations$xlab),
      ylab = switch(
        as.numeric(plot_annotations$exist$ylab) + 1,
        "EIB",
        plot_annotations$ylab),
      main = switch(
        as.numeric(plot_annotations$exist$title) + 1, 
        paste0("Expected Incremental Benefit",
               ifelse(
                 plot.cri,
                 paste0("\nand ", format((1 - alpha)*100, digits = 4),
                        "% credible intervals"), "")),
        plot_annotations$title))
    
    abline(h = 0, col = "grey")
    
    if (is.null(plot_aes$line$types))
      plot_aes$line$types <- 1:he$n_comparisons
    
    for (j in seq_len(he$n_comparisons)) {
      lines(he$k, he$eib[, j],
            lty = plot_aes$line$types[min(j, length(plot_aes$line$types))], 
            lwd = ifelse(isTRUE(plot.cri), lwd + 1, lwd), 
            col = plot_aes$line$colors[min(j, length(plot_aes$line$colors))])
      if (isTRUE(plot.cri)) {
        lines(he$k,
              cri$low[cri$comp == j],
              lwd = lwd, 
              col = plot_aes$line$cri_colors[min(j, length(plot_aes$line$cri_colors))],
              lty = plot_aes$line$types[min(j, length(plot_aes$line$types))])
        lines(he$k,
              cri$upp[cri$comp == j],
              lwd = lwd, 
              col = plot_aes$line$cri_colors[min(j, length(plot_aes$line$cri_colors))],
              lty = plot_aes$line$types[min(j, length(plot_aes$line$types))])
      }
    }
    
    if (length(he$kstar) > 0 & is.null(size)) {
      abline(v = he$kstar, col = "dark grey", lty = "dotted")
      text(he$kstar, min(yl), paste("k* = ", he$kstar, sep = ""))
    }
    
    text <- paste0(he$interventions[he$ref], " vs ", he$interventions[he$comp])
    
    legend(
      alt.legend,
      text,
      cex = 0.7,
      bty = "n",
      lty = plot_aes$line$types,
      lwd = ifelse(isTRUE(plot.cri), lwd + 1, lwd))
  } else if (he$n_comparisons > 1 &
             !is.null(comparison)) {
    # adjusts bcea object for the correct number of dimensions and comparators
    he$comp <- he$comp[comparison]
    he$delta_e <- he$delta_e[, comparison]
    he$delta_c <- he$delta_c[, comparison]
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
    he$change_comp <- TRUE
    
    eib.plot(
      he,
      comparison = NULL,
      pos = alt.legend,
      size = size,
      plot.cri = plot.cri,
      graph = "base",
      alpha = alpha,
      # cri.quantile = cri.quantile,
      ...)
  }
  
}