
#' @noRd
#' 
.ceac_plot_ggplot <- function() {
  
  if (!isTRUE(
    requireNamespace("ggplot2", quietly = TRUE) &
    requireNamespace("grid", quietly = TRUE)
  )) {
    message("falling back to base graphics\n")
    ceac.plot(he, pos = alt.legend, graph = "base", ...)
    return(invisible(NULL))
  }
  
  if (he$n.comparisons > 1 & is.null(comparison) == FALSE) {
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
    return(ceac.plot(he, pos = alt.legend, graph = "ggplot2", ...))
  }
  # no visible binding note
  k = NA_real_
  if (he$n.comparisons == 1) {
    data.psa <- data.frame("k" = he$k, "ceac" = he$ceac)
    if (is.null(plot_aes$line$types)) 
      plot_aes$line$types <- 1
    ceac <- ggplot2::ggplot(data.psa, ggplot2::aes(k,ceac)) + 
      ggplot2::geom_line(
        linetype = plot_aes$line$types[1], 
        colour = plot_aes$line$colors[1])
  }
  if (he$n.comparisons > 1 & is.null(comparison) == TRUE) {
    data.psa <- with(
      he, data.frame(
        "k" = c(k), "ceac" = c(ceac),
        "comparison" = as.factor(c(
          sapply(1:he$n.comparisons, function(x) rep(x, length(he$k)))
        ))))
    # labels for legend
    comparisons.label <- with(he,paste0(interventions[ref]," vs ",interventions[comp]))
    # linetype is the indicator
    if (is.null(plot_aes$line$types))
      plot_aes$line$types = rep_len(1:6, he$n.comparisons)
    # adjust provided aes lengths
    if (length(plot_aes$line$types) < length(comparisons.label))
      plot_aes$line$types <- rep_len(plot_aes$line$types, length(comparisons.label))
    if (length(plot_aes$line$colors) < length(comparisons.label))
      plot_aes$line$colors <- rep_len(plot_aes$line$colors, length(comparisons.label))
    ceac <- ggplot2::ggplot(
      data.psa,
      ggplot2::aes(k, ceac, linetype = comparison, colour = comparison)) +
      ggplot2::geom_line() +
      ggplot2::scale_linetype_manual(
        "", labels = comparisons.label, values = plot_aes$line$types) +
      ggplot2::scale_colour_manual(
        "", labels = comparisons.label, values = plot_aes$line$colors)
  }
  ceac <- ceac + ggplot2::theme_bw() + 
    ggplot2::scale_y_continuous(limits = c(0,1)) +
    ggplot2::labs(
      title = plot_annotations$title,
      x = plot_annotations$xlab, y = plot_annotations$ylab) 
  jus <- NULL
  if (isTRUE(alt.legend)) {
    alt.legend = "bottom"
    ceac <- ceac + ggplot2::theme(legend.direction = "vertical")
  }
  else{
    if (is.character(alt.legend)) {
      choices <- c("left", "right", "bottom", "top")
      alt.legend <- choices[pmatch(alt.legend, choices)]
      jus = "center"
      if (is.na(alt.legend)) alt.legend = FALSE
    }
    if (length(alt.legend) > 1) jus <- alt.legend
    if (length(alt.legend) == 1 & !is.character(alt.legend)) {
      alt.legend <- c(1, 0); jus <- alt.legend
    }
  }
  # opt theme retrieval, if any
  opt.theme <- ggplot2::theme()
  for (obj in exArgs)
    if (ggplot2::is.theme(obj))
      opt.theme <- opt.theme + obj
  
  # theme refinement
  ceac +
    ggplot2::theme(
      legend.position = alt.legend,
      legend.justification = jus,
      legend.title = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      text = ggplot2::element_text(size = 11),
      legend.key.size = grid::unit(.66, "lines"),
      legend.spacing = grid::unit(-1.25, "line"),
      panel.grid = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.text.align = 0,
      plot.title = ggplot2::element_text(
        lineheight = 1.05,
        face = "bold",
        size = 14.3,
        hjust = 0.5)) + opt.theme
}