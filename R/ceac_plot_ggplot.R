
#' @noRd
#' 
.ceac_plot_ggplot <- function(he,
                              pos_legend,
                              graph_params,
                              comparison, ...) {
  
  is_pkg_avail <-
    requireNamespace("ggplot2", quietly = TRUE) & requireNamespace("grid", quietly = TRUE)
  
  if (!is_pkg_avail) {
    message("falling back to base graphics\n")
    ceac.plot(he, pos = pos_legend, graph = "base", ...)
    return(invisible(NULL))
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
    
    return(ceac.plot(he, pos = pos_legend, graph = "ggplot2", ...))
  }
  
  # no visible binding note
  k <- NA_real_
  
  if (he$n.comparisons == 1) {
    
    data.psa <- mutate(he,
                       k = k,
                       ceac = ceac)
    
    if (is.null(plot_params$line$types))
      plot_params$line$types <- 1
    
    ceac <-
      ggplot2::ggplot(data.psa, aes(k, ceac)) + 
      ggplot2::geom_line(
        linetype = plot_params$line$types[1], 
        colour = plot_params$line$colors[1])
  }
  
  if (he$n.comparisons > 1 & is.null(comparison)) {
    
    data.psa <-
      tibble(k = he$k,
             ceac = he$ceac,
             comparison = as.factor(c(
               sapply(1:he$n.comparisons, function(x) rep(x, length(he$k))))))  #TODO:...
    
    # labels for legend
    comparisons_label <- with(he, paste0(interventions[ref], " vs ", interventions[comp]))
    
    # linetype is the indicator
    if (is.null(plot_params$line$types))
      plot_params$line$types = rep_len(1:6, he$n.comparisons)
    
    # adjust provided aes lengths
    if (length(plot_params$line$types) < length(comparisons_label))
      plot_params$line$types <- rep_len(plot_params$line$types, length(comparisons_label))
    
    if (length(plot_params$line$colors) < length(comparisons_label))
      plot_params$line$colors <- rep_len(plot_params$line$colors, length(comparisons_label))
    
    ceac <-
      ggplot(data.psa, aes(k, ceac, linetype = comparison, colour = comparison)) +
      geom_line() +
      scale_linetype_manual(
        "", labels = comparisons_label, values = plot_params$line$types) +
      scale_colour_manual(
        "", labels = comparisons_label, values = plot_params$line$colors)
  }
  
  ceac <-
    ceac +
    theme_bw() + 
    scale_y_continuous(limits = c(0,1)) +
    do.call(labs, plot_annot)
  
  
  legend_params <- make_legend(pos_legend)
  
  
  # opt theme retrieval, if any
  opt.theme <- ggplot2::theme()
  
  for (obj in extra_params)
    if (ggplot2::is.theme(obj))
      opt.theme <- opt.theme + obj
  
  ceac +
    do.call(theme,
            modifyList(
              list(legend.title = element_blank(),
                   legend.background = element_blank(),
                   text = element_text(size = 11),
                   legend.key.size = grid::unit(.66, "lines"),
                   legend.spacing = grid::unit(-1.25, "line"),
                   panel.grid = element_blank(),
                   legend.key = element_blank(),
                   legend.text.align = 0,
                   plot.title = element_text(
                     lineheight = 1.05,
                     face = "bold",
                     size = 14.3,
                     hjust = 0.5)),
              legend_params)) + opt.theme
}
