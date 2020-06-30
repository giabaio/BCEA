
#' @noRd
#' 
.ceac_plot_ggplot <- function(he,
                              pos_legend,
                              graph_params,
                              comparison, ...) {
  
  extra_params <- list(...)
  
  is_pkg_avail <-
    requireNamespace("ggplot2", quietly = TRUE) & requireNamespace("grid", quietly = TRUE)
  
  if (!is_pkg_avail) {
    message("falling back to base graphics\n")
    ceac.plot(he, pos = pos_legend, graph = "base", ...)
    return(invisible(NULL))
  }
  
  if (!is.null(comparison) & he$n_comparisons > 1) {
    
    he <- adjust_for_comparison(he, comparison)
    return(ceac.plot(he, pos = pos_legend, graph = "ggplot2", ...))
  }
  
  k <- NA_real_
  
  if (he$n_comparisons == 1) {
    
    data_psa <- tibble(k = he$k,
                       ceac = he$ceac)
    
    default_params <- list(plot =
                             list(labels = NULL,
                                  line =
                                    list(types = 1)))
    graph_params <- modifyList(default_params, graph_params)
    
    ceac <-
      ggplot(data_psa, aes(k, ceac, colour = factor(1))) +
      geom_line()
  }
  
  if (he$n_comparisons > 1 & is.null(comparison)) {
    
    data_psa <-
      tibble(k = rep(he$k,
                     times = he$n_comparisons),
             ceac = c(he$ceac),
             comparison = as.factor(rep(1:he$n_comparisons,
                                        each = length(he$k))))
    
    graph_params <- create_multiple_comparison_params(he, graph_params)
    
    ceac <- 
      ggplot(data_psa, aes(k, ceac)) +
      geom_line(linetype = comparison,
                colour = comparison)
  }
  
  legend_params <- make_legend(pos_legend)
  
  opt_theme <- purrr::keep(extra_params, is.theme)
  
  ceac +
    theme_bw() + 
    opt_theme +
    scale_y_continuous(limits = c(0, 1)) +
    do.call(labs, graph_params$annot) +                    # plot text
    do.call(theme,                                         # theme
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
              legend_params)) +
    scale_linetype_manual("",
                          labels = graph_params$plot$labels,
                          values = graph_params$plot$line$types) +
    scale_color_manual("",
                       labels = graph_params$plot$labels,
                       values = graph_params$plot$line$colors)
}
