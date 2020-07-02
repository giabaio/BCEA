
#' @noRd
#' 
#' @importFrom ggplot2
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
    return()
  }
  
  if (is.null(comparison)) comparison <- he$comp
  
  data_psa <-
    tibble(k = rep(he$k,
                   times = he$n_comparisons),
           ceac = c(he$ceac),
           comparison = as.factor(rep(he$comp,
                                      each = length(he$k))))
  
  graph_params <- helper_ggplot_params(he, graph_params)
  legend_params <- make_legend(pos_legend)
  theme_add <- purrr::keep(extra_params, is.theme)
  
  ggplot(data_psa, aes(k, ceac)) +
    geom_line(aes(linetype = comparison,
                  colour = factor(comparison))) +
    theme_ceac() + 
    theme_add +                                            # theme
    scale_y_continuous(limits = c(0, 1)) +
    do.call(labs, graph_params$annot) +                    # text
    do.call(theme, legend_params) +                        # legend
    scale_linetype_manual("",                              # lines
                          labels = graph_params$plot$labels,
                          values = graph_params$plot$line$types) +
    scale_color_manual("",
                       labels = graph_params$plot$labels,  # colours
                       values = graph_params$plot$line$colors)
}
