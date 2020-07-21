
#' @keywords hplot
#' 
ceac_plot_ggplot <- function(he,
                             pos_legend,
                             graph_params, ...) UseMethod("ceac_plot_ggplot", he)

#
ceac_plot_ggplot.pairwise <- function(he,
                                      pos_legend,
                                      graph_params, ...) {
  ceac_ggplot(he,
              pos_legend,
              graph_params,
              "p_best_interv", ...)
}

#' @keywords hplot
#' 
ceac_plot_ggplot.default <- function(he,
                                     pos_legend,
                                     graph_params, ...) {
  ceac_ggplot(he,
              pos_legend,
              graph_params,
              "ceac", ...)
}

#' @noRd
#' 
#' @keywords hplot
#' 
ceac_ggplot <- function(he,
                        pos_legend,
                        graph_params,
                        ceac, ...) {
  
  extra_params <- list(...)

  ceac_dat <- he[[ceac]]
  n_lines <- ncol(ceac_dat)
  
  data_psa <-
    tibble(k = rep(he$k,
                   times = n_lines),
           ceac = c(ceac_dat),
           comparison = as.factor(rep(1:n_lines,
                                      each = length(he$k))))
  
  graph_params <- helper_ggplot_params(he, graph_params)
  legend_params <- make_legend_ggplot(he, pos_legend)
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
