
#' @rdname ceac_plot_graph
#' 
#' @keywords hplot
#' 
ceac_plot_ggplot <- function(he,
                             pos_legend,
                             graph_params, ...)
  UseMethod("ceac_plot_ggplot", he)


#' @rdname ceac_plot_graph
#' @keywords hplot
#' 
#' @export
#' 
ceac_plot_ggplot.pairwise <- function(he,
                                      pos_legend,
                                      graph_params, ...) {
  ceac_ggplot(he,
              pos_legend,
              graph_params,
              "p_best_interv", ...)
}

#' @rdname ceac_plot_graph
#' @keywords hplot
#' 
#' @export
#' 
ceac_plot_ggplot.bcea <- function(he,
                                  pos_legend,
                                  graph_params, ...) {
  ceac_ggplot(he,
              pos_legend,
              graph_params,
              "ceac", ...)
}

#' @rdname ceac_plot_graph
#' @param ceac ceac index in he
#' @keywords hplot
#' 
#' @export
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
  
  ggplot(data_psa, aes(.data$k, .data$ceac)) +
    geom_line(aes(linetype = .data$comparison,
                  colour = factor(.data$comparison))) +
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
