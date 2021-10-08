
#' @import ggplot2
#' @importFrom grid unit
#' @importFrom purrr keep
#' 
eib_plot_ggplot.bcea <- function(he,
                                 graph_params,
                                 ...) {
  extra_params <- list(...)
  
  ##TODO: can we move this up a level?
  cri_params <- eib_params_cri(he, graph_params)
  
  theme_add <- purrr::keep(extra_params, is.theme)
  legend_params <- make_legend_ggplot(he, graph_params$pos)
  graph_params <- eib_params_ggplot(he, graph_params, cri_params)
  
  data_psa <-
    data.frame(
      k = c(he$k),
      eib = c(he$eib), 
      comparison =
        as.factor(rep(1:he$n_comparison,
                      each = length(he$k))))
  
  ggplot(data_psa,
         aes(x = .data$k, y = .data$eib,
             group = .data$comparison)) + 
    geom_line(aes(colour = .data$comparison,
                  linetype = .data$comparison)) +
    theme_eib() +
    theme_add +
    do.call(theme, legend_params) +
    do.call(labs,
            list(title = graph_params$main,
                 x = graph_params$xlab,
                 y = graph_params$ylab)) +
    geom_hline(aes(yintercept = 0),
               colour = "grey",
               linetype = 1) + 
    geom_cri(graph_params$plot.cri, cri_params) +
    do.call(annotate, graph_params$kstar) +
    geom_vline(
      aes(xintercept = .data$kstar),
      data = data.frame("kstar" = he$kstar),
      colour = "grey50",
      linetype = 2,
      size = 0.5) +
    scale_linetype_manual(
      "",
      labels = graph_params$plot$labels,
      values = graph_params$plot$line$types) +
    scale_colour_manual(
      "",
      labels = graph_params$plot$labels,
      values = graph_params$plot$line$colors)
}


#' EIB plot ggplot2 version
#'  
#' @template args-he
#' @param graph_params List of graph parameters
#' @param ... Additional arguments
#' @return ggplot2 object
#' @keywords hplot
#'
eib_plot_ggplot <- function(he,
                            graph_params, ...)
  UseMethod("eib_plot_ggplot", he)


