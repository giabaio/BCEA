
#' @noRd
#' 
#' @keywords hplot
#' @importFrom ggplot2, purrr
#' 
#' @examples 
#' 
#' data("Vaccine")
#' he <- BCEA::bcea(e, c)
#' 
#' ceac.plot(he, graph = "ggplot2")
#' 
#' ceac.plot(he,
#'           graph = "ggplot2",
#'           title = "my title",
#'           line = list(colors = "green"),
#'           theme = theme_dark())
#
#' he2 <- BCEA::bcea(cbind(e,e - 0.0002), cbind(c,c + 5))
#' mypalette <- RColorBrewer::brewer.pal(3, "Accent")
#' ceac.plot(he2,
#'           graph = "ggplot2", 
#'           title = "my title",
#'           theme = theme_dark(),
#'           pos = TRUE,
#'           line = mypalette)
#' 
ceac_plot_ggplot <- function(he,
                             pos_legend,
                             graph_params, ...) {
  
  extra_params <- list(...)
  
  is_req_pkgs <- map_lgl(c("ggplot2","grid"), requireNamespace, quietly = TRUE)
  
  if (!all(is_req_pkgs)) {
    message("falling back to base graphics\n")
    ceac.plot(he, pos = pos_legend, graph = "base", ...)
    return()
  }
  
  data_psa <-
    tibble(k = rep(he$k,
                   times = he$n_comparisons),
           ceac = c(he$ceac),
           comparison = as.factor(rep(he$comp,
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
