
#' ceplane_ggplot_params
#' 
#' @import ggplot2
#'
ceplane_ggplot_params <- function(he, graph_params) {
  
  default_params <-
    list(
      size = rel(3.5),         # relative size
      label.pos = TRUE,
      ICER = list(
        sizes = ifelse(he$n_comparisons == 1, 2, 0)),
      means = list(
        dat =
          data.frame(means) %>%
          `names<-`(c("lambda.e", "lambda.c", "comparison")),
        comparison = factor(1:he$n_comparisons)),
      scale_size_manual = rep_len(graph_params$point$sizes,
                                  length(comparisons.label)),
      annot_wtp_params = list(
        geom = "text",
        hjust = 0.15),
      legend = list(
        comparisons.label =
          with(he, paste0(interventions[ref],
                          " vs ",
                          interventions[comp])),
        jus = NULL),
      annot_line_params = list(
        color = "black"),
      annot_polygon_params = list(
        fill = graph_params$area$color,
        alpha = 0.3))
  
  
  setup_params()
  
  polygon_params <- c(geom = "polygon", polygon_params(graph_params, wtp))
  
  plot_params_ktxt()
  
  plot_params$icer_text
  
  annot_wtp_params <- 
    modifyList(annot_wtp_params,
               list(
                 x = k_text$x,
                 y = k_text$y,
                 label = paste0("k = ", format(wtp, digits = 6), " "),
                 size = size))
  
  annot_line_params <-
    modifyList(annot_line_params,
               list(
                 geom = "line",
                 x = x[c(1,2)],
                 y = y[c(1,2)],
                 color = plot_aes$area$line_color))
}

