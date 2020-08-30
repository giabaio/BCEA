
#' ceplane_ggplot_params
#' 
#' @import ggplot2
#'
ceplane_ggplot_params <- function(he, graph_params) {
  
  default_params <-
    list(
      size = rel(3.5),         # relative size
      opt.theme = theme(),
      label.pos = TRUE,
      ICER = list(
        sizes = ifelse(he$n_comparisons == 1, 2, 0)),
      means = list(
        dat = data.frame(means) %>%
          `names<-`(c("lambda.e", "lambda.c", "comparison")),
        comparison = factor(1:he$n_comparisons)),
      scale_size_manual = rep_len(plot_aes$point$sizes,
                                  length(comparisons.label)),
      annot_wtp_params = list(
        geom = "text",
        x = ifelse(range.c[1] / wtp > range.e[1],
                   range.c[1] / wtp,
                   range.e[1]),
        y = range.c[1],
        label = paste0("k = ", format(wtp, digits = 6), "  "),
        hjust = 0.15,
        size = size),
      legend = list(
        jus = NULL),
      annot_line_params = list(
        color = "black"),
      annot_polygon_params = list(
        fill = "lightgrey",
        alpha = 0.3)
    )
  
  # labels for legend
  comparisons.label <-
    with(he, paste0(interventions[ref], " vs ", interventions[comp]))
  
  setup_params
  
  polygon_params
  
  plot_params_ktxt
  
  plot_params$icer_text
  
  annot_wtp_params <- 
    modifyList(annot_wtp_params,
               list(
                 x = x.pt,
                 y = y.pt,
                 label = paste0("k = ", format(wtp, digits = 6)),
                 size = size))
  
  annot_line_params <-
    modifyList(annot_line_params,
               list(
                 geom = "line",
                 x = plane[1:2, 1],
                 y = plane[1:2, 2],
                 color = plot_aes$area$line_color))
  
  annot_polygon_params <- 
    modifyList(annot_polygon_params,
               list(
                 geom = "polygon",
                 x = plane$x,
                 y = plane$y,
                 fill = plot_aes$area$color))
}

