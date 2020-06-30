
#
prepare_graph_params <- function(...) {
  
  extra_params <- list(...)
  
  # defaults
  
  plot_params <- list(area = list(include = TRUE,
                                  color = NULL),
                      line = list(colors = "black"))
  
  annot_params <- list(title = "Cost Effectiveness Acceptability Curve",
                       xlab = "Willingness to pay",
                       ylab = "Probability of cost effectiveness")
  
  plot_extra_params <- list(extra_params$area,
                            extra_params$line)
  
  annot_extra_params <- list(extra_params$title,
                             extra_params$xlab,
                             extra_params$ylab)
  
  annot_params <- modifyList(annot_params, annot_extra_params)
  plot_params <- modifyList(plot_params, plot_extra_params)
  
  list(annot = annot_params,
       plot = plot_params)
}
