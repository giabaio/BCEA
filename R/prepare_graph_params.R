
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
  
  plot_extra_params <- extra_params[c("area", "line")]
  annot_extra_params <- extra_params[c("title", "xlab", "ylab")]
  
  annot_params <- modifyList(annot_params, annot_extra_params)
  plot_params <- modifyList(plot_params, plot_extra_params)
  
  list(annot = annot_params,
       plot = plot_params)
}
