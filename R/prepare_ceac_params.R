
#' @keywords dplot
#' 
prepare_ceac_params <- function(he, ...) {
  
  extra_params <- list(...)
  
  # defaults
  
  plot_params <- list(area = list(include = FALSE,
                                  color = NULL),
                      line = list(color = "black",
                                  size = 1,
                                  type = 1:num_lines(he)),
                      text = list(size = 11),
                      currency = "")
  
  annot_params <- list(title = "Cost Effectiveness Acceptability Curve",
                       x = "Willingness to pay",
                       y = "Probability of cost effectiveness")
  
  plot_extra_params <- extra_params[c("area", "line", "text", "currency")]
  annot_extra_params <- extra_params[c("title", "xlab", "ylab")]
  
  annot_params <- modifyList(annot_params, annot_extra_params)
  plot_params <- modifyList(plot_params, plot_extra_params)
  
  c(plot_params,
    list(annot = annot_params))
}
