
#' prep_ceplane_params
#'
#' In ggplot format, combine user supplied with defaults.
#'
#' @param he 
#' @param comparison 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
#' 
prep_ceplane_params <- function(he,
                                comparison,
                                ...) {
  graph_params <- list(...)
  
  ##TODO: back-compatibility helper..
  
  plot_title <-
    paste0(
      "Cost-Effectiveness Plane",
      ifelse(he$mod,                   ##TODO: how to use this (mod)?
             yes = paste("\n",
                         he$interventions[ref],
                         "vs",
                         paste0(he$interventions[comp],
                                collapse = ", ")),
             no = ""))
  
  default_params <-
    list(xlab = "Effectiveness differential",
         ylab = "Cost differential",
         title = plot_title,
         xlim = NULL,
         ylim = NULL,
         point = list(
           colors = grey.colors(n = length(comparison),
                                end = 0.7,
                                alpha = 1),
           size = 4), #sizes = 0.35?
         area = list(
           include = TRUE,
           color = "grey95",
           line_color = "black"))
  
  modifyList(default_params, graph_params)
}
