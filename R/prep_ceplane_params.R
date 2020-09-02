
#' prep_ceplane_params
#'
#' In ggplot format, combine user supplied with defaults.
#'
#' @param he 
#' @param wtp
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
#' 
prep_ceplane_params <- function(he, wtp, ...) {
  
  graph_params <- list(...)
  
  ##TODO: back-compatibility helper..
  
  intervs_in_title <-
    paste("\n",
          he$interventions[he$ref],
          "vs",
          paste0(he$interventions[he$comp],
                 collapse = ", "))
  
  plot_title <-
    paste0(
      "Cost-Effectiveness Plane",
      ifelse(he$n_comparisons == 1,  #he$change_comp,
             yes = intervs_in_title,
             no = ""))
  
  axes_lim <- xy_params(he, wtp, graph_params)
  
  default_params <-
    list(xlab = "Effectiveness differential",
         ylab = "Cost differential",
         title = plot_title,
         xlim = axes_lim$x,
         ylim = axes_lim$y,
         point = list(
           colors = grey.colors(n = he$n_comparisons,
                                end = 0.7,
                                alpha = 1),
           sizes = 0.35),
         area = list(
           include = TRUE,
           color = "grey95",
           line_color = "black"))
  
  modifyList(default_params, graph_params)
}
