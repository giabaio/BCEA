
#' Prepare CE-plane Parameters
#'
#' In ggplot format, combine user-supplied
#' parameters with defaults.
#'
#' @template args-he
#' @param wtp Willingness-to-pay
#' @param graph A string used to select the graphical engine to use for
#'   plotting. Must match either `"base"`, `"ggplot2"` or `"plotly"`
#' @param ... Additional arguments
#' @importFrom grDevices grey.colors
#'
#' @return List of graph parameters
#' @export
#' @keywords internal
#'
prep_ceplane_params <- function(he, wtp, graph, ...) {
  
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
    list(xlab = "Incremental effectiveness",
         ylab = "Incremental cost",
         title = plot_title,
         xlim = axes_lim$x,
         ylim = axes_lim$y,
         point = list(
           color = grey.colors(n = he$n_comparisons,
                               end = 0.7,
                               alpha = 1),
           size = ifelse(graph == "plotly", 8, 0.35),
           shape = rep(20, he$n_comparisons)),
         area_include = TRUE,
         ICER_size = ifelse(graph == "plotly", ifelse(he$n_comparisons == 1, 8, 0), 2),
         area = list(
           # line_color = "black",
           col = "grey95"),
         ref_first = TRUE)
  
  modifyList(default_params, graph_params)
}

