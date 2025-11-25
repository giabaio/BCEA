
#' Prepare CE-plane Parameters
#'
#' In ggplot format, combine user-supplied
#' parameters with defaults.
#'
#' @template args-he
#' @param wtp_params Willingness-to-pay parameters. This can be a single value or a list.
#' @param ... Additional arguments
#' @importFrom grDevices grey.colors
#'
#' @return List of graph parameters
#' @export
#' @keywords internal
#'

prep_ceplane_params <- function(he, wtp_params, ...) {
  
  graph_params <- list(...)
  
  # back compatibility
  if (!is.list(wtp_params)) {
    wtp_params <- list(value = wtp_params)
  }
  
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

  axes_lim <- xy_params(he, wtp_params$value, graph_params)
  
  default_params <-
    list(xlab = "Incremental effectiveness",
         ylab = "Incremental cost",
         title = plot_title,
         xlim = axes_lim$xlim,
         ylim = axes_lim$ylim,
         label.pos = TRUE,
         point = list(
           color = grey.colors(n = he$n_comparisons,
                               end = 0.7,
                               alpha = 1),
           size = 0.35,
           shape = rep(20, he$n_comparisons)),
         wtp = list(
           value = 25000),
         area_include = TRUE,
         ICER_size = 2,
         text = list(
           size = 11),
         area = list(
           # line_color = "black",
           color = "grey95"),
         ref_first = TRUE)
  
  out <- 
    modifyList(default_params, graph_params) |> 
    modifyList(list(wtp = wtp_params))
  
  out$wtp$label <- paste0("  k = ", format(out$wtp$value, digits = 6), "\n")
  
  # move out of wtp list so can pass straight to geom
  out$wtp_value <- out$wtp$value
  out$wtp$value <- NULL
    
  out
}

