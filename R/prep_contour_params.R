
#
prep_contour_params <- function(he, graph,...) {
  contour_params <- list(...)
  ceplane_params <- prep_ceplane_params(he, wtp_params = 1e7, graph, ...)
  
  default_params <-
    modifyList(ceplane_params,
               list(scale = 0.5,
                    nlevels = NULL,
                    levels = c(0.25, 0.5, 0.75, 0.95)))
  if (graph == "plotly") default_params$ICER_size = 8
  modifyList(default_params, contour_params)
}
