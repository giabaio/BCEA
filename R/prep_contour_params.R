
#' Prepare contour plot parameters
#' Additional to ceplane parameters
#' @template args-he
#' @param ... Additional parameters
#' @return A list of parameters
#' @keywords internal
#' 
prep_contour_params <- function(he, ...) {
  
  contour_params <- list(...)
  ceplane_params <- prep_ceplane_params(he, wtp_params = 1e7, ...)
  
  default_params <-
    modifyList(ceplane_params,
               list(scale = 0.5,
                    nlevels = NULL,
                    levels = c(0.25, 0.5, 0.75, 0.95)))
  modifyList(default_params, contour_params)
}
