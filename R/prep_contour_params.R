
#
prep_contour_params <- function(he, ...) {
  
  contour_params <- list(...)
  ceplane_params <- prep_ceplane_params(he, wtp = 1e7, ...)
  
  default_params <-
    modifyList(ceplane_params,
               list(scale = 0.5,
                    nlevels = 4,
                    levels = NULL))
  modifyList(default_params, contour_params)
}
