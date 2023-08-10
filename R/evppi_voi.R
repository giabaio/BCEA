
#' @export
#' 
evppi_voi <- function(he,
                      param_idx = NULL,
                      input,
                      N = NULL,
                      plot = FALSE,
                      residuals = TRUE,
                      ...)
  UseMethod("evppi_voi", he)


#' @examples
#' 
#' data(Vaccine, package = "BCEA")
#' treats <- c("Status quo", "Vaccination")
#' bcea_vacc <- bcea(e.pts, c.pts, ref = 2, interventions = treats)
#' inp <- createInputs(vaccine_mat)
#' evppi_voi(bcea_vacc, c("beta.1.", "beta.2."), inp$mat)
#' @export
#' 
evppi_voi.bcea <- function(he, param_idx = NULL, input, ...) {
  outputs <- he[c("e","c","k")]
  
  voi::evppi(outputs, inputs = input, pars = param_idx, ...)
}

