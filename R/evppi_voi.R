
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
evppi_voi.bcea <- function(he,
                           param_idx = NULL,
                           input,
                           N = NULL, 
                           plot = FALSE,      # plot triangular mesh for SPDE-INLA 
                           residuals = TRUE,  # output fitted values for SPDE-INLA method
                           method = NULL, ...) {
  outputs <- he[c("e","c","k")]
  if (!is.null(method))
    method <- tolower(method)
  
  # allow alternative name for Sadatsafavi method
  if (length(method) > 0 && method == "sad") method <- "sal"
  
  # replace column numbers with names
  pars <- 
    if (all(is.numeric(param_idx))) {
      if (any(!param_idx %in% 1:ncol(input)))
        stop("Column number(s) not in available parameter inputs")
      
      param_idx <- names(input)[param_idx]
    } else {
      param_idx
    }
  
  n_sims <- nrow(input)
  n_outputs <- nrow(outputs$c)
  
  # this way passes on error checking to voi::evppi
  if (n_sims == n_outputs) {
    
    # subset number of PSA samples; default all
    row_idxs <- 
      if (is.null(N) || (N >= n_sims)) {
        1:n_sims
      } else {
        sample(1:n_sims, size = N, replace = FALSE)
      }
    
    input <- data.frame(input[row_idxs, ])
    outputs$e <- outputs$e[row_idxs, ]
    outputs$c <- outputs$c[row_idxs, ]
  }
  
  res <- voi::evppi(outputs, inputs = input, pars = pars, method = method, ...)
  
  form <- 
    if (!is.null(method) && method == "gam") {
      paste("te(", paste(param_idx, ",", sep = "",
                         collapse = ""), "bs='cr')")
    } else {NULL}
  
  voi_methods <- unname(attr(res, "methods"))
  
  list(
    evppi = res$evppi,
    index = pars,
    k = res$k,
    evi = he$evi,
    fitted.costs = NULL,
    fitted.effects = NULL,
    parameters = paste(pars, collapse = " and "),
    pars = res$pars,
    time = list("Fitting for Effects" = NULL,
                "Fitting for Costs" = NULL,
                "Calculating EVPPI" = NULL),
    fit.c = NULL,
    fit.e = NULL,
    formula = form,
    method = list("Methods for Effects" = voi_methods,
                  "Methods for Costs" = voi_methods),
    res = res)
}

