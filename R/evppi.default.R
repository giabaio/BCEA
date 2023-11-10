
#' @rdname evppi
#' @export
#'
evppi.default <- function(he, ...) {
  stop("No method available", call. = FALSE)
}


#' @rdname evppi
#'
#' @importFrom voi evppi
#' @importFrom purrr list_cbind map
#' @examples
#' data(Vaccine, package = "BCEA")
#' treats <- c("Status quo", "Vaccination")
#' bcea_vacc <- bcea(e.pts, c.pts, ref = 2, interventions = treats)
#' inp <- createInputs(vaccine_mat)
#' evppi(bcea_vacc, c("beta.1.", "beta.2."), inp$mat)
#' @export
#' 
evppi.bcea <- function(he,
                       param_idx = NULL,
                       input,
                       N = NULL, 
                       plot = FALSE,
                       residuals = TRUE,
                       method = NULL, ...) {
  
  comp_ids <- c(he$comp, he$ref)
  outputs <- list(e = he$e[, comp_ids],
                  c = he$c[, comp_ids],
                  k = he$k)
  
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
  
  res <- voi::evppi(outputs, inputs = input, pars = pars, method = method,
                    check = TRUE, plot_inla_mesh = plot, ...)
  
  voi_methods <- unname(attr(res, "methods"))
  voi_models <- attr(res, "models")
  
  # method name returned from evppi
  method_nm <- voi_methods[1]
  
  form <- 
    if (method_nm == "gam") {
      paste("te(", paste(param_idx, ",", sep = "",
                         collapse = ""), "bs='cr')")
    } else {NULL}
  
  # fitted values
  get_fitted_values <- residuals && (method_nm %in% c("inla", "gp", "gam"))
  
  # named differently in different methods
  fitted_lup <- c(inla = "fitted", gp = "fitted", gam = "fitted.values")
  
  if (get_fitted_values) {
    fitted_txt <- fitted_lup[method_nm]
    
    fitted_c_ls <-
      purrr::map(voi_models[[1]]$c,
                 ~unname(as.data.frame(.x[[fitted_txt]]))) |>
      rev()
    
    fitted_c <-
      suppressMessages(purrr::list_cbind(fitted_c_ls)) |> 
      as.matrix() |> 
      cbind(0)
    
    fitted_e_ls <-
      purrr::map(voi_models[[1]]$e,
                 ~unname(as.data.frame(.x[[fitted_txt]]))) |>
      rev()
    
    fitted_e <-
      suppressMessages(purrr::list_cbind(fitted_e_ls)) |> 
      as.matrix() |> 
      cbind(0)
  } else {
    fitted_c <- NULL
    fitted_e <- NULL
  }
  
  residuals_out <- 
    list(fitted.costs = fitted_c,
         fitted.effects = fitted_e,
         select = row_idxs)
  
  out <- c(
    list(
      evppi = res$evppi,
      index = pars,
      k = res$k,
      evi = he$evi,
      parameters = paste(pars, collapse = " and "),
      time = list("Fitting for Effects" = NULL,
                  "Fitting for Costs" = NULL,
                  "Calculating EVPPI" = NULL),
      method = list("Methods for Effects" = voi_methods,
                    "Methods for Costs" = voi_methods)),
    residuals_out,
    list(formula = form,
         pars = res$pars,
         res = res))
  
  structure(out, class = c("evppi", class(out)))
}

