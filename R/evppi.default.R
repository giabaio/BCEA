
#' @rdname evppi
#' @export
#'
evppi.default <- function(he, ...) {
  stop("No method available", call. = FALSE)
}


#' @rdname evppi
#' @import voi
#' 
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
  
  has_fitted_values <- residuals && (method_nm %in% c("inla", "gp", "gam"))
  
  if (has_fitted_values) {
    fitted_c <- get_fitted_("c", voi_methods, voi_models)
    fitted_e <- get_fitted_("e", voi_methods, voi_models)
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

#' Get fitted values from evppi object
#' 
#' @importFrom purrr list_cbind map
#' @keywords internal
#' @return matrix
#
get_fitted_ <- function(val, voi_methods, voi_models) {
  
  method_nm <- voi_methods[1]
  
  # named differently in different methods
  fitted_lup <- c(inla = "fitted", gp = "fitted", gam = "fitted.values")
  
  fitted_txt <- fitted_lup[method_nm]
  
  fitted_ls <-
    purrr::map(voi_models[[1]][[val]],
               ~unname(as.data.frame(.x[[fitted_txt]]))) |>
    rev()
  
  suppressMessages(
    purrr::list_cbind(fitted_ls)) |> 
    as.matrix() |> 
    cbind(0)
}
