
#' parameters general to all plotting devices
#' 
prep_eib_params <- function(he, ...) {
  
  graph_params <- list(...)
  
  ##TODO: what is this?...
  # # if existing, read and store graphical options
  # for (aes_arg in plot_aes_args) {
  #   if (exists(aes_arg, where = exArgs)) {
  #     aes_cat <- strsplit(aes_arg, "_")[[1]][1]
  #     aes_name <- paste0(strsplit(aes_arg, "_")[[1]][-1], collapse = "_")
  #     plot_aes[[aes_cat]][[aes_name]] <- exArgs[[aes_arg]]
  #   }
  # }
  
  default_params <- 
    list(
      xlab = "Willingness to pay",
      ylab = "EIB",
      main = "Expected Incremental Benefit",
      alpha_cri = 0.05,
      cri.quantile = TRUE,
      area = list(include = FALSE,
                  color = "grey"),
      line = list(
        types = 1,
        lwd = 1,
        colors = "black",
        cri_col = "grey50",
        cri_lty = 2))
  
  graph_params <- modifyList(default_params, graph_params)
  
  graph_params$cri <-
    compute_eib_cri(he,
                    graph_params$alpha_cri,
                    graph_params$cri.quantile)
  
  graph_params <- validate_eib_params(graph_params)
  
  graph_params
}


#'
validate_eib_params <- function(params) {
  
  if (params$alpha_cri < 0 | params$alpha_cri > 1) {
    warning("Argument alpha must be between 0 and 1. Reset to default value 0.95.")
    params$alpha_cri <- 0.05
  }
  
  if (params$alpha_cri > 0.80 & params$cri.quantile) {
    warning(
      "It is recommended adopting the normal approximation of the credible interval for high values of alpha.
       Please set the argument cri.quantile = FALSE to use the normal approximation.")
  }
  
  params
}

