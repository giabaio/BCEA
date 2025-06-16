
#' Prepare EIB plot parameters
#' 
#' Parameters general to all plotting devices.
#' 
#' @template args-he
#' @param plot.cri Make title including credible interval? Logical
#' @param ... Additional parameters
#' @return List of graph parameters
#' @keywords internal
#'  
prep_eib_params <- function(he, plot.cri, ...) {
  
  graph_params <- list(...)
  
  default_params <- 
    list(
      xlab = "Willingness to pay",
      ylab = "EIB",
      alpha_cri = 0.05,
      cri.quantile = TRUE,
      area = list(include = FALSE,
                  color = "grey"),
      labels = line_labels(he),
      text = list(
        size = 11),
      line = list(
        type = rep_len(1:6, he$n_comparisons),
        lwd = ifelse(he$n_comparisons > 6, 1.5, 1),
        # color = 1, #1:he$n_comparisons,
        color = "black",
        cri_col = "grey50",
        cri_lty = 2),
      plot.cri = ifelse((is.null(plot.cri) && he$n_comparisons == 1) ||
                          (!is.null(plot.cri) && plot.cri),
                        TRUE, FALSE))
  
  graph_params <- modifyList(default_params, graph_params)
  
  graph_params$main <-
    paste0(
      "Expected Incremental Benefit",
      ifelse(
        default_params$plot.cri,
        paste0("\nand ", format((1 - graph_params$alpha_cri)*100, digits = 4),
               "% credible intervals"),
        ""))
  
  graph_params <- validate_eib_params(graph_params)
  
  graph_params
}


#' Validate EIB parameters
#' 
#' @param params Graph parameters
#' @seealso [prep_eib_params()]
#' @return List of graph parameters
#' @importFrom cli cli_alert_warning
#' @keywords internal
#' 
validate_eib_params <- function(params) {
  
  if (params$alpha_cri < 0 || params$alpha_cri > 1) {
    cli::cli_alert_warning(
      "Argument {.var alpha} must be between 0 and 1. Reset to default value 0.95.")
    params$alpha_cri <- 0.05
  }
  
  if (params$alpha_cri > 0.8 && params$cri.quantile) {
    cli::cli_alert_warning(
      "It is recommended adopting the normal approximation of the credible interval for high values of {.var alpha}.
       Please set the argument {.code cri.quantile = FALSE} to use the normal approximation.")
  }
  
  params
}

