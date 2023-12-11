#' Reshape BCEA object in long format (delta_ce)
#'
#' @param he a BCEA object of interest
#'
#' @return a data.frame, displaying the BCEA object information in long format
#' 
#' @importFrom dplyr mutate
#' @importFrom reshape2 melt
#'
#' @keywords internal
prep_delta_ce = function(he) {
  merge(
    melt(
      cbind(sim = seq_len(nrow(he$delta_c)),
            he$delta_c),
      variable.name = "comparison",
      value.name = "delta_c",
      id.vars = "sim"),
    melt(
      cbind(sim = seq_len(nrow(he$delta_e)),
            he$delta_e),
      variable.name = "comparison",
      value.name = "delta_e",
      id.vars = "sim"),
    by = c("sim", "comparison")) |>
    mutate(comparison = factor(.data$comparison))
}
