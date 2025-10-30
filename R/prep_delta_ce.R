#' Reshape BCEA object in long format (delta_ce)
#'
#' @param he a BCEA object of interest
#'
#' @return a data.frame, displaying the BCEA object information in long format
#'
#' @importFrom dplyr mutate
#' @importFrom tidyr pivot_longer
#'
#' @keywords internal
prep_delta_ce <- function(he) {
  delta_c <- he$delta_c |>
    as_tibble() |>
    mutate(sim = dplyr::row_number()) |>
    pivot_longer(
      cols = -"sim",
      names_to = "comparison",
      values_to = "delta_c"
    )
  
  delta_e <- he$delta_e |>
    as_tibble() |>
    mutate(sim = dplyr::row_number()) |>
    pivot_longer(
      cols = -"sim",
      names_to = "comparison",
      values_to = "delta_e"
    )
  
  delta_c |>
    left_join(delta_e, by = c("sim", "comparison")) |>
    mutate(comparison = factor(.data$comparison)) |> as.data.frame()
}



