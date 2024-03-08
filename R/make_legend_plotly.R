
#' Legend Positioning
#' 
#' @param pos_legend Position of legend
#' @return String
#'
#' @export
#' @keywords internal
#' 
make_legend_plotly <- function(pos_legend) {
  
  legend_params <- list(orientation = "v",
                        xanchor = "center",
                        x = 1.15,
                        y = 0.5)
  
  if (is.character(pos_legend)) {
    legend_params <- switch(
      pos_legend,
      "left" = list(orientation = "v", x = 0, y = 0.5, xanchor = "left", yanchor = "center"),
      "bottomleft" = list(orientation = "v", x = 0, y = 0, xanchor = "left", yanchor = "bottom"),
      "topleft" = list(orientation = "v", x = 0, y = 1, xanchor = "left", yanchor = "top"),
      "right" = list(orientation = "v", x = 1, y = 0.5, xanchor = "right", yanchor = "center"),
      "bottomright" = list(orientation = "v", x = 1, y = 0, xanchor = "right", yanchor = "bottom"),
      "topright" = list(orientation = "v", x = 1, y = 1, xanchor = "right", yanchor = "top"),
      "bottom" = list(orientation = "v", x = 0.5, y = 0, xanchor = "center", yanchor = "bottom"),
      "top" = list(orientation = "h", x = 0.5, y = 100, xanchor = "center", yanchor = "bottom"))
  } else if (all(is.numeric(pos_legend)) && length(pos_legend) == 2) {
    legend_params$x = pos_legend[1]
    legend_params$y = pos_legend[2]
  } else if (is.list(pos_legend)) {
    legend_params = modifyList(legend_params, pos_legend)
  }
  
  legend_params
}