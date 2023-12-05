
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
      "left" = list(orientation = "v", x = 0, y = 0.5),
      "bottomleft" = list(orientation = "v", x = 0, y = 0),
      "topleft" = list(orientation = "v", x = 0, y = 1),
      "right" = list(orientation = "v", x = 1, y = 0.5),
      "bottomright" = list(orientation = "v", x = 1, y = 0),
      "topright" = list(orientation = "v", x = 1, y = 1),
      "bottom" = list(orientation = "v", xanchor = "center", x = 0.5, y = 0),
      "top" = list(orientation = "h", xanchor = "center", x = 0.5, y = 100))
  } else if (all(is.numeric(pos_legend)) && length(pos_legend) == 2) {
    legend_params$x = pos_legend[1]
    legend_params$y = pos_legend[2]
  } else if (is.list(pos_legend)) {
    legend_params = modifyList(legend_params, pos_legend)
  }
  
  legend_params
}