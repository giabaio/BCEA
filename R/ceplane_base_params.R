
#' ceplane_base_params
#'
#' Reformat from ggplot format to
#' list of base parameters.
#'
#' @template args-he 
#' @param comparison
#' @param graph_params 
#'
#' @return
#' @export
#' @keywords dplot
#'
#' @examples
#' 
ceplane_base_params <- function(he,
                                comparison,
                                graph_params) {
  list(setup =
         list(x = NULL,
              axes = FALSE,
              xlab = graph_params$xlab,
              ylab = graph_params$ylab,
              main = graph_params$title),
       polygon =
         list(col = graph_params$area$color,
              border = graph_params$area$line_color),
       points =
         list(pch = 20,
              cex = graph_params$point$sizes,
              col = graph_params$point$colors),
       k_txt =
         list(cex = 0.8,
              pos = 4, 
              adj = c(0, 0)),
       icer_text =
         list(labels = icer_label(he, comparison),
              cex = 0.95,
              pos = 2,
              col = "red"),
       icer_points =
         list(pch = 20,
              col = "red",
              cex = 1))
}

#
icer_label <- function(he, comparison) {
  
  if (length(comparison) == 1) {
    return(
      paste0("\U2022",
             " ICER = ",
             format(he$ICER[comparison],
                    digits = 6,
                    nsmall = 2)))
  } 
  return("")
}

