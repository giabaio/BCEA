
#' ceplane_base_params
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
  
  default_params <- list(area = 
                           list(color = "grey95",
                                line_color = "black"),
                         point = list(sizes = 0.35,
                                      colors = grey.colors(n = length(comparison),
                                                           end = 0.7,
                                                           alpha = 1)),
                         ICER = list(colors = "red",
                                     size = 1),
                         legend = list(x = "topright",
                                       legend = paste(he$interventions[he$ref],
                                                      "vs",
                                                      he$interventions[comparison]), ##TODO: is this out-by-one?
                                       col = grey.colors(n = length(comparison),
                                                         end = 0.7,
                                                         alpha = 1),
                                       cex = 0.7,
                                       bty = "n",
                                       pch = 16))
  
  graph_params <- modifyList(default_params, graph_params)
  
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
       icer_text =
         list(labels = 
                if (length(comparison) == 1) {   ##TODO: could move to add_k_txt()?
                  paste("\U2022",
                        " ICER = ",
                        format(he$ICER[comparison],
                               digits = 6,
                               nsmall = 2),
                        sep = "")
                } else {
                  ""},
              cex = 0.95,
              pos = 2,
              col = graph_params$ICER$colors),
       icer_points =
         list(pch = 20,
              col = graph_params$ICER$colors,
              cex = graph_params$ICER$sizes),
       legend = graph_params$legend)
}

