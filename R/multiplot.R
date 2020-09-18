
#' Plot Multiple bcea Graphs
#' 
#' Arrange plots in grid. Sourced from R graphics cookbook.
#' 
#' @param plotlist List of ggplot objects
#' @param cols Number of columns
#' @param layout Matrix
#' @param extra_args List of plotting arguments
#' @param ... Potential further arguments
#' @return ggplot TableGrob object
#' 
#' @importFrom gridExtra grid.arrange
#' @export
#' 
multiplot <- function(plotlist = NULL,
                      cols = 1,
                      layout_config = NULL) {
  
  n_plots <- length(plotlist)
  
  if (is.null(layout_config)) {
    layout_config <- matrix(seq(1, cols*ceiling(n_plots/cols)),
                            ncol = cols,
                            nrow = ceiling(n_plots/cols))
  }
  
  grid_params <-
    c(plotlist, list(layout_matrix = layout_config))
  
  do.call("grid.arrange", grid_params)
}
