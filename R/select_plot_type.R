
# choose graphical engine
#
select_plot_type <- function(graph) {
  
  if (is.null(graph) || is.na(graph)) graph <- "base"
  
  graph_type <- pmatch(graph[1], c("base", "ggplot2", "plotly"), nomatch = 1)
  is_pkg_avail <-
    requireNamespace("ggplot2", quietly = TRUE) & requireNamespace("grid", quietly = TRUE)
  
  # check feasibility
  if (graph_type == 2 && !is_pkg_avail) {
    warning(
      "Package ggplot2 and grid not found; plot will be rendered using base graphics.", call. = FALSE)
    graph_type <- 1}
  
  if (graph_type == 3 && !requireNamespace("plotly", quietly = TRUE)) {
    warning(
      "Package plotly not found; plot will be rendered using base graphics.", call. = FALSE)
    graph_type <- 1}
  
  graph_type
}