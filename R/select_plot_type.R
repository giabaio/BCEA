
#' Choose Graphical Engine
#' 
#' From base R, ggplot2 or plotly.
#'
#' @param graph Type names; string
#' @return Plot ID integer 1:base R; 2:ggplot2; 3:plotly
#' @importFrom cli cli_alert_warning
#' @importFrom purrr map_lgl
#' @keywords dplot internal
#' 
select_plot_type <- function(graph) {
  
  if (missing(graph)) graph <- "base"
  
  graph_lup <- c(base = 1, ggplot2 = 2, plotly =3)
  graph_type <- graph_lup[graph]
  
  is_req_pkgs <- map_lgl(c("ggplot2", "grid"), requireNamespace, quietly = TRUE)
  
  if (graph_type == 2 && !all(is_req_pkgs)) {
    cli::cli_alert_warning(
      "Packages {.pkg ggplot2} and {.pkg grid} not found;
       plot will be rendered using base graphics.")
    graph_type <- 1}
  
  if (graph_type == 3 && !requireNamespace("plotly", quietly = TRUE)) {
    cli::cli_alert_warning(
      "Package {.pkg plotly} not found;
       plot will be rendered using base graphics.")
    graph_type <- 1}
  
  graph_type
}


#'
is_baseplot <- function(graph) {
  
  select_plot_type(graph) == 1
}

#'
is_ggplot <- function(graph) {
  
  select_plot_type(graph) == 2
}

#'
is_plotly <- function(graph) {
  
  select_plot_type(graph) == 3
}


##TODO: this is from eib.plot()
##      do we need to change anything?
##
# # choose graphical engine
# if (any(is.null(graph)) || any(is.na(graph))) graph <- "base"
# 
# graph_choice <- pmatch(graph[1], c("base", "ggplot2", "plotly"), nomatch = 1)
# 
# if (graph_choice == 2 &&
#     !requireNamespace("ggplot2", quietly = TRUE) &
#     requireNamespace("grid", quietly = TRUE)) {
#   warning("Package ggplot2 and grid not found;
#             eib.plot will be rendered using base graphics.")
#   graph_choice <- 1
# }
# if (graph_choice == 3 &&
#     !requireNamespace("plotly", quietly = TRUE)) {
#   warning("Package plotly not found;
#             eib.plot will be rendered using base graphics.")
#   graph_choice <- 1
# }

