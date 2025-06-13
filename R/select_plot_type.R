
#' Choose Graphical Engine
#' 
#' From base R, ggplot2 or plotly.
#'
#' @param graph Type names; string
#' @return Plot ID integer 1:base R; 2:ggplot2; 3:plotly
#' @importFrom cli cli_alert_warning
#' @keywords dplot internal
#' 
select_plot_type <- function(graph) {
  
  if (missing(graph)) graph <- "base"
  
  graph_lup <- c(base = 1, ggplot2 = 2, plotly =3)
  graph_type <- graph_lup[graph]
  
  is_req_pkgs <- unname(sapply(c("ggplot2", "grid"),
                               requireNamespace, quietly = TRUE))
  
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
