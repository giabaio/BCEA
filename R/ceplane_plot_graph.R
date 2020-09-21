
#' Cost-Effectiveness Plane Plot By Graph Device
#'
#' Choice of base R, ggplot2 or plotly.
#' @name ceplane_plot_graph
#'
#' @examples
#' # single comparator
#' data(Vaccine)
#' 
#' he <- bcea(e,c)
#' 
#' # need to provide all the defaults because thats what
#' # ceplane.plot() does
#' 
#' graph_params <-  list(xlab = "x-axis label",
#'                       ylab = "y-axis label",
#'                       title = "my title",
#'                       xlim = c(-0.002, 0.001),
#'                       ylim = c(-13, 5),
#'                       point = list(sizes = 1,
#'                                    colors = "darkgrey"),
#'                       area = list(color = "lightgrey"))
#'                       
#' he$delta_e <- as.matrix(he$delta_e)
#' he$delta_c <- as.matrix(he$delta_c)
#' 
#' ceplane_plot_base(he, graph_params = graph_params)
#' 
#' ## single non-default comparator
#' 
#' 
#' ## multiple comparators
#' data(Smoking)
#' 
#' graph_params <-  list(xlab = "x-axis label",
#'                       ylab = "y-axis label",
#'                       title = "my title",
#'                       xlim = c(-1, 2.5),
#'                       ylim = c(-1, 160),
#'                       point = list(sizes = 0.5,
#'                                    colors = grey.colors(3, start = 0.1, end = 0.7)),
#'                       area = list(color = "lightgrey"))
#'                                    
#' he <- bcea(e, c, ref = 4, Kmax = 500, interventions = treats)
#' ceplane_plot_base(he,
#'                   wtp = 200,
#'                   pos_legend = FALSE,
#'                   graph_params = graph_params)
#' 
NULL