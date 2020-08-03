
#' ceplane_plot_base
#'
#' @template args-he
#' @param wtp Willingness to pay threshold; default 25,000
#' @param pos_legend Legend position
#' @param graph_params Graph parameters in ggplot format
#' 
#' @return 
#' @keywords hplot
#' 
#' @examples
#'
#' ## single comparator
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
#' ceplane_plot_base(he, wtp = 200, pos_legend = FALSE, graph_params = graph_params)
#' 
ceplane_plot_base <- function(he,
                              wtp = 25000,
                              pos_legend,
                              graph_params) {
  
  plot_params <-
    ceplane_base_params(he, wtp, graph_params)

  legend_params <-
    ceplane_legend_base(he, pos_legend, plot_params)
  
  add_ceplane_setup(plot_params)
  add_ceplane_polygon(plot_params)
  add_ceplane_points(he, plot_params)
  add_axes()
  add_ceplane_icer(he, plot_params)
  add_ceplane_k_txt(plot_params)
  add_ceplane_legend(legend_params)
}

