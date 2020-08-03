
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
#' graph_params <-  list(xlab = "x-axis label",
#'                       ylab = "y-axis label",
#'                       title = "my title",
#'                       xlim = NULL,
#'                       ylim = NULL)
#' 
#' ceplane_plot_base(he, graph_params = graph_params)
#' 
#' ## single non-default comparator
#' 
#' 
#' ## multiple comparators
#' data(Smoking)
#' 
#' m <- bcea(e, c, ref = 4, Kmax = 500, interventions = treats)
#' ceplane_plot_base(m, wtp = 200, graph_params = graph_params)
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
  add_ceplane_icer(plot_params)
  add_ceplane_k_txt(plot_params)
  add_ceplane_legend(legend_params)
}

