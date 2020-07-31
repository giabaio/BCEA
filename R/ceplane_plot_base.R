
#' ceplane_plot_base
#'
#' @template args-he
#' @param comparison Interventions to compare against reference
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
                              comparison = NULL,
                              wtp = 25000,
                              pos_legend,
                              graph_params) {
  
  if (is.null(comparison)) comparison <- 1:he$n_comparisons
  
  # encodes characters to save graphs as postscript or pdf
  ps.options(encoding = "CP1250")
  pdf.options(encoding = "CP1250")
  
  axes_params <- ceplane_axes_params(he, comparison, wtp, graph_params)
  base_params <- ceplane_base_params(he, comparison, graph_params)
  legend_params <- make_legend_base(he, pos_legend, base_params)
  
  add_ceplane_setup(axes_params, base_params)
  add_ceplane_polygon(axes_params, base_params)
  add_ceplane_points(he, comparison, base_params)
  abline(h = 0, v = 0, col = "dark grey")
  add_ceplane_icer(comparison, axes_params, base_params)
  add_ceplane_k_txt(axes_params, wtp)
  add_ceplane_legend(comparison, legend_params)
}

