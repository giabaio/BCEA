
#' ceplane_plot_base
#'
#' @template args-he
#' @param comparison 
#' @param wtp 
#' @param graph_params 
#' @param ... 
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
#' ceplane_plot_base(he)
#' 
#' ## multiple comparators
#' data(Smoking)
#' 
#' m <- bcea(e, c, ref = 4, Kmax = 500, interventions = treats)
#' ceplane_plot_base(m, wtp = 200)
#' 
ceplane_plot_base <- function(he,
                              comparison = NULL,
                              wtp = 25000,
                              graph_params,
                              ...) {
  
  if (is.null(comparison)) comparison <- 1:he$n_comparisons
  
  # encodes characters to save graphs as ps or pdf
  ps.options(encoding = "CP1250")
  pdf.options(encoding = "CP1250")
  
  axes_params <- axes_params(he, comparison, wtp)
  base_params <- ceplane_base_params(he, comparison, graph_params)
  
  add_ceplane_setup(axes_params, base_params)
  add_ceplane_polygon(axes_params, base_params)
  add_ceplane_points(he, comparison, base_params)
  abline(h = 0, v = 0, col = "dark grey")
  add_ceplane_icer(comparison, axes_params, base_params)
  add_ceplane_k_txt(axes_params, wtp)
  add_ceplane_legend(he, comparison, base_params)
}

