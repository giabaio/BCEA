
#' @rdname evppi_plot_graph
#'
#' @param annot Annotate EVPPI curve with parameter names
#'
#' @export
#'
evppi_plot_base.evppi <- function(evppi_obj,
                                  pos_legend = pos,
                                  col = NULL,
                                  annot = FALSE) {
  
  legend_params <-
    evppi_legend_base(evppi_obj, pos_legend, col)
  
  plot(evppi_obj$k,
       evppi_obj$evi,
       type = "l",
       col = legend_params$col[1],
       lty = legend_params$lty[1],
       xlab = "Willingness to pay",
       ylab = "",
       main = "Expected Value of Perfect Partial Information",
       lwd = 2,
       ylim = range(range(evppi_obj$evi),
                    range(evppi_obj$evppi)))
  
  if (!is.list(evppi_obj$evppi))
    evppi_obj$evppi <- list(evppi_obj$evppi)
      
  evppi_dat <- do.call(cbind, evppi_obj$evppi)
  txt_coord_y <- evppi_dat[length(evppi_obj$k), ]
  
  matplot(evppi_obj$k,
          evppi_dat,
          type = "l",
          col = legend_params$col[-1], 
          lty = legend_params$lty[-1],
          add = TRUE)
  
  if (annot) {
    text(x = par("usr")[2],
         y = txt_coord_y, 
         labels = paste0("(", evppi_obj$index, ")", collapse = " "),
         cex = 0.7,
         pos = 2)
  }
  
  do.call(legend, legend_params)
  
  return(invisible(NULL))
}


#' @rdname evppi_plot_graph
#' @export
#' 
evppi_plot_base <- function(evppi_obj, ...) {
  UseMethod('evppi_plot_base', evppi_obj)
}

