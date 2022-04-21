
#' EIB parameters specific to base R plot
#' 
#' @template args-he
#' @param graph_params Type of plot device
#' @param cri_params Credible interval parameters
#' @return list
#' @keywords internal
#' 
eib_params_base <- function(he,
                            graph_params,
                            cri_params) {
  
  ylim <-
    if (!cri_params$plot.cri) {
      range(c(he$eib))
    } else {
      range(c(he$eib),
            cri_params$data$low,
            cri_params$data$upp)
    }
  
  list(
    xlab = graph_params$xlab,
    ylab = graph_params$ylab,
    main = graph_params$main,
    col = graph_params$line$color,
    lwd = graph_params$line$lwd,
    lty = graph_params$line$type,
    type = "l",
    xlim = range(he$k),
    ylim = ylim)
}

