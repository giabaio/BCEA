
#' parameters specific to base R plot
#' 
eib_params_base <- function(he,
                            graph_params,
                            cri_params) {
  
  ylim <-
    if (!cri_params$plot.cri) {
      range(c(he$eib))
    } else {
      range(c(he$eib),
            cri_params$data)
    }
  
  list(
    xlab = graph_params$xlab,
    ylab = graph_params$ylab,
    main = paste0(
      graph_params$main,
      ifelse(
        graph_params$plot.cri,
        paste0("\nand ", format((1 - graph_params$alpha_cri)*100, digits = 4),
               "% credible intervals"),
        "")),
    col = graph_params$line$colors,
    lwd = graph_params$line$lwd,
    lty = graph_params$line$types,
    type = "l",
    xlim = range(he$k),
    ylim = ylim)
}

