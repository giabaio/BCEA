
#' parameters specific to base R plot
#' 
eib_params_base <- function(he, graph_params) {
  
  ylim <-
    if (!graph_params$plot.cri) {
      range(c(he$eib))
    } else {
      range(c(he$eib),
            c(graph_params$cri[, 1:2]))
    }
  
  default_params <-  list()
  
  plot_params <- 
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
      # vertical limits
      ylim = ylim,
      plot.cri = graph_params$plot.cri,
      cri = graph_params$cri,
      line = list(cri_col = graph_params$line$cri_col,
                  cri_lty =  graph_params$line$cri_lty))
  
  plot_params <- modifyList(default_params, plot_params)
  
  ##TODO:
  # comparison > 1
  #
  # plot_params <-
  #   list(
  #     lwd = ifelse(he$n_comparisons > 6, 1.5, 1),
  #     lwd = ifelse(graph_params$plot.cri, graph_params$lwd + 1, graph_params$lwd), 
  #     lty = graph_params$plot_aes$line$types[min(j, length(graph_params$plot_aes$line$types))], 
  #     col = graph_params$plot_aes$line$colors[min(j, length(graph_params$plot_aes$line$colors))])
  
  plot_params  
}

