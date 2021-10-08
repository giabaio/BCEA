
#' @rdname evi_plot_graph
#' EVI plot ggplot version
#' 
#' @template args-he
#' @param data.psa Data
#' @param plot_aes Aesthetic parameters
#' @param plot_annotations Plot parameters
#' 
#' @import ggplot2 grid
#' @export
#' 
evi_plot_ggplot <- function(he,
                            data.psa,
                            plot_aes,
                            plot_annotations) {
  
  evi <-
    ggplot(data.psa, aes(.data$k, .data$evi)) +
    geom_line(
      colour = plot_aes$line$colors,
      lty = ifelse(is.null(plot_aes$line$types), 1, plot_aes$line$type)) +
    theme_bw() +
    labs(title = plot_annotations$title,
         x = plot_annotations$xlab,
         y = plot_annotations$ylab)
  
  if (length(he$kstar) != 0) {
    kstars <- length(he$kstar)
    evi.at.kstar <- numeric(kstars)
    
    for (i in seq_len(kstars)) {
      evi.at.kstar[i] <- he$evi[which.min(abs(he$k - he$kstar[i]))]
    }
    
    for (i in seq_len(kstars)) {
      evi <- evi + 
        annotate(
          "segment",
          x = he$kstar[i],
          xend = he$kstar[i],
          y = evi.at.kstar[i],
          yend = -Inf,
          linetype = 2,
          colour = "grey50") +
        annotate(
          "segment",
          x = he$kstar[i],
          xend = -Inf,
          y = evi.at.kstar[i],
          yend = evi.at.kstar[i],
          linetype = 2,
          colour = "grey50")
    }
  }
  
  evi +
    theme(text = element_text(size = 11),
          legend.key.size = grid::unit(0.66, "lines"),
          legend.spacing = grid::unit(-1.25, "line"),
          panel.grid = element_blank(),
          legend.key = element_blank(),
          plot.title = element_text(
            lineheight = 1.05,
            face = "bold",
            size = 14.3,
            hjust = 0.5))
}

