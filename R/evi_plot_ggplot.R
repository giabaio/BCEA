
#' evi_plot_ggplot
#' 
#' @import ggplot2 grid
#' 
evi_plot_ggplot <- function(he,
                            data.psa,
                            plot_aes,
                            plot_annotations) {
  
  if (!(requireNamespace("ggplot2", quietly = TRUE) &
        requireNamespace("grid", quietly = TRUE))) {
    message("falling back to base graphics\n")
    
    evi.plot(he, graph = "base", ...)
    
    return(invisible(NULL))
  }
  
  # no visible binding note
  k <- NA_real_
  
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
      evi.at.kstar[i] <- with(he, evi[which.min(abs(k - kstar[i]))])
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

