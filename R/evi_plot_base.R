
#' EVI base R version
#' 
#' @template args-he
#' @param data.psa Data
#' @param plot_aes Aesthetic parameters
#' @param plot_annotations Plot parameters
#' 
evi_plot_base <- function(he,
                          data.psa,
                          plot_aes,
                          plot_annotations) {
  
  plot(
    x = data.psa$k, y = data.psa$evi,
    type = "l",
    xlab = plot_annotations$xlab,
    ylab = plot_annotations$ylab,
    main = plot_annotations$title,
    col = plot_aes$line$colors,
    lty = ifelse(is.null(plot_aes$line$types), 1, plot_aes$line$type))
  
  pts_lty <- 2
  pts_col <- "dark grey"
  
  if (length(he$kstar) == 1) {
    points(
      x = rep(he$kstar, 3),
      y = c(-10000, he$evi[he$k == he$kstar] / 2, he$evi[he$k == he$kstar]),
      type = "l",
      lty = pts_lty,
      col = pts_col)
    
    points(x = c(-10000, he$kstar / 2, he$kstar),
           y = rep(he$evi[he$k == he$kstar], 3),
           type = "l",
           lty = pts_lty,
           col = pts_col)
  }
  
  if (length(he$kstar) > 1) {
    for (i in seq_along(he$kstar)) {
      points(
        x = rep(he$kstar[i], 3),
        y = c(-10000, he$evi[he$k == he$kstar[i]] / 2, he$evi[he$k == he$kstar[i]]),
        type = "l",
        lty = pts_lty,
        col = pts_col)
    
      points(
        x = c(-10000, he$kstar[i] / 2, he$kstar[i]),
        y = rep(he$evi[he$k == he$kstar[i]], 3),
        type = "l",
        lty = pts_lty,
        col = pts_col)
    }
  }
}

