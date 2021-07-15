
#' evi_plot_base
#' 
evi_plot_base <- function(he,
                          data.psa,
                          plot_aes,
                          plot_annotations) {
  
  plot(
    data.psa$k, data.psa$evi,
    type = "l",
    xlab = plot_annotations$xlab,
    ylab = plot_annotations$ylab,
    main = plot_annotations$title,
    col = plot_aes$line$colors,
    lty = ifelse(is.null(plot_aes$line$types), 1, plot_aes$line$type))
  
  if (length(he$kstar) == 1) {
    points(
      rep(he$kstar, 3), c(-10000, he$evi[he$k == he$kstar] / 2, he$evi[he$k == he$kstar]),
      type = "l",
      lty = 2,
      col = "dark grey")
    points(c(-10000, he$kstar / 2, he$kstar), rep(he$evi[he$k == he$kstar], 3),
           type = "l",
           lty = 2,
           col = "dark grey")
  }
  
  if (length(he$kstar) > 1) {
    for (i in seq_along(he$kstar)) {
      points(
        rep(he$kstar[i], 3), c(-10000, he$evi[he$k == he$kstar[i]] / 2, he$evi[he$k == he$kstar[i]]),
        type = "l",
        lty = 2,
        col = "dark grey")
      points(
        c(-10000, he$kstar[i] / 2, he$kstar[i]), rep(he$evi[he$k == he$kstar[i]], 3),
        type = "l",
        lty = 2,
        col = "dark grey")
    }
  }
}

