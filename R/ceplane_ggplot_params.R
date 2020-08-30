
#' @import ggplot2
#'
ceplane_ggplot_params <- function(he, graph_params) {
  
  if (is.null(size))
    size <- rel(3.5)  # relative size
  
  label.pos <- TRUE
  opt.theme <- theme()
  
  if (!plot_aes$exist$ICER$sizes)
    plot_aes$ICER$sizes <- ifelse(he$n_comparisons == 1, 2, 0)
  
  if (length(exArgs) >= 1) {
    if (exists("label.pos", where = exArgs))
      if (is.logical(exArgs$label.pos))
        label.pos <- exArgs$label.pos
      for (obj in exArgs)
        if (is.theme(obj))
          opt.theme <- opt.theme + obj
  }
  
  # dataset for ICERs
  means <- matrix(NA_real_, nrow = he$n_comparisons, ncol = 2)
  
  for (i in 1:he$n_comparisons)
    means[i, ] <- colMeans(kd[kd$comparison == i, -3])
  
  means <- data.frame(means)
  means$comparison <- factor(1:he$n_comparisons)
  names(means) <- c("lambda.e", "lambda.c", "comparison")
  
  
  scale_size_manual <- 
    if (!plot_aes$exist$point$sizes)
      rep_len(1, length(comparisons.label))
  else
    rep_len(plot_aes$point$sizes,
            length(comparisons.label))
  
  
  # labels for legend
  comparisons.label <-
    with(he, paste0(interventions[ref], " vs ", interventions[comp]))
  
  # polygon
  do.nothing <- function(x, limits) x
  
  # plot limits
  range.e <- range(kd$delta_e)
  range.c <- range(kd$delta_c)
  range.e[1] <- ifelse(range.e[1] < 0, range.e[1], -range.e[1])
  range.c[1] <- ifelse(range.c[1] < 0, range.c[1], -range.c[1])
  
  # build a trapezoidal plane instead of a triangle if the y value is
  # less than the minimum difference on costs
  
  # ce plane data
  x1 <- range.e[1] - 2 * abs(diff(range.e))
  x2 <- range.e[2] + 2 * abs(diff(range.e))
  x3 <- x2
  x <- c(x1, x2, x3)
  y <- x * wtp
  y[3] <- x1 * wtp
  plane <-
    data.frame(x = x,
               y = y,
               comparison = factor(rep(he$n_comparisons + 1, 3)))
  
  if (y[1] > min(kd$delta_c)) {
    plane <- rbind(plane,
                   c(x2, 2 * min(kd$delta_c), he$n_comparisons + 1),
                   #new bottom-right vertex
                   c(x1, 2 * min(kd$delta_c), he$n_comparisons + 1)) #new bottom-left vertex
  }
  
  # wtp label
  if (!label.pos) {
    ceplane <- ceplane +
      annotate(
        geom = "text",
        x = ifelse(range.c[1] / wtp > range.e[1], range.c[1] /
                     wtp, range.e[1]),
        y = range.c[1],
        label = paste0("k = ", format(wtp, digits = 6), "  "),
        hjust = 0.15,
        size = size)
  } else {
    m.e <- ifelse(range.e[1] < 0, range.e[1], -range.e[1])
    m.c <- ifelse(range.c[1] < 0, range.c[1], -range.c[1])
    x.pt <- 0.95 * m.e
    y.pt <- ifelse(x.pt * wtp < m.c,
                   m.c,
                   x.pt * wtp)
    ceplane <- ceplane +
      annotate(
        geom = "text",
        x = x.pt,
        y = y.pt,
        label = paste0("k = ", format(wtp, digits = 6)),
        hjust = 0.15,
        size = size)
  }
  
  
  # legend
  
  jus <- NULL
  
  if (alt.legend) {
    alt.legend = "bottom"
    ceplane <- ceplane + theme(legend.direction = "vertical")
  } else {
    if (is.character(alt.legend)) {
      choices <- c("left", "right", "bottom", "top")
      alt.legend <- choices[pmatch(alt.legend, choices)]
      jus <- "center"
      if (is.na(alt.legend))
        alt.legend <- FALSE
    }
    
    if (length(alt.legend) > 1)
      jus <- alt.legend
    
    if (length(alt.legend) == 1 & !is.character(alt.legend)) {
      alt.legend <- c(1, 1)
      jus <- alt.legend
    }
  }
  
  annotate_line_params <- 
    list(
      geom = "line",
      x = plane[1:2, 1],
      y = plane[1:2, 2],
      color = ifelse(
        !plot_aes$exist$area$line_color,
        "black",
        plot_aes$area$line_color))
  
  
  annotate_polygon_params <- 
    list(
      geom = "polygon",
      x = plane$x,
      y = plane$y,
      fill = ifelse(
        is.null(plot_aes$area$color),
        "light gray",
        plot_aes$area$color),
      alpha = 0.3)

annotate_wtp_params <- 
  list(
    geom = "text",
    x = x.pt,
    y = y.pt,
    label = paste0("k = ", format(wtp, digits = 6)),
    hjust = 0.15,
    size = size)

}