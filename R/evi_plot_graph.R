
#' Expected Value of Information Plot By Graph Device
#'
#' Choice of base R, \pkg{ggplot2} or \pkg{plotly}.
#' @name evi_plot_graph
#' 
NULL


#' EVI plot base R version
#' @rdname evi_plot_graph
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


#' EVI plot ggplot version
#' @rdname evi_plot_graph
#' 
#' @template args-he
#' @param data.psa Data
#' @param plot_aes Aesthetic parameters
#' @param plot_annotations Plot parameters
#' 
#' @import ggplot2 grid
#' 
evi_plot_ggplot <- function(he,
                            data.psa,
                            plot_aes,
                            plot_annotations) {
  text_params <- list(
    size =
      if (is.rel(plot_annotations$text$size)) {
        11 * unclass(plot_annotations$text$size)  # theme_get()$text$size
      } else {
        plot_annotations$text$size
      })
  
  # core plot
  evi <-
    ggplot(data.psa, aes(.data$k, .data$evi)) +
    geom_line(
      color = plot_aes$line$colors,
      lty = ifelse(is.null(plot_aes$line$types), 1, plot_aes$line$type)) +
    theme_bw() +
    labs(title = plot_annotations$title,
         x = plot_annotations$xlab,
         y = plot_annotations$ylab) +
    do.call(theme, list(
      axis.text = element_text(size = text_params$size),
      axis.title.x = element_text(size = text_params$size),
      axis.title.y = element_text(size = text_params$size))) 
  
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
          color = "grey50") +
        annotate(
          "segment",
          x = he$kstar[i],
          xend = -Inf,
          y = evi.at.kstar[i],
          yend = evi.at.kstar[i],
          linetype = 2,
          color = "grey50")
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



#' EVI plot plotly version
#' @rdname evi_plot_graph
#' 
#' @param data.psa Data
#' @param plot_aes Aesthetic parameters
#' @param plot_annotations Plot parameters
#' 
evi_plot_plotly <- function(data.psa,
                            plot_aes,
                            plot_annotations) {
  
  plot_aes$area$color <- 
    sapply(plot_aes$area$color,
           function(x)
             ifelse(grepl(pattern = "^rgba\\(", x = x), x, plotly::toRGB(x, 0.4)))
  
  legend_list <- list(orientation = "h",
                      xanchor = "center",
                      x = 0.5)
  # actual plot
  evi <- plotly::plot_ly(data.psa, x = ~k)
  
  evi <- plotly::add_trace(
    evi,
    y = ~evi,
    type = "scatter",
    mode = "lines",
    name = "EVPI",
    fill = ifelse(plot_aes$area$include, "tozeroy", "none"),
    fillcolor = plot_aes$area$color,
    line = list(
      color = plot_aes$line$colors[1],
      dash = c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot")[
        ifelse(is.null(plot_aes$line$types), 1, plot_aes$line$types)]))
  
  evi <-
    plotly::layout(
      evi,
      title = plot_annotations$title,
      xaxis = list(
        hoverformat = ".2f",
        title = plot_annotations$xlab),
      yaxis = list(
        hoverformat = ".2f",
        title = plot_annotations$ylab),
      # legend hidden by default (single series)
      showlegend = FALSE,
      legend = legend_list)
  
  plotly::config(evi, displayModeBar = FALSE)
}

