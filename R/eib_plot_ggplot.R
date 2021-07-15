
#' eib_plot_ggplot
#' 
#' @import ggplot2 grid
#' 
eib_plot_ggplot <- function(he,
                            alt.legend,
                            plot_aes,
                            data.psa,
                            plot.cri,
                            plot_annotations,
                            alpha,
                            size,
                            exArgs,
                            yl,
                            cri) {
  
  if (!(requireNamespace("ggplot2", quietly = TRUE) &
        requireNamespace("grid", quietly = TRUE))) {
    message("falling back to base graphics\n")
    
    eib.plot(he, pos = alt.legend, graph = "base")
    return(invisible(NULL))
  }
  
  ### no visible binding note
  k <- kstar <- low <- upp <- NA_real_
  
  if (is.null(size))
    size <- rel(3.5)
  
  opt.theme <- theme()
  for (obj in exArgs)
    if (is.theme(obj))
      opt.theme <- opt.theme + obj
  
  if (he$n_comparisons == 1) {
    
    data.psa <- data.frame(
      k = he$k,
      eib = c(he$eib), 
      comparison = as.factor(
        rep(1:he$n_comparison, each = length(he$k)))
    )
    
    if (plot.cri) 
      data.psa <- cbind(data.psa, cri)
    
    if (is.null(plot_aes$line$types))
      plot_aes$line$types <- 1:he$n_comparisons
    
    eib <-
      ggplot(data.psa, aes(x = .data$k, y = .data$eib)) +
      theme_bw() +
      geom_hline(aes(yintercept = 0), colour = "grey")
    
    if (!he$change_comp) {
      eib <-
        eib +
        geom_line(
          linetype = plot_aes$line$types[1],
          colour = plot_aes$line$colors[1])
    } else {
      eib <-
        eib + 
        geom_line(linetype = plot_aes$line$types[1],
                  colour = plot_aes$line$colors[1]) +
        scale_linetype_manual(
          "",
          values = ifelse(is.null(plot_aes$line$types), 1, plot_aes$line$types[1]),
          labels = with(he, paste0(interventions[ref], " vs ", interventions[comp])))
    }
    
    if (!length(he$kstar) == 0 && !is.na(size)) {
      label <- paste0("k* = ", format(he$kstar, digits = 6))
      eib <-
        eib +
        geom_vline(
          aes(xintercept = .data$kstar),
          data = data.frame("kstar" = he$kstar),
          colour = "grey50",
          linetype = 2,
          size = 0.5) +
        annotate(
          "text",
          label = label,
          x = he$kstar,
          y = min(yl),
          hjust = ifelse((max(he$k) - he$kstar) / max(he$k) > 1 / 6,
                         yes = -0.1,
                         no = 1.1),
          size = size)
    }
    if (plot.cri) {
      eib <- eib +
        geom_line(aes(y = .data$low),
                  colour = plot_aes$line$cri_colors[1],
                  lty = 2) +
        geom_line(aes(y = .data$upp),
                  colour = plot_aes$line$cri_colors[1],
                  lty = 2)
    }
  } else if (he$n_comparisons > 1 &&
             is.null(comparison)) {
    data.psa <-
      data.frame(
        k = c(he$k),
        eib = c(he$eib),
        comparison = as.factor(
          rep(1:he$n_comparison, each = length(he$k)))
      )
    if (plot.cri)
      data.psa <- cbind(data.psa, cri)
    comparisons.label <-
      paste0(he$interventions[he$ref], " vs ", he$interventions[he$comp])
    
    # linetype is the indicator of the comparison.
    # 1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash
    if (is.null(plot_aes$line$types))
      plot_aes$line$types <- rep(c(1,2,3,4,5,6), ceiling(he$n_comparisons/6))[1:he$n_comparisons]
    
    if (length(plot_aes$line$types) < length(comparisons.label))
      plot_aes$line$types <- rep_len(plot_aes$line$types, length(comparisons.label))
    
    if (length(plot_aes$line$colors) < length(comparisons.label))
      plot_aes$line$colors <- rep_len(plot_aes$line$colors, length(comparisons.label))
    
    eib <- 
      ggplot(
        data.psa,
        aes(x = .data$k, y = .data$eib,
            linetype = .data$comparison,
            colour = .data$comparison)) + 
      geom_hline(yintercept = 0, linetype = 1, color = "grey") + 
      theme_bw() +
      geom_line(lwd = ifelse(!plot.cri, 0.5, 0.75)) +
      scale_colour_manual(
        "",
        labels = comparisons.label, 
        values = plot_aes$line$colors) +
      scale_linetype_manual(
        "",
        labels = comparisons.label, 
        values = plot_aes$line$types)
    
    if (!length(he$kstar) == 0 && !is.na(size)) {
      label <- paste0("k* = ", format(he$kstar, digits = 6))
      eib <-
        eib +
        geom_vline(
          aes(xintercept = .data$kstar),
          data = data.frame("kstar" = he$kstar),
          colour = "grey50",
          linetype = 2,
          size = 0.5) +
        annotate(
          "text",
          label = label,
          x = he$kstar,
          y = min(yl),
          hjust = ifelse((max(he$k) - he$kstar) / max(he$k) > 1/6,
                         -0.1, 1.1),
          size = size,
          vjust = 1)
    }
    
    if (plot.cri) {
      eib <-
        eib +
        geom_line(aes(y = .data$low),
                  colour = plot_aes$line$cri_colors,
                  show.legend = FALSE) +
        geom_line(aes(y = .data$upp),
                  colour = plot_aes$line$cri_colors,
                  show.legend = FALSE)
    }
  } else if (he$n_comparisons > 1 & !is.null(comparison)) {
    # adjusts bcea object for the correct number of dimensions and comparators
    he$comp <- he$comp[comparison]
    he$delta_e <- he$delta_e[, comparison]
    he$delta_c <- he$delta_c[, comparison]
    he$n_comparators <- length(comparison) + 1
    he$n_comparisons <- length(comparison)
    he$interventions <- he$interventions[sort(c(he$ref, he$comp))]
    he$ICER <- he$ICER[comparison]
    he$ib <- he$ib[, , comparison]
    he$eib <- he$eib[, comparison]
    he$U <- he$U[, , sort(c(he$ref, comparison + 1))]
    he$ceac <- he$ceac[, comparison]
    he$ref <- rank(c(he$ref, he$comp))[1]
    he$comp <- rank(c(he$ref, he$comp))[-1]
    he$change_comp <- TRUE
    
    return(
      eib.plot(
        he,
        pos = alt.legend,
        graph = "ggplot2",
        size = size,
        comparison = NULL,
        plot.cri = plot.cri,
        alpha = alpha,
        cri.quantile = cri.quantile,
        ...))
  }
  
  eib <- eib + 
    ggplot2::labs(
      x = switch(
        as.numeric(plot_annotations$exist$xlab) + 1,
        "Willingness to pay",
        plot_annotations$xlab),
      y = switch(
        as.numeric(plot_annotations$exist$ylab) + 1,
        "EIB",
        plot_annotations$ylab),
      title = switch(
        as.numeric(plot_annotations$exist$title) + 1, 
        paste0("Expected Incremental Benefit", ifelse(
          plot.cri,
          paste0("\nand ", format((1 - alpha)*100, digits = 4),
                 "% credible intervals"), "")),
        plot_annotations$title))
  
  jus <- NULL
  
  if (length(alt.legend) == 1 && alt.legend)  {
    alt.legend <- "bottom"
    eib <- eib + theme(legend.direction = "vertical")
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
    if (length(alt.legend) == 1 && !is.character(alt.legend)) {
      alt.legend <- c(1, 0)
      jus <- alt.legend
    }
  }
  
  eib + 
    theme(
      legend.position = alt.legend,
      legend.justification = jus,
      legend.title = element_blank(),
      legend.background = element_blank(),
      text = element_text(size = 11),
      legend.key.size = grid::unit(0.66, "lines"),
      legend.spacing = grid::unit(-1.25, "line"),
      panel.grid = element_blank(),
      legend.key = element_blank(),
      legend.text.align = 0,
      plot.title = element_text(
        lineheight = 1.05,
        face = "bold",
        size = 14.3,
        hjust = 0.5)) + opt.theme
}

