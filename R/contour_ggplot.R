
#' Contour plot ggplot2 version
#' @rdname contour_graph
#' 
#' @template args-he
#' @param params Plot parameters
#' @param scale Scale
#' @param nlevels Number of levels
#' @param levels levels
#' @param xlim x-axis limits
#' @param ylim y-axis limits
#' @param extra_args additional arguments
#' @param comparison Comparison interventions; default 1
#' 
#' @importFrom grid unit
#' @import ggplot2
#' 
contour_ggplot <- function(he, params, scale, nlevels, levels,
                           xlim, ylim, extra_args, comparison = 1) {
  
  xlab <- params$xlab
  ylab <- params$ylab
  title <- params$title
  alt.legend <- params$alt.legend
  
  if (!is.null(nlevels)) {
    nlevels <- round(nlevels)
    if (nlevels < 0)
      nlevels <- 10
    if (nlevels == 0)
      nlevels <- 1
  }
  
  if (he$n_comparisons == 1) {
    kd <- data.frame(e = unlist(he$delta_e),
                     c = unlist(he$delta_c))
    
    # for scale_x_continuous(oob=)
    do.nothing <- function(x, limits) x
    
    # plot limits
    range.e <- range(kd$e)
    range.c <- range(kd$c)
    range.e[1] <- ifelse(range.e[1] < 0, range.e[1], -range.e[1])
    range.c[1] <- ifelse(range.c[1] < 0, range.c[1], -range.c[1])
    
    # labels
    p.ne <- sum(he$delta_e > 0 & he$delta_c > 0) / he$n_sim
    p.ne <-
      paste0("Pr(Delta[e]>0, Delta[c]>0)==",
             format(p.ne, digits = 4, nsmall = 3))
    p.nw <- sum(he$delta_e <= 0 & he$delta_c > 0) / he$n_sim
    p.nw <-
      paste0("Pr(Delta[e]<=0, Delta[c]>0)==",
             format(p.nw, digits = 4, nsmall = 3))
    p.sw <- sum(he$delta_e <= 0 & he$delta_c <= 0) / he$n_sim
    p.sw <-
      paste0("Pr(Delta[e]<=0, Delta[c]<=0)==",
             format(p.sw, digits = 4, nsmall = 3))
    p.se <- sum(he$delta_e > 0 & he$delta_c <= 0) / he$n_sim
    p.se <-
      paste0("Pr(Delta[e]>0, Delta[c]<=0)==",
             format(p.se, digits = 4, nsmall = 3))
    
    labels.df <-
      data.frame(
        he = c(range.e[2], range.e[1], range.e[1], range.e[2]),
        y = c(rep(range.c[2], 2), rep(range.c[1], 2)),
        label = c(p.ne, p.nw, p.sw, p.se),
        hjust = as.factor(c(1, 0, 0, 1)))
    
    points.colour <- 
      if (nlevels == 1) {
        "black"
      } else {
        "grey"}
    
    ceplane <-
      ggplot(kd, aes(.data$e, .data$c)) +
      geom_hline(aes(yintercept = 0), colour = "grey") +
      geom_vline(aes(xintercept = 0), colour = "grey") +
      theme_bw() +
      geom_point(size = 1, color = points.colour) +
      scale_x_continuous(limits = range.e, oob = do.nothing) +
      scale_y_continuous(limits = range.c, oob = do.nothing)
    
    if (!is.null(scale) && requireNamespace("MASS", quietly = TRUE)) {
      density <-
        MASS::kde2d(as.matrix(he$delta_e),
                    as.matrix(he$delta_c),
                    n = 300,
                    h = c(sd(as.matrix(he$delta_e)) / scale,
                          sd(as.matrix(he$delta_c)) / scale))
      
      density <-
        data.frame(expand.grid(e = density$x,
                               c = density$y),
                   z = as.vector(density$z))
      ceplane <-
        ceplane +
        geom_contour(
          aes(z = .data$z),
          data = density,
          colour = "black",
          bins = nlevels)
    } else {
      ceplane <- ceplane + stat_density2d(color = "black")
    }
    
    ceplane <- ceplane +
      geom_text(
        data = labels.df,
        aes(
          x = .data$he,
          y = .data$y,
          hjust = .data$hjust,
          label = .data$label),
        parse = TRUE,
        size = rel(3.5))
  }
  if (he$n_comparisons > 1 & is.null(comparison)) {
    kd <- data.frame(
      delta_e = c(as.matrix(he$delta_e)),
      delta_c = c(as.matrix(he$delta_c)),
      comparison =
        as.factor(sort(
          rep(1:he$n_comparisons, dim(as.matrix(he$delta_e))[1])))
    )
    
    # vector of values for colour, take out white, get integer values
    colors.label <-
      paste0("gray",
             round(seq(0, 100,
                       length.out = (he$n_comparisons + 1))[-(he$n_comparisons + 1)]))
    comparisons.label <-
      paste0(he$interventions[he$ref], " vs ", he$interventions[he$comp])
    
    # plot limits
    range.e <- range(kd$delta_e)
    range.c <- range(kd$delta_c)
    range.e[1] <- ifelse(range.e[1] < 0, range.e[1], -range.e[1])
    range.c[1] <- ifelse(range.c[1] < 0, range.c[1], -range.c[1])
    
    ceplane <-
      ggplot(kd,
             aes(x = .data$delta_e, y = .data$delta_c,
                 col = .data$comparison)) +
      geom_hline(yintercept = 0, colour = "grey") +
      geom_vline(xintercept = 0, colour = "grey") +
      theme_bw() +
      geom_point(size = 1) +
      scale_color_manual(label = comparisons.label,
                         values = colors.label,
                         na.value = "black") +
      scale_x_continuous(limits = range.e, oob = do.nothing) +
      scale_y_continuous(limits = range.c, oob = do.nothing)
    
    if (!is.null(scale) && requireNamespace("MASS", quietly = TRUE)) {
      densitydf <- data.frame()
      for (i in seq_len(he$n.comparison)) {
        temp <-
          MASS::kde2d(
            as.matrix(he$delta_e)[, i],
            as.matrix(he$delta_c)[, i],
            n = 300,
            h = c(sd(as.matrix(he$delta_e)[, i]) / scale,
                  sd(as.matrix(he$delta_c)[, i]) / scale))
        temp <-
          data.frame(expand.grid(e = temp$x,
                                 c = temp$y),
                     z = as.vector(temp$z))
        densitydf <-
          rbind(densitydf, cbind(temp, rep(i, dim(temp)[[1]])))
      }
      names(densitydf) <- c("delta_e", "delta_c", "z", "comparison")
      densitydf$comparison <- as.factor(densitydf$comparison)
      ceplane <-
        ceplane +
        geom_contour(aes(z = .data$z, colour = .data$comparison),
                     data = densitydf,
                     bins = nlevels) +
        guides(colour = guide_legend(override.aes =
                                       list(linetype = 0)))
    } else {
      ceplane <-
        ceplane +
        stat_density2d() +
        guides(colour = guide_legend(override.aes =
                                       list(linetype = 0)))
    }
  }
  if (he$n_comparisons > 1 & !is.null(comparison)) {
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
    he$mod <- TRUE
    
    return(
      contour(
        he,
        scale = scale,
        pos = alt.legend,
        nlevels = nlevels,
        graph = "ggplot2",
        comparison = NULL)
    )
  }
  
  ceplane <-
    ceplane + labs(title = title, x = xlab, y = ylab)
  
  jus <- NULL
  
  if (isTRUE(alt.legend)) {
    alt.legend <- "bottom"
    ceplane <-
      ceplane + theme(legend.direction = "vertical")
  } else {
    if (is.character(alt.legend)) {
      choices <- c("left", "right", "bottom", "top")
      alt.legend <- choices[pmatch(alt.legend, choices)]
      jus <- "center"
      if (is.na(alt.legend)) alt.legend <- FALSE
    }
    if (length(alt.legend) > 1)
      jus <- alt.legend
    if (length(alt.legend) == 1 && !is.character(alt.legend)) {
      alt.legend <- c(1, 0)
      jus <- alt.legend
    }
  }
  
  ceplane +
    theme(
      legend.position = alt.legend,
      legend.justification = jus,
      legend.title = element_blank(),
      legend.background = element_blank(),
      text = element_text(size = 11),
      legend.key.size = grid::unit(.66, "lines"),
      legend.spacing = grid::unit(-1.25, "line"),
      panel.grid = element_blank(),
      legend.key = element_blank(),
      legend.text.align = 0,
      plot.title = element_text(
        lineheight = 1.05,
        face = "bold",
        size = 14.3,
        hjust = 0.5))
}

