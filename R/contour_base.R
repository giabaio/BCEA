
#' Contour Cost-Effectiveness Plane
#'
#' Choice of base R, ggplot2
#' @name contour_graph
#' 
NULL


#' Contour plot base R version
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
#' @export
#' 
contour_base <- function(he, params, scale, nlevels, levels,
                         xlim, ylim, extra_args, comparison = 1) {
  
  xlab <- params$xlab
  ylab <- params$ylab
  title <- params$title
  
  if (he$n_comparisons == 1) {
    density <- MASS::kde2d(as.matrix(he$delta_e),
                           as.matrix(he$delta_c),
                           n = 300,
                           h = c(sd(as.matrix(he$delta_e)) / scale,
                                 sd(as.matrix(he$delta_c)) / scale))
    offset <- 1.0
    
    p.ne <- sum(he$delta_e > 0 & he$delta_c > 0) / he$n_sim
    p.nw <- sum(he$delta_e <= 0 & he$delta_c > 0) / he$n_sim
    p.sw <- sum(he$delta_e <= 0 & he$delta_c <= 0) / he$n_sim
    p.se <- sum(he$delta_e > 0 & he$delta_c <= 0) / he$n_sim
    
    m.c <- range(he$delta_c)[1]
    M.c <- range(he$delta_c)[2]
    m.e <- range(he$delta_e)[1]
    M.e <- range(he$delta_e)[2]
    
    ##TODO: this isn't how to use ifelse(); refactor
    # Changes the range so that the plot always shows the x and y axes
    # ch1 <- ifelse(m.e > 0,
    #               m.e <- -m.e,
    #               m.e <- m.e)
    # ch2 <- ifelse(M.e < 0, M.e <- -M.e, M.e <- M.e)
    # ch3 <- ifelse(m.c > 0, m.c <- -m.c, m.c <- m.c)
    # ch4 <- ifelse(M.c < 0, M.c <- -M.c, M.c <- M.c)
    
    # If the user has specified the range of the graph, use those values
    if (!is.null(xlim)) {
      m.e <- xlim[1]
      M.e <- xlim[2]
    }
    if (!is.null(ylim)) {
      m.c <- ylim[1]
      M.c <- ylim[2]
    }
    
    plot(
      as.matrix(he$delta_e),
      as.matrix(he$delta_c),
      pch = 20,
      cex = 0.3,
      col = "dark grey",
      xlab = xlab,
      ylab = ylab,
      main = title,
      xlim = c(m.e, M.e),
      ylim = c(m.c, M.c))
    abline(h = 0, v = 0, col = "dark grey")
    
    if (!any(is.na(density$z))) {
      if (!is.null(levels)) {
        # normalise the density and use levels in the contour
        density$z <-
          (density$z - min(density$z)) / (max(density$z) - min(density$z))
        
        graphics::contour(
          density$x,
          density$y,
          density$z,
          add = TRUE,
          levels = levels,
          drawlabels = TRUE)
      }
      if (is.null(levels)) {
        graphics::contour(
          density$x,
          density$y,
          density$z,
          add = TRUE,
          nlevels = nlevels,
          drawlabels = FALSE)
      }
    }
    
    t1 <-
      paste("Pr(Delta[e]>0, Delta[c]>0)==",
            format(p.ne, digits = 4, nsmall = 3),
            sep = "")
    
    text(offset * M.e,
         offset * M.c,
         parse(text = t1),
         cex = 0.8,
         pos = 2)
    
    t2 <-
      paste("Pr(Delta[e]<=0, Delta[c]>0)==",
            format(p.nw, digits = 4, nsmall = 3),
            sep = "")
    
    text(offset * m.e,
         offset * M.c,
         parse(text = t2),
         cex = 0.8,
         pos = 4)
    
    t3 <-
      paste("Pr(Delta[e]<=0, Delta[c]<=0)==",
            format(p.sw, digits = 4, nsmall = 3),
            sep = "")
    
    text(offset * m.e,
         offset * m.c,
         parse(text = t3),
         cex = 0.8,
         pos = 4)
    
    t4 <-
      paste("Pr(Delta[e]>0, Delta[c]<=0)==",
            format(p.se, digits = 4, nsmall = 3),
            sep = "")
    
    text(offset * M.e,
         offset * m.c,
         parse(text = t4),
         cex = 0.8,
         pos = 2)
  }
  
  if (he$n_comparisons > 1) {
    if (!exists("title", where = extra_args)) {
      title <-
        paste(
          "Cost effectiveness plane contour plot \n",
          he$interventions[he$ref],
          " vs ",
          he$interventions[he$comp[comparison]],
          sep = "")
    }
    else {
      title <- extra_args$title
    }
    
    density <-
      MASS::kde2d(as.matrix(he$delta_e[, comparison]),
                  as.matrix(he$delta_c[, comparison]),
                  n = 300,
                  h = c(sd(as.matrix(he$delta_e[, comparison])) / scale,
                        sd(as.matrix(he$delta_c[, comparison])) / scale))
    offset <- 1.0
    
    p.ne <-
      sum(he$delta_e[, comparison] > 0 &
            he$delta_c[, comparison] > 0) / he$n_sim
    p.nw <-
      sum(he$delta_e[, comparison] <= 0 &
            he$delta_c[, comparison] > 0) / he$n_sim
    p.sw <-
      sum(he$delta_e[, comparison] <= 0 &
            he$delta_c[, comparison] <= 0) / he$n_sim
    p.se <-
      sum(he$delta_e[, comparison] > 0 &
            he$delta_c[, comparison] <= 0) / he$n_sim
    
    m.c <- range(he$delta_c[, comparison])[1]
    M.c <- range(he$delta_c[, comparison])[2]
    m.e <- range(he$delta_e[, comparison])[1]
    M.e <- range(he$delta_e[, comparison])[2]
    
    ##TODO:
    # Changes the range so that the plot always shows the x and y axes
    # ch1 <- ifelse(m.e > 0, m.e <- -m.e, m.e <- m.e)
    # ch2 <- ifelse(M.e < 0, M.e <- -M.e, M.e <- M.e)
    # ch3 <- ifelse(m.c > 0, m.c <- -m.c, m.c <- m.c)
    # ch4 <- ifelse(M.c < 0, M.c <- -M.c, M.c <- M.c)
     
    # If the user has specified the range of the graph, use those values
    if (!is.null(xlim)) {
      m.e <- xlim[1]
      M.e <- xlim[2]
    }
    if (!is.null(ylim)) {
      m.c <- ylim[1]
      M.c <- ylim[2]
    }
    
    plot(
      as.matrix(he$delta_e[, comparison]),
      as.matrix(he$delta_c[, comparison]),
      pch = 20,
      cex = 0.3,
      col = "dark grey",
      xlab = xlab,
      ylab = ylab,
      main = title,
      xlim = c(m.e, M.e),
      ylim = c(m.c, M.c))
    
    abline(h = 0, v = 0, col = "dark grey")
    
    if (!any(is.na(density$z))) {
      graphics::contour(
        density$x,
        density$y,
        density$z,
        add = TRUE,
        drawlabels = TRUE)
      if (!is.null(levels)) {
        # Normalise the density and use levels in the contour
        density$z <-
          (density$z - min(density$z)) / (max(density$z) - min(density$z))
        graphics::contour(
          density$x,
          density$y,
          density$z,
          add = TRUE,
          levels = levels,
          drawlabels = TRUE
        )
      }
      if (is.null(levels)) {
        graphics::contour(
          density$x,
          density$y,
          density$z,
          add = TRUE,
          nlevels = nlevels,
          drawlabels = FALSE
        )
      }
    }
    
    t1 <-
      paste("Pr(Delta[e]>0, Delta[c]>0)==",
            format(p.ne, digits = 4, nsmall = 3),
            sep = "")
    text(offset * M.e,
         offset * M.c,
         parse(text = t1),
         cex = 0.8,
         pos = 2)
    t2 <-
      paste("Pr(Delta[e]<=0, Delta[c]>0)==",
            format(p.nw, digits = 4, nsmall = 3),
            sep = "")
    text(offset * m.e,
         offset * M.c,
         parse(text = t2),
         cex = 0.8,
         pos = 4)
    t3 <-
      paste("Pr(Delta[e]<=0, Delta[c]<=0)==",
            format(p.sw, digits = 4, nsmall = 3),
            sep = "")
    text(offset * m.e,
         offset * m.c,
         parse(text = t3),
         cex = 0.8,
         pos = 4)
    t4 <-
      paste("Pr(Delta[e]>0, Delta[c]<=0)==",
            format(p.se, digits = 4, nsmall = 3),
            sep = "")
    text(offset * M.e,
         offset * m.c,
         parse(text = t4),
         cex = 0.8,
         pos = 2)
  }
}

