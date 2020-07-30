
#' ceplane_plot_base
#'
#' @template args-he
#' @param comparison 
#' @param wtp 
#' @param graph_params 
#' @param ... 
#' 
#' @return 
#' @keywords hplot
#' 
#' @examples
#'
#' ## single comparator
#' data(Vaccine)
#' 
#' he <- bcea(e,c)
#' ceplane_plot_base(he)
#' 
#' ## multiple comparators
#' data(Smoking)
#' 
#' m <- bcea(e, c, ref = 4, Kmax = 500, interventions = treats)
#' ceplane_plot_base(m, wtp = 200)
#' 
ceplane_plot_base <- function(he,
                              comparison = NULL,
                              wtp = 25000,
                              graph_params,
                              ...) {
  
  # Encodes characters so that the graph can be saved as ps or pdf
  ps.options(encoding = "CP1250")
  pdf.options(encoding = "CP1250")
  
  if (he$n_comparisons == 1) {
    
    axes_params <- axes_params(he, wtp)
    base_params <- ceplane_base_params(he, graph_params)
    
    add_ceplane_setup()
    add_ceplane_polygon()
    add_ceplane_points()
    abline(h = 0, v = 0, col = "dark grey")
    add_ceplane_icer()
    add_ceplane_k_txt()

    
  } else if (he$n_comparisons > 1 & is.null(comparison)) {
    if (is.null(xlim)) {xlim <- range(he$delta_e)}
    if (is.null(ylim)) {ylim <- range(he$delta_c)}
    
    plot(he$delta_e[, 1],
         he$delta_c[, 1],
         pch = 20,
         cex = ifelse(
           !plot_params$exist$point$sizes,
           yes = 0.35,
           no = plot_params$point$sizes[1]),
         col = plot_params$point$colors[1],
         xlim = xlim,
         ylim = ylim,
         xlab = plot_annotations$xlab,
         ylab = plot_annotations$ylab,
         main = plot_annotations$title)
    
    for (i in 2:he$n_comparisons) {
      points(he$delta_e[, i],
             he$delta_c[, i],
             pch = 20,
             cex = ifelse(!plot_params$exist$point$sizes,
                          yes = 0.35,
                          no = plot_params$point$sizes[i]),
             col = plot_params$point$colors[i])
    }
    
    abline(h = 0, col = "dark grey")
    abline(v = 0, col = "dark grey")
    text <- paste(he$interventions[he$ref]," vs ",he$interventions[he$comp])
    legend(alt.legend,
           text,
           col = plot_params$point$colors,
           cex = 0.7,
           bty = "n",
           lty = 1)
    
  } else if (he$n_comparisons > 1 & !is.null(comparison) & length(comparison) == 1) {
    lower_e <- range(he$delta_e[, comparison])[1]
    upper_e <- range(he$delta_e[, comparison])[2]
    lower_c <- range(he$delta_c[, comparison])[1]
    upper_c <- range(he$delta_c[, comparison])[2]
    step_size <- (upper_e - lower_e)/10
    lower_e <- ifelse(lower_e < 0,
                      yes = lower_e,
                      no = -lower_e)
    lower_c <- ifelse(lower_c < 0,
                      yes = lower_c,
                      no = -lower_c)
    x_k <- 0.95*lower_e
    y_k <- ifelse(x_k*wtp < lower_c,
                  yes = lower_c,
                  no = x_k*wtp)
    x_seq <- seq(from = 100*lower_c/wtp,
                 to = 100*upper_c/wtp,
                 by = step_size)
    y_seq <- x_seq*wtp
    x_seq[1] <- ifelse(min(x_seq) < lower_e,
                       x_seq[1],
                       2*lower_e)
    y_seq[1] <- ifelse(min(y_seq) < lower_c,
                       y_seq[1],
                       2*lower_c)
    x_seq[length(x_seq)] <- ifelse(x_seq[length(x_seq)] < upper_e,
                                   yes = 1.5*upper_e,
                                   no = x_seq[length(x_seq)])
    if (!is.null(xlim)) {
      lower_e <- xlim[1]
      upper_e <- xlim[2]
    }
    if (!is.null(ylim)) {
      lower_c <- ylim[1]
      upper_c <- ylim[2]
    }
    
    plot(NULL,
         axes = FALSE,
         xlim = c(lower_e, upper_e),
         ylim = c(lower_c, upper_c),
         xlab = plot_annotations$xlab,
         ylab = plot_annotations$ylab,
         main = plot_annotations$title)
    
    # Note: default area colour light gray is too dark for base graphics
    polygon(
      c(min(x_seq), seq(min(x_seq), max(x_seq), step_size), max(x_seq)),
      c(min(y_seq), wtp * seq(min(x_seq), max(x_seq), step_size), min(y_seq)),
      col = ifelse(is.null(plot_params$area$color),
                   "grey95",
                   plot_params$area$color),
      border = ifelse(!plot_params$exist$area$line_color,
                      "black",
                      plot_params$area$line_color))
    axis(1)
    axis(2)
    box()
    points(he$delta_e[,comparison],
           he$delta_c[,comparison],
           pch = 20,
           cex = ifelse(!plot_params$exist$point$sizes,
                        yes = 0.35,
                        no = plot_params$point$sizes[1]),
           col = plot_params$point$colors[1])
    abline(h = 0, col = "dark grey")
    abline(v = 0, col = "dark grey")
    text(
      upper_e,
      upper_c,
      paste("\U2022"," ICER=",
            format(he$ICER[comparison], digits = 6, nsmall = 2), sep = ""),
      cex = 0.95,
      pos = 2,
      col = ifelse(!plot_params$exist$ICER$colors,
                   "red",
                   plot_params$ICER$colors[1]))
    
    points(mean(he$delta_e[,comparison]),
           mean(he$delta_c[,comparison]),
           pch = 20,
           col = ifelse(
             !plot_params$exist$ICER$colors,
             "red",
             plot_params$ICER$colors[1]),
           cex = ifelse(
             !plot_params$exist$ICER$sizes,
             1,
             plot_params$ICER$sizes[1]))
    
    k_equals_txt <- paste("k==", format(wtp, digits = 3, nsmall = 2, scientific = FALSE), sep = "")
    text(x_k,
         y_k,
         parse(text = k_equals_txt),
         cex = 0.8,
         pos = 4)
    
  } else if (he$n_comparisons > 1 & !is.null(comparison) & length(comparison) != 1) {
    stopifnot(all(comparison %in% 1:he$n_comparisons))
    # adjusts bcea object for the correct number of dimensions and comparators
    he$comp <- he$comp[comparison]
    he$delta_e <- he$delta_e[, comparison]
    he$delta_c <- he$delta_c[, comparison]
    he$n_comparators = length(comparison) + 1
    he$n_comparisons = length(comparison)
    he$interventions = he$interventions[sort(c(he$ref, he$comp))]
    he$ICER = he$ICER[comparison]
    he$ib = he$ib[, , comparison]
    he$eib = he$eib[, comparison]
    he$U = he$U[, , sort(c(he$ref, comparison + 1))]
    he$ceac = he$ceac[, comparison]
    he$ref = rank(c(he$ref, he$comp))[1]
    he$comp = rank(c(he$ref, he$comp))[-1]
    he$mod <- TRUE
    
    return(ceplane.plot(he,
                        wtp = wtp,
                        pos = alt.legend,
                        graph = "base",
                        size = size,
                        ...))
  }
}
