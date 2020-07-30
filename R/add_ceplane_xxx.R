
#
add_ceplane_setup <- function(axes_params,
                              base_params) {
  do.call("plot",
          c(axes_params$limits,
            base_params$setup),
          quote = TRUE)
  axis(1)
  axis(2)
  box()
}

#
add_ceplane_polygon <- function(axes_params,
                                base_params) {
  do.call("polygon",
          c(axes_params$polygon,
            base_params$polygon),
          quote = TRUE)
}

#
add_ceplane_points <- function(he,
                               comparison,
                               base_params) {
  do.call("matplot",
          c(list(x = he$delta_e[, comparison],
                 y = he$delta_c[, comparison]),
            base_params$points),
          quote = TRUE)
}

#
add_ceplane_icer <- function(comparison,
                             axes_params,
                             base_params) {
  do.call("text",
          c(list(x = axes_params$xlim[2],
                 y = axes_params$ylim[2]),
            base_params$icer_text),
          quote = TRUE)
  
  do.call("points",
          c(list(x = colMeans(he$delta_e[, comparison, drop = FALSE]),
                 y = colMeans(he$delta_c[, comparison, drop = FALSE])),
            base_params$icer_points),
          quote = TRUE)
}

#
add_ceplane_k_txt <- function(axes_params,
                              wtp) {
  k_equals_txt <-
    paste0("k == ",
           format(
             wtp,
             digits = 3,
             nsmall = 2,
             scientific = FALSE))
  
  text(axes_params$k$x,
       axes_params$k$y,
       parse(text = k_equals_txt),
       cex = 0.8,
       pos = 4)
}

#
add_ceplane_legend <- function(he,
                               comparison,
                               base_params) {
  
  if (length(comparison) == 1) return()
  
  text <- paste(he$interventions[he$ref],
                "vs",
                he$interventions[comparison])
  legend("topright",
         text,
         col = base_params$point$colors,
         cex = 0.7,
         bty = "n",
         lty = 1)
}

