
#
add_ceplane_setup <- function(axes_params,
                              base_params) {
  do.call("plot",
          c(axes_params$limits,
            base_params$setup),
          quote = TRUE)
  axis(1)
  axis(2)
}

#
add_ceplane_polygon <- function(axes_params,
                                base_params) {
  do.call("polygon",
          c(axes_params$polygon,
            base_params$polygon),
          quote = TRUE)
  box()
}

#
add_ceplane_points <- function(he,
                               comparison,
                               base_params) {
  do.call("matplot",
          c(list(x = as.matrix(he$delta_e)[, comparison],
                 y = as.matrix(he$delta_c)[, comparison],
                 add = TRUE),
            base_params$points),
          quote = TRUE)
}

#
add_ceplane_icer <- function(comparison,
                             axes_params,
                             base_params) {
  do.call("text",
          c(axes_params$icer_text,
            base_params$icer_text),
          quote = TRUE)
  
  do.call("points",
          c(list(
            x = colMeans(
              as.matrix(he$delta_e)[, comparison, drop = FALSE]),
            y = colMeans(
              as.matrix(he$delta_c)[, comparison, drop = FALSE])),
            base_params$icer_points),
          quote = TRUE)
}

#
add_ceplane_k_txt <- function(axes_params,
                              base_params) {
  k_equals_txt <-
    paste0("k == ",
           format(
             axes_params$wtp,
             digits = 3,
             nsmall = 2,
             scientific = FALSE))
  
  do.call(text, c(list(labels =
                         parse(text = k_equals_txt)),
                  axes_params$k_txt,
                  base_params$k_txt))
}

#
add_ceplane_legend <- function(comparison,
                               legend_params) {
  
  if (length(comparison) == 1) return()
  
  do.call(legend, legend_params)
}

