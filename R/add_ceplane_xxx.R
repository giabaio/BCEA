
#
add_ceplane_setup <- function(plot_params) {
  
  do.call("plot",
          plot_params$setup,
          quote = TRUE)
  axis(1)
  axis(2)
}

#
add_ceplane_polygon <- function(plot_params) {
  
  do.call("polygon",
          plot_params$polygon,
          quote = TRUE)
  box()
}

#
add_ceplane_points <- function(he,
                               comparison,
                               plot_params) {
  do.call("matplot",
          c(list(x = as.matrix(he$delta_e)[, comparison],
                 y = as.matrix(he$delta_c)[, comparison],
                 add = TRUE),
            plot_params$points),
          quote = TRUE)
}

#
add_ceplane_icer <- function(comparison,
                             plot_params) {
  do.call("text",
          plot_params$icer_text,
          quote = TRUE)
  
  do.call("points",
          c(list(
            x = colMeans(
              as.matrix(he$delta_e)[, comparison, drop = FALSE]),
            y = colMeans(
              as.matrix(he$delta_c)[, comparison, drop = FALSE])),
            plot_params$icer_points),
          quote = TRUE)
}

#
add_ceplane_k_txt <- function(plot_params) {
  
  k_equals_txt <-
    paste0("k == ",
           format(
             plot_params$wtp,
             digits = 3,
             nsmall = 2,
             scientific = FALSE))
  
  do.call(text,
          c(list(labels =
                   parse(text = k_equals_txt)),
            plot_params$k_txt))
}

#
add_ceplane_legend <- function(comparison,
                               legend_params) {
  
  if (length(comparison) == 1) return()
  
  do.call(legend, legend_params)
}

