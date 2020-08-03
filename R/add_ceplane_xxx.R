
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
                               plot_params) {
  do.call("matplot",
          c(list(x = he$delta_e,
                 y = he$delta_c,
                 add = TRUE),
            plot_params$points),
          quote = TRUE)
}

#
add_ceplane_icer <- function(plot_params) {
  
  do.call("text",
          plot_params$icer_text,
          quote = TRUE)
  
  do.call("points",
          c(list(
            x = colMeans(he$delta_e),
            y = colMeans(he$delta_c)),
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
add_ceplane_legend <- function(legend_params) {
  
  do.call(legend, legend_params)
}

#
add_axes <- function() {
  
  abline(h = 0, v = 0, col = "dark grey")
}
