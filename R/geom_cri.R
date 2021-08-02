
#
geom_cri <- function(plot.cri = TRUE,
                     params = NA) {
  if (plot.cri) {
    list(geom_line(data = params$data,
                   aes(y = .data$low,
                       group = comp),
                   linetype = 2),
         geom_line(data = params$data,
                   aes(y = .data$upp,
                       group = comp),
                   linetype = 2))
  } else list(NULL)
}

