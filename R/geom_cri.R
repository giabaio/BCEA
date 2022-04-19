
#' Credible interval ggplot geom
#' 
#' @param plot.cri Should we plot CrI? Logical
#' @param params Plot parameters including data
#' @keywords internal aplot
#' 
geom_cri <- function(plot.cri = TRUE,
                     params = NA) {
  if (plot.cri) {
    list(geom_line(data = params$data,
                   aes(y = .data$low,
                       group = .data$comp),
                   linetype = 2),
         geom_line(data = params$data,
                   aes(y = .data$upp,
                       group = .data$comp),
                   linetype = 2))
  } else list(NULL)
}

