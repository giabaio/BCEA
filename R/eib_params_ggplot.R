
#
eib_params_ggplot <- function(he,
                              graph_params,
                              cri_params,
                              ...) {
  
  graph_params <- helper_ggplot_params(he, graph_params)
  
  ##TODO: remove duplication with base
  ylim <-
    if (!cri_params$plot.cri) {
      range(c(he$eib))
    } else {
      range(c(he$eib),
            cri_params$data)
    }
  
  default_params <- 
    list(
      size = rel(3.5),
      text = list(
        size =
          if (is.rel(graph_params$text$size)) {
            11 * unclass(graph_params$text$size)  # theme_get()$text$size
          } else {
            graph_params$text$size
          }
      ),
      kstar = list(
        geom = "text",
        label = paste0("k* = ", format(he$kstar, digits = 6)),
        x = he$kstar,
        y = min(ylim),
        hjust = ifelse((max(he$k) - he$kstar)/max(he$k) > 1/6,
                       yes = -0.1,
                       no = 1.1),
        vjust = 1),
      cri = list(
        lwd = ifelse(!graph_params$plot.cri, 0.5, 0.75),
        show.legend = FALSE),
      currency = "")
  
  modifyList(default_params,
             graph_params)
}
