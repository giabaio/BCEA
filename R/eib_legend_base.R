
#'
eib_legend_base <- function(he, graph_params) {
  
  list(x = where_legend(he, graph_params$pos),
       legend = paste0(he$interventions[he$ref], " vs ",
                       he$interventions[he$comp]),
       cex = 0.7,
       bty = "n",
       lwd = graph_params$line$lwd,
       col = graph_params$line$color,
       lty = graph_params$line$type)
}

