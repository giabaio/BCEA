
#
make_legend_base <- function(he,
                             pos_legend,
                             graph_params) {
  
  if (is.numeric(pos_legend) & length(pos_legend) == 2) {
    
    ns <- ifelse(pos_legend[2] == 1, "top", "bottom")
    ew <- ifelse(pos_legend[1] == 1, "left", "right")
    pos_legend <- paste0(ns, ew)
  }
  
  if (is.logical(pos_legend)) {
    if (!pos_legend)
      alt_legend = "bottomright"
    else
      alt_legend = "bottomleft"
  }
  
  text <- paste(he$interventions[he$ref], " vs ", he$interventions[he$comp])
  
  list(x = pos_legend,
       legend = text,
       cex = 0.7,
       bty = "n", 
       lty = graph_params$plot$line$types,
       col = graph_params$plot$line$colors)
}
