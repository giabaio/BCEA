
#
make_legend <- function(pos_legend) {
  
  jus <- NULL
  legend_direction <- "horizontal"
  
  if (pos_legend) {
    pos_legend <- "bottom"
    legend_direction <- "vertical"
  } else {
    if (is.character(pos_legend)) {
      choices <- c("left", "right", "bottom", "top")
      pos_legend <- choices[pmatch(pos_legend, choices)]
      jus <- "center"
      
      if (is.na(pos_legend)) pos_legend <- FALSE
    }
    
    if (length(pos_legend) > 1)
      jus <- pos_legend
    
    if (length(pos_legend) == 1 & !is.character(pos_legend)) {
      pos_legend <- c(1, 0)
      jus <- pos_legend }
  }
  
  list(legend.direction = legend_direction,
       legend.justification = jus,
       legend.position = pos_legend)
}