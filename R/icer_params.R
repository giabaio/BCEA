
#
icer_params <- function(graph_params,
                        he,
                        comparison) {
  
  list(icer_text =
         list(labels = icer_label(he, comparison),
              cex = 0.95,
              pos = 2,
              col = "red",
              x = graph_params$xlim[2],
              y = graph_params$ylim[2]),
       icer_points =
         list(pch = 20,
              col = "red",
              cex = 1))
}


#
icer_label <- function(he, comparison) {
  
  if (length(comparison) == 1) {
    return(
      paste0("\U2022",
             " ICER = ",
             format(he$ICER[comparison],
                    digits = 6,
                    nsmall = 2)))
  } 
  return("")
}

