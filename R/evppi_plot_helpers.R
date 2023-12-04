
#' @importFrom grDevices colors
#' 
evppi_legend_cols <- function(evppi_obj, col = NULL) {
  
  n_cols <- length(evppi_obj$parameters) + 1
  
  if (is.null(col)) {
    cols <- colors()
    gr <- floor(seq(from = 261, to = 336, length.out = n_cols))
    return(cols[gr])
    
  } else {
    if (length(col) != n_cols) {
      message(
        "The vector 'col' must have the number of elements for an EVPI
        colour and each of the EVPPI parameters. Forced to black\n")
      return(rep("black", length(evppi_obj$parameters) + 1))
    }
  }
  
  col
}


#'
evppi_legend_text <- function(evppi_obj) {
  
  cmd <- 
    if (nchar(evppi_obj$parameters[1]) <= 25) {
      paste0("EVPPI for ", evppi_obj$parameters)
    } else "EVPPI for the selected\nsubset of parameters"
  
  if (length(evppi_obj$index) > 1 &&
      (("Strong & Oakley (univariate)" %in% evppi_obj$method) || 
       ("Sadatsafavi et al" %in% evppi_obj$method))) {
    
    # label lines
    for (i in seq_along(evppi_obj$index)) {
      
      ##TODO:
      # text(x = par("usr")[2],
      #      y = evppi_obj$evppi[[i]][length(evppi_obj$k)],
      #      labels = paste0("(", i, ")"), cex = 0.7, pos = 2)
    }    

    cmd <-
      paste0("(",
             paste(seq_len(evppi_obj$index)),
             ") EVPPI for ",
             evppi_obj$parameters)
  }
  
  c("EVPI", cmd)
}

