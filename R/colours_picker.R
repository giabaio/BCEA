
#
colours_picker <- function(...) {
  
  # Allows to specify colours for the plots
  # If the user doesn't specify anything, use defaults
  if (!exists("color", exArgs)) {
    color <- rep(1, he$n_comparators + 1)
    lwd <- 1
  
    if (he$n_comparators > 7) {
      cl <- colors()
      color <- cl[floor(seq(262, 340, length.out = he$n_comparators))]	# grey scale
      lwd <- 1.5
    } 
  }
  
  # If the user specify colours, then use but check they are the right number
  if (exists("color", exArgs)) {
    color <- exArgs$color
    lwd <- 1
    
    if (he$n_comparators > 7) lwd <- 1.5
  
    if (length(color) != he$n_comparators) {
      message(paste("You need to specify",
                     he$n_comparators,
                     "colours. Falling back to default\n"))
    }
  }
  
  colour_params
}

