
#' @importFrom utils methods
#' 
list_plot_methods <- function() {
  
  m <- methods(class = "bcea")
  m[grep(pattern = "plot", m)]
}

