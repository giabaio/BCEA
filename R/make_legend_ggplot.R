
#' @noRd
#' 
#' c(0,0) corresponds to the “bottom left”
#' c(1,1) corresponds to the “top right”
#' inside the plotting area
#'
make_legend_ggplot <- function(he, legend_pos) {
  
  legend_just <- NULL  # sets the corner that the legend_pos position refers to
  legend_dir <- "horizontal"
  
  n_lines <- 
    if (inherits(he, "pairwise")) {
      he$n_comparators
    } else {
      he$n_comparisons}
  
  if (n_lines == 1) {
    
    legend_pos <- "none"
    
  } else if (any(is.na(legend_pos))) {
    
    legend_pos <- "none"
    
  } else if (is.logical(legend_pos)) {
    
    if (legend_pos) {
      legend_pos <- "bottom"
      legend_dir <- "vertical"
    } else {
      legend_pos <- c(1, 0)
      legend_just <- legend_pos 
    }
  } else if (is.character(legend_pos)) {
    
    pos_choices <- c("left", "right", "bottom", "top")
    legend_pos <- choices[pmatch(legend_pos, pos_choices)]
    legend_just <- "center"
  } else if (is.numeric(legend_pos) & length(legend_pos) == 2) {
    
    legend_just <- legend_pos
  } else {
    # default
    legend_pos <- c(1, 0)
    legend_just <- legend_pos
  } 
  
  list(legend.direction = legend_dir,
       legend.justification = legend_just,
       legend.position = legend_pos)
}