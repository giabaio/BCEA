
#' @noRd
#' @title make_legend_ggplot
#' 
#' @description c(0, 0) corresponds to the “bottom left”
#'  c(1, 1) corresponds to the “top right”
#'  inside the plotting area
#' 
#' @param dat data
#' @param pos_legend Legend position
#'
#' @keywords internal
#' 
make_legend_ggplot <- function(dat, pos_legend) {
  legend_just <- NULL  # sets the corner that the legend_pos position refers to
  legend_dir <- NULL
  
  n_lines <- num_lines(dat)

  ##TODO: why would it be NULL?  
  # n_lines <- 0
  # if (!is.null(dat)) n_lines <- num_lines(dat)
  
  if (n_lines == 1) {
    
    legend_pos <- "none"
    
  } else if (any(is.na(pos_legend))) {
    
    legend_pos <- "none"
    
  } else if (is.logical(pos_legend)) {
    
    if (isTRUE(pos_legend)) {
      legend_pos <- "bottom"
    } else {
      legend_pos <- c(1, 0)
      legend_just <- legend_pos 
    }
  } else if (is.character(pos_legend) && pos_legend != "") {
    
    # verify string is specified correctly
    stopifnot(grepl("^(bottom|top)*(left|right)*$", pos_legend))
    pos_choices <- c("left", "right", "bottom", "top")
    dir_choices <- c("horizontal", "vertical")
    
    # set default
    legend_just <- "center"
    
    # if legend_pos is a combined string such as "topright" or "bottomleft"
    if (sapply(pos_choices, function(pattern) grepl(pattern, pos_legend)) |> sum() > 1) {
      pos <- numeric(2)
      pos[1] <- grepl(pos_choices[2], paste0(pos_legend, "$"))
      pos[2] <- grepl(pos_choices[4], paste0("^", pos_legend))
      legend_pos <- pos
      # also overwrite justification for proper alignment in the plot corners
      legend_just <- pos
    } else {
      legend_pos <- pos_choices[pmatch(pos_legend, pos_choices)]
    }
    
    # restrict direction to a single value, NA is caught later
    legend_dir <- dir_choices[sapply(dir_choices, function(t) grepl(t, pos_legend))][1]
    
    if (is.na(legend_dir)) {
      legend_dir <- NULL  # default
    }
  } else if (is.numeric(pos_legend) &&
             length(pos_legend) == 2) {
    legend_pos <- pos_legend
    legend_just <- pos_legend

  } else {
    # default
    legend_pos <- c(1, 0)
    legend_just <- legend_pos
  } 
  list(legend.direction = legend_dir,
       legend.justification = legend_just,
       legend.position = legend_pos)
}
