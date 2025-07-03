# themes for ggplot functions ---------------------------------------------

#' bcea theme ggplot2
#' @name theme_bcea
#' @keywords internal aplot
#' 
theme_default <- function() {
  
  theme_bw() %+replace%
    theme(legend.title = element_blank(),
          legend.background = element_blank(),
          text = element_text(size = 11),
          legend.key.size = grid::unit(0.66, "lines"),
          legend.spacing = grid::unit(-1.25, "line"),
          panel.grid = element_blank(),
          legend.key = element_blank(),
          legend.text = element_text(hjust = 0),
          plot.title = element_text(
            lineheight = 1.05,
            face = "bold",
            size = 14.3,
            hjust = 0.5),
          complete = TRUE)
}

#' @rdname theme_bcea
#'
theme_ceac <- function() {
  theme_default()  
}

#' @rdname theme_bcea
#'
theme_ceplane <- function() {
  theme_default()
}

#' @rdname theme_bcea
#'
theme_eib <- function() {
  theme_default()
}

#' @rdname theme_bcea
#'
theme_contour <- function() {
  theme_default()
}
