
#' Add Contour Quadrants
#'
#' @template args-he
#' @param params List
#'
#' @return Plot side effect
#' @keywords internal aplot
#'
add_contour_quadrants <- function(he, params) {
  
  if (length(he$comp) > 1) return()
  
  pm <- params$quadrants
  
  text(x = pm$offset * pm$M.e,
       y = pm$offset * pm$M.c,
       adj = pm$adj[[1]],
       parse(text = pm$t1),
       cex = pm$cex)
  
  text(pm$offset * pm$m.e,
       pm$offset * pm$M.c,
       adj = pm$adj[[2]],
       parse(text = pm$t2),
       cex = pm$cex)
  
  text(pm$offset * pm$m.e,
       pm$offset * pm$m.c,
       adj = pm$adj[[3]],
       parse(text = pm$t3),
       cex = pm$cex)
  
  text(pm$offset * pm$M.e,
       pm$offset * pm$m.c,
       adj = pm$adj[[4]],
       parse(text = pm$t4),
       cex = pm$cex)
}


#' Geom Quadrant Text
#' 
#' @template args-he
#' @param graph_params Plot parameters; list
#' @keywords internal aplot
#' 
geom_quad_txt <- function(he, graph_params) {
  
  if (length(he$comp) > 1) return(NULL)
  
  geom_text(data = graph_params$quad_txt,
            aes(x = .data$x,
                y = .data$y,
                hjust = .data$hjust,
                label = .data$label),
            parse = TRUE,
            size = rel(3.5),
            inherit.aes = FALSE)
}

