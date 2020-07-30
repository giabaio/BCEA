
##TODO: finish, tidy

#' prep_ceplane_params
#'
#' @param he 
#' @param comparison 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
#' 
prep_ceplane_params <- function(he,
                                comparison,
                                ...) {
  graph_params <- list(...)
  
  ##TODO: back-compatibility helper..
  
  point_colors <- 
    if (he$n_comparisons > 1 &
        (is.null(comparison) || length(comparison) > 1)) {
      colors()[floor(seq(262, 340, length.out = he$n_comparisons))]
    } else {
      "grey55"}
  
  is_single_comp <-
    n_comparisons == 1 | (n_comparisons > 1 & (!is.null(comparison) && length(comparison) == 1))
  
  plot_title <-
    with(he, paste0(
      "Cost-Effectiveness Plane",
      ifelse(
        is_single_comp,
        yes = paste("\n", interventions[ref], "vs", interventions[-ref]),
        no = paste0(
          ifelse(isTRUE(he$mod),
                 yes = paste0("\n",
                              interventions[ref],
                              " vs ",
                              paste0(interventions[comp],
                                     collapse = ", ")),
                 no = "")))
    ))
  
  default_params <-
    list(xlab = "Effectiveness differential",
         ylab = "Cost differential",
         title = plot_title,
         xlim = NULL,
         ylim = NULL,
         point = list(
           colors = point_colors,
           size = 4),
         area = list(
           include = TRUE,
           color = "light gray",
           line_color = "black"),
         ICER = list(
           colors = "red",
           sizes = 8))
  
  modifyList(default_params, graph_params)
}
