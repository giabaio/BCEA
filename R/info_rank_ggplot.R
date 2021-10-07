
#' Info rank plot ggplot2 version
#' 
#' @export
#' 
info_rank_ggplot <- function(he, params) {
  
  ggplot(data = params$res,
         aes(x = reorder(.data$parameter, .data$info), y = .data$info)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_default() +
    scale_fill_manual(rep(c("red","blue"), nrow(params$res))) +
    ylab(params$xlab) +
    xlab("") +
    ggtitle(params$tit)
}

