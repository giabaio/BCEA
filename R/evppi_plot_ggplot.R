
#' @rdname evppi_plot_graph
#' @importFrom purrr pluck keep
#' @import dplyr ggplot2
#' 
#' @export
#'
evppi_plot_ggplot.evppi <- function(evppi_obj,
                                    pos_legend = c(0, 0.8),
                                    col = c(1,1),
                                    ...) {
  
  extra_args <- list(...)
  
  plot_dat <-
    evppi_obj[c("evi", "evppi", "k")] %>% 
    bind_rows() %>% 
    ##TODO: for >1 evppi
    melt(id.vars = "k") %>% 
    mutate(variable = as.character(.data$variable),
           variable = ifelse(.data$variable == "evppi",
                             paste("EVPPI for",
                                   evppi_obj$parameters),
                             "EVPI"))
  
  theme_add <- purrr::keep(extra_args, is.theme)
  size <- purrr::pluck(extra_args, "size", .default = c(1, 0.5))
  
  legend_params <- make_legend_ggplot(evppi_obj, pos_legend)
  
  ggplot(plot_dat,
         aes(x = .data$k, y = .data$value,
             group = .data$variable, size = .data$variable, colour = .data$variable)) +
    geom_line() +
    theme_default() +
    theme_add +
    scale_color_manual(values = col) +
    scale_size_manual(values = size) +
    do.call(theme, legend_params) +
    ggtitle("Expected Value of Perfect Partial Information") +
    xlab("Willingness to pay") +
    ylab("")
}


#' @rdname evppi_plot_graph
#' @export
#' 
evppi_plot_ggplot <- function(evppi_obj, ...) {
  UseMethod('evppi_plot_ggplot', evppi_obj)
}

