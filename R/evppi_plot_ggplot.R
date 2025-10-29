
#' @rdname evppi_plot_graph
#' 
#' @param evppi_obj Object of class evppi
#' @param pos_legend Position of legend 
#' @param col Colour
#' @param ... Additional arguments
#' 
#' @importFrom purrr pluck keep
#' @import dplyr ggplot2
#' 
evppi_plot_ggplot <- function(evppi_obj,
                              pos_legend = c(0, 0.8),
                              col = c(1, 1),
                              ...) {
  
  extra_args <- list(...)
  
  # Combine evi and evppi into a tidy format
  plot_dat <- evppi_obj[c("evi", "evppi", "k")] %>%
    bind_rows() %>%
    pivot_longer(
      cols = -"k",
      names_to = "variable",
      values_to = "value"
    ) %>%
    mutate(
      variable = as.character(.data$variable),
      variable = ifelse(.data$variable == "evppi",
                        paste("EVPPI for", evppi_obj$parameters),
                        "EVPI")
    )
  
  # Extract optional ggplot2 themes from extra arguments
  theme_add <- Filter(ggplot2::is_theme, extra_args)
  
  # Extract size vector or use default
  size <- purrr::pluck(extra_args, "size", .default = c(1, 0.5))
  
  # Prepare legend position
  legend_params <- make_legend_ggplot(evppi_obj, pos_legend)
  
  # Build the ggplot
  ggplot(plot_dat,
         aes(x = .data$k, y = .data$value,
             group = .data$variable,
             size = .data$variable,
             color = .data$variable)) +
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
