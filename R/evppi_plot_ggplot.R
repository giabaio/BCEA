
#' evppi_plot_ggplot
#'
#' @param he 
#' @param pos_legend 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
#' 
evppi_plot_ggplot <- function(he,
                              pos_legend = pos,
                              # graph_params,
                              ...) {
  
  plot_dat <-
    he[c("evi", "evppi", "k")] %>% 
    bind_rows() %>% 
    melt(id.vars = "k") %>% 
    mutate(variable = as.character(variable),
           variable = ifelse(variable == "evppi",
                             paste("EVPPI for", he$params),
                             "EVPI"))
  
  ggplot(plot_dat,
         aes(x = k, y = value, group = variable, size = variable)) +
    geom_line() +
    theme_default() +
    scale_size_manual(values = c(1, 0.5)) +
    ggtitle("Expected Value of Perfect Partial Information") +
    xlab("Willingness to pay") +
    ylab("")
}

