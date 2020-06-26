
#
prepare_graph_args <- function() {
  
  plot_aes <- list(area = list(include = TRUE,
                               color = NULL),
                   line = list(colors = "black",
                               types = NULL))
  
  plot_aes_args <- c("area_include", "area_color", "line_colors", "line_types")
  
  plot_annot <- list(title = "Cost Effectiveness Acceptability Curve",
                     xlab = "Willingness to pay",
                     ylab = "Probability of cost effectiveness")
  
  if (length(extra_args) >= 1) {
    
    extra_args <- extra_args[c("title", "xlab", "ylab")]
    
    for (a in names(extra_args)) {
      plot_annot[[a]] <- extra_args[[a]]}
    
    # if existing, read and store graphical options
    for (a in plot_aes_args) {
      if (exists(a, where = extra_args)) {
        aes_cat <- strsplit(a, "_")[[1]][1]
        aes_name <- paste0(strsplit(a, "_")[[1]][-1], collapse = "_")
        plot_aes[[aes_cat]][[aes_name]] <- extra_args[[a]]
      }
    }
  }
  
  grpah_args
}