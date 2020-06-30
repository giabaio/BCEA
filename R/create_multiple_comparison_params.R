
#
create_multiple_comparison_params <- function(he,
                                              graph_params) {
  
  # linetype is the indicator
  if (is.null(graph_params$plot$line$types))
    graph_params$plot$line$types = rep_len(1:6, he$n.comparisons)
  
  # adjust provided aes lengths
  if (length(graph_params$plot$line$types) < length(comparisons_label))
    graph_params$plot$line$types <- rep_len(graph_params$plot$line$types,
                                            length(comparisons_label))
  
  if (length(graph_params$plot$line$colors) < length(comparisons_label))
    graph_params$plot$line$colors <- rep_len(graph_params$plot$line$colors,
                                             length(comparisons_label))
  
  # labels for legend
  graph_params$plot$labels <- paste0(interventions[he$ref], " vs ",
                                     interventions[he$comp])
  
  graph_params
}
