
#
ceplane_legend_manual <- function(he, plot_params) {
  isfirst <- plot_params$ref_first
  
  list(
    scale_color_manual(
      labels =
        line_labels.default(he, ref_first = isfirst),
      values = plot_params$point$color),
    scale_shape_manual(
      labels =
        line_labels.default(he, ref_first = isfirst),
      values = plot_params$point$shape))
}
