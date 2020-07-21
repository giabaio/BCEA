
##TODO:
#
prepare_graph_params_ceplane <- function() {
  
  # evaluate additional arguments -----
  plot_annotations <- list("exist" = list("title" = FALSE, "xlab" = FALSE, "ylab" = FALSE))
  plot_aes <- list("area" = list("include" = TRUE, "color" = "light gray", "line_color" = "black"),
                   "point" = list("colors" = "black", "sizes" = 4),
                   "ICER" = list("colors" = "red", "sizes" = 8),
                   "exist" = list("area" = list("include" = FALSE, "color" = FALSE, "line_color" = FALSE),
                                  "point" = list("colors" = FALSE, "sizes" = FALSE),
                                  "ICER" = list("colors" = FALSE, "sizes" = FALSE)))
  plot_aes_args = c("area_include", "area_color", "area_line_color",
                    "point_colors", "point_sizes",
                    "ICER_colors", "ICER_sizes")
  if (length(exArgs) >= 1) {
    # if existing, read and store title, xlab and ylab
    for (annotation in names(plot_annotations$exist)) {
      if (exists(annotation, where = exArgs)) {
        plot_annotations$exist[[annotation]] <- TRUE
        plot_annotations[[annotation]] <- exArgs[[annotation]]
      }
    }
    # if existing, read and store graphical options
    for (aes_arg in plot_aes_args) {
      if (exists(aes_arg, where = exArgs)) {
        aes_cat <- strsplit(aes_arg, "_")[[1]][1]
        aes_name <- paste0(strsplit(aes_arg, "_")[[1]][-1], collapse = "_")
        plot_aes[[aes_cat]][[aes_name]] <- exArgs[[aes_arg]]
        plot_aes$exist[[aes_cat]][[aes_name]] <- TRUE
      }
    }
  }
  # Args compatibility
  if (exists("ICER.size", where = exArgs)) {
    if (plot_aes$exist$ICER$sizes) {
      warning("Both ICER.size and ICER_sizes arguments specified. ICER_sizes will be used.")
    } else {
      warning("ICER.size is softly deprecated. Please use ICER_sizes instead.")
      plot_aes$exist$ICER$sizes <- TRUE
      plot_aes$ICER$sizes <- exArgs$ICER.size
    }
  }
  if (exists("ICER.col", where = exArgs)) {
    if (plot_aes$exist$ICER$colors) {
      warning("Both ICER.col and ICER_col arguments specified. ICER_col will be used.")
    } else {
      warning("ICER.col is softly deprecated. Please use ICER_col instead.")
      plot_aes$exist$ICER$colors <- TRUE
      plot_aes$ICER$colors <- exArgs$ICER.col
    }
  }
  if (exists("col", where = exArgs)) {
    if (plot_aes$exist$point$colors) {
      warning("Both col and point_colors arguments specified. point_colors will be used.")
    } else {
      warning("col argument is softly deprecated. Please use point_colors instead.")
      plot_aes$exist$point$colors <- TRUE
      plot_aes$point$colors <- exArgs$col
    }
  }
  # set default colour scheme
  if (!plot_aes$exist$point$colors) {
    if (he$n.comparisons > 1 & (is.null(comparison) || length(comparison) > 1)) {
      plot_aes$point$colors <- colors()[floor(seq(262, 340, length.out = he$n.comparisons))]
    } else {
      plot_aes$point$colors <- "grey55"
    }
  }
  # default plot annotations -----
  if (!plot_annotations$exist$title)
    plot_annotations$title <- with(he, paste0(
      "Cost-Effectiveness Plane",
      ifelse(
        n.comparisons == 1 | (n.comparisons > 1 & (!is.null(comparison) && length(comparison) == 1)),
        paste0("\n", interventions[ref], " vs ", interventions[-ref]),
        paste0(ifelse(
          isTRUE(he$mod),
          paste0(
            "\n",
            interventions[ref],
            " vs ",
            paste0(interventions[comp], collapse = ", ")
          ),
          ""
        ))
      )
    ))
  if (!plot_annotations$exist$xlab)
    plot_annotations$xlab = "Effectiveness differential"
  if (!plot_annotations$exist$ylab)
    plot_annotations$ylab = "Cost differential"
}
