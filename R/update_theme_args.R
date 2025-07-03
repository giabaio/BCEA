
#' Map theme element names to their constructors
#'
#' @noRd
#' @keywords internal
element_constructors <- function() {
  list(
    text = ggplot2::element_text,
    axis.text = ggplot2::element_text,
    axis.title = ggplot2::element_text,
    legend.title = ggplot2::element_text,
    legend.text = ggplot2::element_text,
    plot.title = ggplot2::element_text,
    strip.text = ggplot2::element_text,
    line = ggplot2::element_line,
    axis.line = ggplot2::element_line,
    rect = ggplot2::element_rect,
    panel.background = ggplot2::element_rect
    # Add more as needed
  )
}

#' Process only matching theme elements
#'
#' @noRd
#' @keywords internal
update_theme_args <- function(extra_args) {
  theme_args <- list()
  element_lup <- element_constructors()
  
  for (nm in names(extra_args)) {
    if (nm %in% names(element_lup)) {
      
      constructor <- element_lup[[nm]]
      
      theme_args[[nm]] <-
        process_element_arg(
          extra_args[[nm]], constructor)
    }
  }
  
  theme_args
}

#' Utility function to modify simply the `ggplot` `theme_BCEA()` theme
#' using inputs inside the call to `plot.bcea`
#'
#' @noRd
#' @keywords internal
#' 
process_element_arg <- function(arg, constructor) {
  
  # safe resolve_rel_size() helper
  resolve_rel_size <- function(size, base = 9) {
    if (inherits(size, "rel") && !is.null(attr(size, "val"))) {
      return(attr(size, "val") * base)
    } else if (is.numeric(size)) {
      return(size)
    } else {
      stop("Invalid size value: must be numeric or rel().")
    }
  }
  
  # an S3 object in ggplot2 of class element, rel, or margin
  if (inherits(arg, class(constructor()))) {
    return(arg)
  } else if (is.list(arg)) {
    if ("size" %in% names(arg)) {
      base_size <- 9  ##TODO:
      arg$size <- resolve_rel_size(arg$size, base = base_size)
    }
    return(do.call(constructor, arg))
  } else if (is.null(arg)) {
    return(NULL)
  } else {
    stop("Invalid theme element.")
  }
}
