## BCEA-deprecated.R

#' @title Deprecated functions in package \pkg{BCEA}.
#' @description The functions listed below are deprecated and will be defunct in
#'   the near future. When possible, alternative functions with similar
#'   functionality are also mentioned. Help pages for deprecated functions are
#'   available at `help("<function>-deprecated")`.
#' @name BCEA-deprecated
#' @keywords internal
NULL

#' @rdname BCEA-deprecated
make.report <- function(...) {
  .Deprecated(package = "BCEA",
              msg = "'make.report()' is deprecated. Please use 'BCEAweb::make.report()' instead.")
  
  return(invisible(NULL))
}
