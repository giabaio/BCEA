.bcea_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.bcea <- list(
    scipen = 10)
  toset <- !(names(op.bcea) %in% names(op))
  if (any(toset)) options(op.bcea[toset])

  ps.options(encoding = "CP1250")
  pdf.options(encoding = "CP1250")
  
  Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_" = "TRUE")
  
  invisible()
}

#' @title .onAttach
#' @description prints out a friendly reminder message to the user
#' @inheritParams base .onAttach
#' @return NULL
#' @noRd
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("The BCEA version loaded is: ", utils::packageVersion("BCEA"))
}
