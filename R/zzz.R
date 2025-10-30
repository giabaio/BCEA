.bcea_env <- new.env(parent = emptyenv())

#' @title .onLoad
#' @inheritParams base .onLoad
#' @return invisible
#' @noRd
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.bcea <- list(
    scipen = 10)
  toset <- !(names(op.bcea) %in% names(op))
  if (any(toset)) options(op.bcea[toset])

  grDevices::ps.options(encoding = "CP1250")
  grDevices::pdf.options(encoding = "CP1250")
  
  Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_" = "TRUE")
  
  invisible()
}

#' @title .onAttach
#' @description prints out a friendly reminder message to the user
#' @inheritParams base .onAttach
#' @return NULL
#' @noRd
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("The BCEA version loaded is: ", utils::packageVersion("BCEA"), " (stable version)\nA development version is available at https://giabaio.r-universe.dev/BCEA")
}
