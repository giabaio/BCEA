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

