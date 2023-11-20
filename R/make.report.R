
#' Make Report
#' 
#' Constructs the automated report from the output of the BCEA.
#' 
#' @template args-he
#' @param evppi An object obtained as output to a call to `evppi`
#' (default is NULL, so not essential to producing the report).
#' @param ext A string of text to indicate the extension of the 
#' resulting output file. Possible options are `"pdf"`, `"docx"`.
#' This requires the use of pandoc, knitr and rmarkdown.
#' @param echo A string (default to `FALSE`) to instruct whether
#' the report should also include the `BCEA` commands used to 
#' produce the analyses. If the optional argument `echo` is set
#' to `TRUE` (default = `FALSE`), then the commands are also
#' printed.
#' @param ... Additional parameters. For example, the user can specify the
#' value of the willingness to pay `wtp`, which is used in some of
#' the resulting analyses (default at the break even point). 
#' Another additional parameter that the user can specify is the name 
#' of the file to which the report should be written. This can be done 
#' by simply passing the optional argument `filename="NAME"`. 
#' The user can also specify an object including the PSA simulations 
#' for all the relevant model parameters. If this is passed to the 
#' function (in the object `psa_sims`),
#' then `make.report` will automatically construct an "Info-rank
#' plot", which is a probabilistic form of tornado plot, based on the
#' Expected Value of Partial Information.  The user can also specify
#' the optional argument `show.tab` (default=FALSE); if set to
#' `TRUE`, then a table with the values of the Info-rank is also
#' shown.
#' 
#' @author Gianluca Baio
#' @seealso [bcea()]
#' @importFrom Rdpack reprompt
#' 
#' @references
#' 
#' \insertRef{Baio2011}{BCEA}
#' 
#' \insertRef{Baio2013}{BCEA}
#' 
#' @export
#' 
#' @examples
#' 
#' \dontrun{
#'   data(Vaccine, package = "BCEA")
#'   m <- bcea(eff, cost, ref = 2)
#'   make.report(m)
#' }
#' 
make.report <- function(he,
                        evppi = NULL,
                        ext = "pdf",
                        echo = FALSE,
                        ...) {
  
  # check if knitr installed (and if not, asks for it)
  if(!isTRUE(requireNamespace("knitr", quietly = TRUE))) {
    stop("You need to install the R package 'knitr'. Please run in your R terminal:\n install.packages('knitr')", call. = FALSE)
  }
  knitr::opts_knit$set(progress = FALSE, verbose = FALSE)
  
  # check if rmarkdown installed (and if not, asks for it)
  if(!isTRUE(requireNamespace("rmarkdown", quietly = TRUE))) {
    stop("You need to install the R package 'rmarkdown'. Please run in your R terminal:\n install.packages('rmarkdown')", call. = FALSE)
  }
  
  extra_args <- list(...)
  
  wtp <- 
    if (exists("wtp", extra_args)) {
      extra_args$wtp
    } else {
      he$k[min(which(he$k >= he$ICER))]
    }
  
  filename <- 
    if (exists("filename", extra_args)) {
      extra_args$filename
    } else {
      paste0("Report.", ext)}
  
  psa_params <- 
    if (exists("psa_sims", extra_args)) {
      extra_args$psa_sims
    } else {
      NULL}
  
  show.tab <-
    if (exists("show.tab", extra_args)) {
      TRUE
    } else {
      FALSE}
  
  rmd_params <-
    list(wtp = wtp,
         filename = filename,
         psa_params = psa_params,
         ext = ext,
         show.tab = show.tab)
  
  # remove all warnings
  withr::with_options(list(warn = -1), {
    
    # get current directory, move to relevant path, go back to current directory
    file <- file.path(tempdir(), filename)
    bcea_file_location <-
      normalizePath(
        file.path(system.file("rmarkdown","report", package = "BCEA"), "report.Rmd"))
    rmd_format <-
      switch(ext,
             pdf = rmarkdown::pdf_document(),
             docx = rmarkdown::word_document())
    out <-
      quiet(
        rmarkdown::render(bcea_file_location,
                          output_format = rmd_format,
                          params = rmd_params))
    
    file.copy(from = out, to = file, overwrite = TRUE)
    cat(paste0("The report is saved in the file ", file, "\n"))
  })
}

#' Allow disabling of the cat messages
#' @param x Object to quietly return
#' @keywords internal
#' 
quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 

#' Automatically open pdf output using default pdf viewer
#' 
#' @param file_name String file names for pdf
#' @keywords internal
#' 
openPDF <- function(file_name) {
  os <- .Platform$OS.type
  if (os == "windows")
    shell.exec(normalizePath(file_name))
  else {
    pdf <- getOption("pdfviewer", default = '')
    if (nchar(pdf) == 0)
      stop("The 'pdfviewer' option is not set. Use options(pdfviewer = ...)")
    system2(pdf, args = c(file_name))
  }
}

