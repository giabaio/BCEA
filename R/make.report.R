#' make.report
#' 
#' Constructs the automated report from the output of the BCEA 
#' 
#' @param he An object obtained as output to a call to \code{bcea}
#' @param evppi An object obtained as output to a call to \code{evppi}
#' (default is NULL, so not essential to producing the report).
#' @param ext A string of text to indicate the extension of the 
#' resulting output file. Possible options are \code{"pdf"}, \code{"docx"}
#' and \code{"html"}. This requires the use of pandoc, knitr and rmarkdown.
#' @param ... Additional parameters. 
#' @author Gianluca Baio
#' @seealso \code{\link{bcea}}
#' @references Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity
#' Analysis in Health Economics.  Statistical Methods in Medical Research
#' doi:10.1177/0962280211419832.
#' 
#' Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall,
#' London
#' @keywords Health economic evaluation, Expected value of information
#' @examples
#' \donttest{
#' data(Vaccine)
#' m=bcea(e,c,ref=2)
#' makeReport(m)
#' }
#' @export 

make.report=function(he,evppi=NULL,ext=c("pdf","docx","html")...) {
  
  ## quiet --- allows to disable the cat messages
  quiet <- function(x) { 
    sink(tempfile()) 
    on.exit(sink()) 
    invisible(force(x)) 
  } 
  
  exArgs=list(...)
  # Checks if knitr is installed (and if not, asks for it)
  if(!isTRUE(requireNamespace("knitr",quietly=TRUE))) {
    stop("You need to install the R package 'knitr'.Please run in your R terminal:\n install.packages('knitr')")
  }
  knitr::opts_knit$set(progress = FALSE, verbose = FALSE)
  # Checks if rmarkdown is installed (and if not, asks for it)
  if(!isTRUE(requireNamespace("rmarkdown",quietly=TRUE))) {
    stop("You need to install the R package 'rmarkdown'.Please run in your R terminal:\n install.packages('rmarkdown')")
  }
  
  # Get current directory, then move to relevant path, then go back to current directory
  curdir=getwd()
  tmpdir=system.file("Report",package="BCEA")
  setwd(tmpdir)
  file=normalizePath("report.Rmd")
  out <- quiet(rmarkdown::render('report.Rmd',
                                 switch(ext,pdf=rmarkdown::pdf_document(),
                                        docx=rmarkdown::word_document())
  ))
  file.copy(out, file)
  setwd(curdir)
}