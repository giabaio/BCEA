#' make.report
#' 
#' Constructs the automated report from the output of the BCEA 
#' 
#' @param he An object obtained as output to a call to \code{bcea}
#' @param evppi An object obtained as output to a call to \code{evppi}
#' (default is NULL, so not essential to producing the report).
#' @param ext A string of text to indicate the extension of the 
#' resulting output file. Possible options are \code{"pdf"}, \code{"docx"}.
#' This requires the use of pandoc, knitr and rmarkdown.
#' @param echo A string (default to \code{FALSE}) to instruct whether
#' the report should also include the \code{BCEA} commands used to 
#' produce the analyses. If the optional argument \code{echo} is set
#' to \code{TRUE} (default = \code{FALSE}), then the commands are also
#' printed.
#' @param ... Additional parameters. For example, the user can specify the
#' value of the willingness to pay \code{wtp}, which is used in some of
#' the resulting analyses (default at the break even point). 
#' Another additional parameter that the user can specify is the name 
#' of the file to which the report should be written. This can be done 
#' by simply passing the optional argument \code{filename="NAME"}. 
#' The user can also specify an object including the PSA simulations 
#' for all the relevant model parameters. If this is passed to the 
#' function (in the object \code{psa_sims}),
#' then \code{make.report} will automatically construct an "Info-rank
#' plot", which is a probabilistic form of tornado plot, based on the
#' Expcted Value of Partial Information.  The user can also specify
#' the optional argument \code{show.tab} (default=FALSE); if set to
#' \code{TRUE}, then a table with the values of the Info-rank is also
#' shown.
#' 
#' @author Gianluca Baio
#' @seealso \code{\link{bcea}}
#' @references Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity
#' Analysis in Health Economics.  Statistical Methods in Medical Research
#' doi:10.1177/0962280211419832.
#' 
#' Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall,
#' London
#' @keywords "Health economic evaluation", "Expected value of information"
#' @examples
#' \donttest{
#' data(Vaccine)
#' m=bcea(e,c,ref=2)
#' makeReport(m)
#' }
#' @export 

make.report=function(he,evppi=NULL,ext="pdf",echo=FALSE,...) {
  
  ## quiet --- allows to disable the cat messages
  quiet <- function(x) { 
    sink(tempfile()) 
    on.exit(sink()) 
    invisible(force(x)) 
  } 
  
  # This may be used to automatically open the pdf output using the default pdf viewer...
  openPDF <- function(f) {
     os <- .Platform$OS.type
     if (os=="windows")
        shell.exec(normalizePath(f))
     else {
        pdf <- getOption("pdfviewer", default='')
        if (nchar(pdf)==0)
           stop("The 'pdfviewer' option is not set. Use options(pdfviewer=...)")
        system2(pdf, args=c(f))
     }
  }
  
  # Additional arguments
  exArgs=list(...)
  if(exists("wtp",exArgs)){wtp=exArgs$wtp} else {wtp=he$k[min(which(he$k>=he$ICER))]}
  if(exists("filename",exArgs)){filename=exArgs$filename} else {filename=paste0("Report.",ext)}
  if(exists("psa_sims",exArgs)){psa_sims=exArgs$psa_sims} else {psa_sims=NULL}
  if(exists("show.tab",exArgs)){show.tab=TRUE} else {show.tab=FALSE}
  
  # Checks if knitr is installed (and if not, asks for it)
  if(!isTRUE(requireNamespace("knitr",quietly=TRUE))) {
    stop("You need to install the R package 'knitr'.Please run in your R terminal:\n install.packages('knitr')")
  }
  knitr::opts_knit$set(progress = FALSE, verbose = FALSE)
  # Checks if rmarkdown is installed (and if not, asks for it)
  if(!isTRUE(requireNamespace("rmarkdown",quietly=TRUE))) {
    stop("You need to install the R package 'rmarkdown'.Please run in your R terminal:\n install.packages('rmarkdown')")
  }
  
  # Removes all warnings
  options(warn=-1)
  # Get current directory, then move to relevant path, then go back to current directory
  file=file.path(tempdir(),filename)
  out = quiet(rmarkdown::render(normalizePath(file.path(system.file("Report",package="BCEA"),"report.Rmd")),
                                 switch(ext,pdf=rmarkdown::pdf_document(),
                                        docx=rmarkdown::word_document())
  ))
  file.copy(out, file, overwrite=TRUE)
  cat(paste0("The report is saved in the file ",file,"\n"))
}
