#' BCEAweb
#' 
#' Launches the web-app 
#' 
#' @param e A matrix containing the simulations for the effectiveness
#' variable (with number of simulation rows and number of interventions
#' columns). Defaults at NULL, which means the user has to load their 
#' own values using the web-interface
#' @param c A matrix containing the simulations for the cost
#' variable (with number of simulation rows and number of interventions
#' columns). Defaults at NULL, which means the user has to load their 
#' own values using the web-interface
#' @param parameters A matrix with the simulations for all the relevant
#' model parameters. Defaults at NULL, which means the user has to load 
#' their own values using the web-interface
#' @param ... Additional parameters. 
#' @author Gianluca Baio
#' @seealso \code{\link{bcea}}
#' @references Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity
#' Analysis in Health Economics.  Statistical Methods in Medical Research
#' doi:10.1177/0962280211419832.
#' 
#' Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall,
#' London
#' @keywords Health economic evaluation Expected value of information
#' @export 

BCEAweb <- function(e=NULL,c=NULL,parameters=NULL,...) {
  if(!isTRUE(requireNamespace("shiny",quietly=TRUE))) {
    stop("You need to install the R package 'shiny'. Please run in your R terminal:\n install.packages('shiny')")
  }

  exArgs=list(...)
  appDir <- system.file("shiny-app", "BCEAweb", package = "BCEA")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `BCEA`.", call. = FALSE)
  }
  
  # This makes the possible inputs available to the webapp!
  # First uses BCEA::CreateInputs to process the simulations for the model parameters
  #  (this means the user can pass a BUGS, JAGS, Stan, or xls object and BCEA will know what to do. Also eliminates need with further dependencies).
  if(!is.null(parameters)){parameters=CreateInputs(parameters)$mat} 
  # Then assigns the arguments to the global environment, so the webapp can access them, if they're not NULL
  assign("e",e,envir=globalenv()); assign("c",c,envir=globalenv()); assign("parameters",parameters,envir=globalenv())

  # Finally run the webapp
  shiny::runApp(appDir, display.mode = "normal", quiet=TRUE, launch.browser=TRUE,...)
}
