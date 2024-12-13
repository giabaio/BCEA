
#' BCEAweb
#' 
#' Launches the web-app.
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
#' their own values using the web-interface. Columns must be named
#' @param ... Additional parameters.
#' 
#' @author Gianluca Baio
#' @seealso \code{\link{bcea}}
#' @importFrom shiny runApp
#' @references
#' Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity
#' Analysis in Health Economics. Statistical Methods in Medical Research
#' doi:10.1177/0962280211419832.
#' 
#' Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall, London.
#' @keywords "Health economic evaluation" "Expected value of information"
#' 
#' @examples
#' \dontrun{
#' data(Vaccine)
#' BCEAweb(eff, cost, vaccine_mat)
#' 
#' data(Smoking)
#' colnames(pi_post) = paste0("p",1:ncol(pi_post))
#' BCEAweb(eff, cost, pi_post)
#' }
#' @export 
#' 
BCEAweb <- function(e = NULL,
                    c = NULL,
                    parameters = NULL,...) {
  exArgs <- list(...)
  appDir <- system.file("BCEAweb", package = "BCEA")
  
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `BCEA`.",
         call. = FALSE)
  }
  
  if (!exists("cairo", exArgs)) {
    if (require(Cairo))
      options(shiny.usecairo=TRUE)
  }
  
  ##TODO: how is this passed on?
  if (exists("launch.browser", exArgs)) {
    launch.browser = exArgs$launch.browser
  } else {
    launch.browser = TRUE}
  
  # This makes the possible inputs available to the webapp!
  # First uses BCEA::createInputs to process the simulations for the model
  # parameters (this means the user can pass a BUGS, JAGS, Stan, or xls
  # object and BCEA will know what to do.
  # Also eliminates need with further dependencies).
  if (!is.null(parameters)){parameters <- createInputs(parameters)$mat} 
  if (!is.null(e)){e <- as.matrix(e)}
  if (!is.null(c)){c <- as.matrix(c)}
  
  # run the webapp
  invisible(launch(e, c, parameters,...))
}


#' Internal launch function
#' 
#' @param e effects
#' @param c costs
#' @param parameters 
#' @param ... 
#' 
launch <- function(e, c, parameters,...) {
  
  if (!requireNamespace("shinythemes", quietly = TRUE)) {
    stop("You need to install the R package 'shinythemes'.
         Please run in your R terminal:\n install.packages('shinythemes')",
         call. = FALSE)
  }
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("You need to install the R package 'shiny'.
         Please run in your R terminal:\n install.packages('shiny')",
         call. = FALSE)
  }
  
  .bcea_env$.e <- e
  .bcea_env$.c <- c
  .bcea_env$.parameters <- parameters
  on.exit(.bcea_env$.e <- NULL, add = TRUE)
  on.exit(.bcea_env$.c <- NULL, add = TRUE)
  on.exit(.bcea_env$.parameters <- NULL, add = TRUE)
  
  shiny::runApp(system.file("BCEAweb", package = "BCEA"), 
                display.mode = "normal",
                # display.mode = "showcase",
                quiet = TRUE,
                launch.browser = TRUE, ...)
}

