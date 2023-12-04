
#' @rdname sim_table
#' @export
#' 
sim_table <- function(he, ...) UseMethod("sim_table", he)


#' Table of Simulation Statistics for the Health Economic Model
#' 
#' Using the input in the form of MCMC simulations and after having run the
#' health economic model, produces a summary table of the simulations from the
#' cost-effectiveness analysis.
#' 
#' @template args-he
#' @param wtp The value of the willingness to pay threshold to be used in the
#'            summary table.
#' @param ... Additional arguments
#' 
#' @return Produces the following elements:
#'   \item{table}{A table with simulation statistics from the economic model}
#'   \item{names.cols}{A vector of labels to be associated with each column of the table}
#'   \item{wtp}{The selected value of the willingness to pay}
#'   \item{idx_wtp}{The index associated with the selected value of the willingness
#'   to pay threshold in the grid used to run the analysis}
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
#' @keywords print
#' @import dplyr
#' 
#' @examples 
#' # See Baio G., Dawid A.P. (2011) for a detailed description of the 
#' # Bayesian model and economic problem
#'
#' # Load the processed results of the MCMC simulation model
#' data(Vaccine)
#' 
#' # Runs the health economic evaluation using BCEA
#' m <- bcea(e=eff,                # defines the variables of 
#'           c=cost,               # effectiveness and cost
#'           ref=2,                # selects the 2nd row of (e, c) 
#'                                 # as containing the reference intervention
#'           interventions=treats, # defines the labels to be associated 
#'                                 # with each intervention
#'           Kmax=50000)           # maximum value possible for the willingness 
#'                                 # to pay threshold; implies that k is chosen 
#'                                 # in a grid from the interval (0, Kmax)
#'
#' # Now can save the simulation exercise in an object using sim_table()
#' sim_table(m,         # uses the results of the economic evaluation 
#'           wtp=25000) # selects the particular value for k
#'                
#' @export
#' @rdname sim_table
#' 
sim_table.bcea <- function(he,
                           wtp = 25000, ...) {
  
  wtp <- min(wtp, he$Kmax)
  
  if (!is.element(wtp, he$k)) {
    if (!is.na(he$step)) {
      # The user has selected a non-acceptable value for wtp,
      # but has not specified wtp in the call to bcea
      stop(
        sprintf("The willingness to pay parameter is defined in the interval [0- %f], with increments of %f \n",
                he$Kmax, he$step), call. = FALSE)
      
    } else { # The user has actually specified wtp as input in the call to bcea
      he_k <- paste(he$k, collapse = " ")
      stop(
        paste0("The willingness to pay parameter is defined as:\n[", he_k, "]
               \nPlease select a suitable value", collapse = " "), call. = FALSE)
    }
  }
  
  # specific willingness to pay
  table <-
    cbind.data.frame(
      U = U_filter_by(he, wtp),
      Ustar = Ustar_filter_by(he, wtp),
      ib = ib_filter_by(he, wtp),
      ol = ol_filter_by(he, wtp),
      vi = vi_filter_by(he, wtp))
  
  table <-
    bind_rows(table,
              summarise_all(table, mean))
  
  # # use intervention names
  # U_cols <- grepl(pattern = paste0("^U\\.", he$interventions, collapse = "|"), names(table))
  # U_names <- names(table)[U_cols]

  U_names <- paste0("U", seq_along(he$interventions))
    
  names.cols <-
    c(U_names,
       "U*",
      paste0("IB", he$ref, "_", he$comp),
      "OL",
      "VI")
  colnames(table) <- names.cols
  rownames(table) <- c(1:he$n_sim, "Average")
  
  list(
    Table = table,
    names.cols = names.cols,
    wtp = wtp,
    ind.table = which(he$k == wtp))
}


#' @export
#' 
sim_table.default <- function(he, ...) {
  
  stop("No method for this object. Run bcea().", call. = FALSE)
}

