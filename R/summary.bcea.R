
#' Summary Method for Objects of Class \code{bcea}
#' 
#' Produces a table printout with some summary results of the health economic
#' evaluation
#' 
#' @template args-he
#' @param wtp The value of the willingness to pay threshold to be used in the
#'   summary table.
#' @param ...  Additional arguments affecting the summary produced.
#' 
#' @return Prints a summary table with some information on the health economic
#' output and synthetic information on the economic measures (EIB, CEAC, EVPI).
#' @author Gianluca Baio
#' @seealso \code{\link{bcea}}
#' @references
#' Baio, G., Dawid, A. P. (2011). Probabilistic Sensitivity
#' Analysis in Health Economics.  Statistical Methods in Medical Research
#' doi:10.1177/0962280211419832.
#' 
#' Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall,
#' London.
#' @keywords Health economic evaluation
#' @import crayon
#' 
#' @export
#' 
#' @examples 
#' 
#' data(Vaccine)
#' 
#  he <- bcea(e,c, interventions = treats, ref = 2)
#' summary(he)
#' 
summary.bcea <- function(he,
                         wtp = 25000,...) {
  
  if (max(he$k) < wtp) {
    wtp <- max(he$k)
    message(
      cat(paste0(
        "NB: k (wtp) is defined in the interval [",
        min(he$k),
        " - ",
        wtp,
        "]\n")))
  }
  
  if (!is.element(wtp, he$k)) {
    if (!is.na(he$step)) {
      # The user has selected a non-acceptable value for wtp, but has not specified wtp in the call to bcea
      stop(
        paste0(
          "The willingness to pay parameter is defined in the interval [0-",
          he$Kmax,
          "], with increments of ",
          he$step,
          "\n"), call. = FALSE)
    } else {
      # The user has actually specified wtp as input in the call to bcea
      he_k <- paste(he$k, collapse = " ")
      stop(
        paste0("The willingness to pay parameter is defined as:\n[",
               he_k,
               "]\nPlease select a suitable value",
               collapse = " "), call. = FALSE)
    }
  }
  
  ##TODO: why doesnt bcea dispatch work??
  Table <- sim_table.bcea(he)$Table
  
  EU_tab <- matrix(NA, he$n_comparators, 1)
  EU_tab[, 1] <-
    unlist(Table[he$n_sim + 1, paste0("U", 1:he$n_comparators)])
  colnames(EU_tab) <- "Expected utility"
  rownames(EU_tab) <- he$interventions
  
  comp_tab <- matrix(NA, he$n_comparisons, 3)
  comp_tab[, 1] <-
    Table[he$n_sim + 1, paste0("IB", he$ref, "_", he$comp)]
  if (he$n_comparisons == 1) {
    comp_tab[, 2] <-
      sum(Table[1:he$n_sim, paste0("IB", he$ref, "_", he$comp)] > 0) / he$n_sim
    comp_tab[, 3] <- he$ICER
  }
  if (he$n_comparisons > 1) {
    for (i in 1:he$n_comparisons) {
      comp_tab[i, 2] <-
        sum(Table[1:he$n_sim, paste0("IB", he$ref, "_", he$comp[i])] > 0) / he$n_sim
      comp_tab[i, 3] <- he$ICER[i]
    }
  }
  colnames(comp_tab) <- c("EIB", "CEAC", "ICER")
  rownames(comp_tab) <-
    paste0(he$interventions[he$ref], " vs ", he$interventions[he$comp])
  
  evpi_tab <- matrix(NA, 1, 1)
  evpi_tab[, 1] <- Table[he$n_sim + 1, "VI"]
  rownames(evpi_tab) <- "EVPI"                  #TODO: this is different value to book??
  colnames(evpi_tab) <- ""
  
  ## prints the summary table
  cat("\n")
  cat("Cost-effectiveness analysis summary \n")
  cat("\n")
  cat(paste0("Reference intervention:  ", he$interventions[he$ref], "\n"))
  # cat(paste0("Reference intervention:  ", green(he$interventions[he$ref]), "\n"))
  
  if (he$n_comparisons == 1) {
    cat(
      paste0("Comparator intervention: ",
             he$interventions[he$comp],
             # green(he$interventions[he$comp]),
             "\n"))
  }
  
  if (he$n_comparisons > 1) {
    cat(
      paste0("Comparator intervention(s): ",
             he$interventions[he$comp[1]],
             "\n"))
    for (i in 2:he$n_comparisons) {
      cat(paste0("                          : ", he$interventions[he$comp[i]], "\n"))
      # cat(paste0("                          : ", green(he$interventions[he$comp[i]]), "\n"))
    }
  }
  cat("\n")
  if (length(he$kstar) == 0 & !is.na(he$step)) {
    cat(
      paste0(
        he$interventions[he$best[1]],
        " dominates for all k in [",
        min(he$k),
        " - ",
        max(he$k),
        "] \n"))
  }
  if (length(he$kstar) == 1 & !is.na(he$step)) {
    kstar <- he$k[which(diff(he$best) == 1) + 1]
    cat(
      paste0(
        "Optimal decision: choose ",
        he$interventions[he$best[1]],
        " for k < ",
        kstar,
        " and ",
        he$interventions[he$best[he$k == kstar]],
        " for k >= ",
        kstar,
        "\n"))
  }
  if (length(he$kstar) > 1 & !is.na(he$step)) {
    cat(
      paste0(
        "Optimal decision: choose ",
        he$interventions[he$best[he$k == he$kstar[1] - he$step]],
        " for k < ",
        he$kstar[1],
        "\n"))
    for (i in 2:length(he$kstar)) {
      cat(paste0(
        "                         ",
        he$interventions[he$best[he$k == he$kstar[i] - he$step]],
        " for ",
        he$kstar[i - 1],
        " <= k < ",
        he$kstar[i],
        "\n"))
    }
    cat(paste0(
      "                         ",
      he$interventions[he$best[he$k == he$kstar[length(he$kstar)]]],
      " for k >= ",
      he$kstar[length(he$kstar)],
      "\n"))
  }
  cat("\n\n")
  cat(paste0("Analysis for willingness to pay parameter k = ", wtp, "\n"))
  cat("\n")
  print(EU_tab,
        quote = FALSE,
        digits = 5,
        justify = "center")
  cat("\n")
  print(comp_tab,
        quote = FALSE,
        digits = 5,
        justify = "center")
  cat("\n")
  cat(
    paste0(
      "Optimal intervention (max expected utility) for k = ",
      wtp,
      ": ",
      he$interventions[he$best][he$k == wtp],
      "\n"))
  print(evpi_tab,
        quote = FALSE,
        digits = 5,
        justify = "center")
}

