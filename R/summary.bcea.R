
#' Summary method for hes in the class \code{bcea}
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
#' @export
#' 
#' @examples 
#' 
#' data(Vaccine)
#' 
#' he <- bcea(e,c)
#' summary(he)
#' 
summary.bcea <- function(he,
                         wtp = 25000,...) {
  
  if (max(he$k) < wtp) {
    wtp <- max(he$k)
    cat(paste(
      "NB: k (wtp) is defined in the interval [",
      min(he$k),
      " - ",
      wtp,
      "]\n",
      sep = ""
    ))
  }
  if (!is.element(wtp, he$k)) {
    if (!is.na(he$step)) {
      # The user has selected a non-acceptable value for wtp, but has not specified wtp in the call to bcea
      stop(
        paste(
          "The willingness to pay parameter is defined in the interval [0-",
          he$Kmax,
          "], with increments of ",
          he$step,
          "\n",
          sep = ""
        )
      )
    } else {
      # The user has actually specified wtp as input in the call to bcea
      tmp <- paste(he$k, collapse = " ")
      stop(
        paste0(
          "The willingness to pay parameter is defined as:\n[",
          tmp,
          "]\nPlease select a suitable value",
          collapse = " "
        )
      )
    }
  }
  ind.table <- which(he$k == wtp)
  cols.u <- 1:he$n_comparators
  cols.ustar <- max(cols.u) + 1
  cols.ib <- (cols.ustar + 1):(cols.ustar + he$n_comparisons)
  cols.ol <- max(cols.ib) + 1
  cols.vi <- cols.ol + 1
  n.cols <- cols.vi
  
  Table <- matrix(NA, (he$n.sim + 1), n.cols)
  Table[1:he$n.sim, cols.u] <- he$U[, ind.table, ]
  Table[1:he$n.sim, cols.ustar] <- he$Ustar[, ind.table]
  if (length(dim(he$ib)) == 2) {
    Table[1:he$n.sim, cols.ib] <- he$ib[ind.table, ]
  }
  if (length(dim(he$ib)) > 2) {
    Table[1:he$n.sim, cols.ib] <- he$ib[ind.table, , ]
  }
  Table[1:he$n.sim, cols.ol] <- he$ol[, ind.table]
  Table[1:he$n.sim, cols.vi] <- he$vi[, ind.table]
  if (length(dim(he$ib)) == 2) {
    Table[(he$n.sim + 1), ] <-
      c(
        apply(he$U[, ind.table, ], 2, mean),
        mean(he$Ustar[, ind.table]),
        mean(he$ib[ind.table, ]),
        mean(he$ol[, ind.table]),
        mean(he$vi[, ind.table])
      )
  }
  if (length(dim(he$ib)) > 2) {
    Table[(he$n.sim + 1), ] <-
      c(
        apply(he$U[, ind.table, ], 2, mean),
        mean(he$Ustar[, ind.table]),
        apply(he$ib[ind.table, , ], 2, mean),
        mean(he$ol[, ind.table]),
        mean(he$vi[, ind.table])
      )
  }
  
  names.cols <-
    c(
      paste("U", seq(1:he$n_comparators), sep = ""),
      "U*",
      paste("IB", he$ref, "_", he$comp, sep = ""),
      "OL",
      "VI"
    )
  colnames(Table) <- names.cols
  
  tab1 <- matrix(NA, he$n_comparators, 1)
  tab1[, 1] <-
    Table[he$n.sim + 1, (paste("U", seq(1:he$n_comparators), sep =
                                     ""))]
  colnames(tab1) <- "Expected utility"
  rownames(tab1) <- he$interventions
  
  tab2 <- matrix(NA, he$n_comparisons, 3)
  tab2[, 1] <-
    Table[he$n.sim + 1, paste("IB", he$ref, "_", he$comp, sep =
                                    "")]
  if (he$n_comparisons == 1) {
    tab2[, 2] <-
      sum(Table[1:he$n.sim, paste("IB", he$ref, "_", he$comp, sep =
                                        "")] > 0) / he$n.sim
    tab2[, 3] <- he$ICER
  }
  if (he$n_comparisons > 1) {
    for (i in 1:he$n_comparisons) {
      tab2[i, 2] <-
        sum(Table[1:he$n.sim, paste("IB", he$ref, "_", he$comp[i], sep =
                                          "")] > 0) / he$n.sim
      tab2[i, 3] <- he$ICER[i]
    }
  }
  colnames(tab2) <- c("EIB", "CEAC", "ICER")
  rownames(tab2) <-
    paste(he$interventions[he$ref], " vs ", he$interventions[he$comp], sep =
            "")
  
  tab3 <- matrix(NA, 1, 1)
  tab3[, 1] <- Table[he$n.sim + 1, "VI"]
  rownames(tab3) <- "EVPI"
  colnames(tab3) <- ""
  
  ## Prints the summary table
  cat("\n")
  cat("Cost-effectiveness analysis summary \n")
  cat("\n")
  cat(paste("Reference intervention:  ", he$interventions[he$ref], "\n", sep =
              ""))
  if (he$n_comparisons == 1) {
    text.temp <-
      paste("Comparator intervention: ",
            he$interventions[he$comp],
            "\n",
            sep = "")
    cat(text.temp)
  }
  
  if (he$n_comparisons > 1) {
    text.temp <-
      paste("Comparator intervention(s): ",
            he$interventions[he$comp[1]],
            "\n",
            sep = "")
    cat(text.temp)
    for (i in 2:he$n_comparisons) {
      cat(paste("                          : ", he$interventions[he$comp[i]], "\n", sep =
                  ""))
    }
  }
  cat("\n")
  if (length(he$kstar) == 0 & !is.na(he$step)) {
    cat(
      paste(
        he$interventions[he$best[1]],
        " dominates for all k in [",
        min(he$k),
        " - ",
        max(he$k),
        "] \n",
        sep = ""
      )
    )
  }
  if (length(he$kstar) == 1 & !is.na(he$step)) {
    cat(
      paste(
        "Optimal decision: choose ",
        he$interventions[he$best[he$k == he$kstar - he$step]],
        " for k<",
        he$kstar,
        " and ",
        he$interventions[he$best[he$k == he$kstar]],
        " for k>=",
        he$kstar,
        "\n",
        sep = ""
      )
    )
  }
  if (length(he$kstar) > 1 & !is.na(he$step)) {
    cat(
      paste(
        "Optimal decision: choose ",
        he$interventions[he$best[he$k == he$kstar[1] - he$step]],
        " for k < ",
        he$kstar[1],
        "\n",
        sep = ""
      )
    )
    for (i in 2:length(he$kstar)) {
      cat(
        paste(
          "                         ",
          he$interventions[he$best[he$k == he$kstar[i] - he$step]],
          " for ",
          he$kstar[i - 1],
          " <= k < ",
          he$kstar[i],
          "\n",
          sep = ""
        )
      )
    }
    cat(paste(
      "                         ",
      he$interventions[he$best[he$k == he$kstar[length(he$kstar)]]],
      " for k >= ",
      he$kstar[length(he$kstar)],
      "\n",
      sep = ""
    ))
  }
  cat("\n\n")
  cat(paste("Analysis for willingness to pay parameter k = ", wtp, "\n", sep =
              ""))
  cat("\n")
  print(tab1,
        quote = FALSE,
        digits = 5,
        justify = "center")
  cat("\n")
  print(tab2,
        quote = FALSE,
        digits = 5,
        justify = "center")
  cat("\n")
  cat(
    paste(
      "Optimal intervention (max expected utility) for k=",
      wtp,
      ": ",
      he$interventions[he$best][he$k == wtp],
      "\n",
      sep = ""
    )
  )
  print(tab3,
        quote = FALSE,
        digits = 5,
        justify = "center")
}


