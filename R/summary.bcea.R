
#' Summary Method for Objects of Class `bcea`
#' 
#' Produces a table printout with some summary results of the health economic
#' evaluation.
#' 
#' @param object A `bcea` object containing the results of the Bayesian
#'               modelling and the economic evaluation.
#' @param wtp The value of the willingness to pay threshold used in the summary table.
#' @param ...  Additional arguments affecting the summary produced.
#' 
#' @return Prints a summary table with some information on the health economic
#' output and synthetic information on the economic measures (EIB, CEAC, EVPI).
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
#' 
#' @export
#' 
#' @examples 
#' data(Vaccine)
#' 
#' he <- bcea(eff, cost, interventions = treats, ref = 2)
#' summary(he)
#' 
summary.bcea=function(object,wtp=25000,...) {
  he=object

  if (max(he$k)<wtp) {
    wtp=max(he$k)
    message("NB: k (wtp) is defined in the interval [",min(he$k)," - ",wtp,"]")
  }

  if (!wtp %in% he$k) {
    if (!is.na(he$step)) {
      stop("The willingness to pay parameter is defined in the interval [0-",he$Kmax,
           "], with increments of ",he$step,call.=FALSE)
    } else {
      stop("The willingness to pay parameter is defined as:\n[",paste(he$k,collapse=" "),
           "]\nPlease select a suitable value",call.=FALSE)
    }
  }

  idx=which(he$k==wtp)
  ints=sort(c(he$ref,he$comp))
  comp_names=paste0(he$interventions[he$ref]," vs ",he$interventions[he$comp])

  # expected net benefit for each intervention at k = wtp
  EU_tab=matrix(apply(he$U[,idx,ints,drop=FALSE],3,mean),ncol=1,
                dimnames=list(he$interventions[ints],"Expected net benefit"))

  # increments (as matrices, so one or many comparisons are handled the same way)
  de=as.matrix(he$delta_e)
  dc=as.matrix(he$delta_c)
  ib=wtp*de-dc

  delta_tab=data.frame(
    "Average cost differential"=colMeans(dc),
    "Average benefit differential"=colMeans(de),
    row.names=comp_names,check.names=FALSE
  )

  comp_tab=data.frame(
    EIB=colMeans(ib),
    CEAC=colMeans(ib>0),
    ICER=he$ICER,
    row.names=comp_names
  )

  ## printout
  cat("\nCost-effectiveness analysis summary\n\n")
  cat("Reference intervention:  ",he$interventions[he$ref],"\n",sep="")
  lab=if (length(he$comp)==1) "Comparator intervention: " else "Comparator intervention(s): "
  cat(lab,paste(he$interventions[he$comp],collapse=paste0("\n",strrep(" ",nchar(lab)-2),": ")),
      "\n\n",sep="")

  if (!is.na(he$step)) {
    r=rle(he$best)
    n=length(r$values)
    best=he$interventions[r$values]
    if (n==1) {
      cat(best," dominates for all k in [",min(he$k)," - ",max(he$k),"]\n",sep="")
    } else {
      brk=he$k[cumsum(r$lengths)[-n]+1]
      rng=c(paste0("k < ",brk[1]),
            if (n>2) paste0(brk[-(n-1)]," <= k < ",brk[-1]),
            paste0("k >= ",brk[n-1]))
      cat("Optimal decision: choose ",
          paste0(best," for ",rng,collapse=paste0("\n",strrep(" ",25))),"\n",sep="")
    }
  }

  cat("\n\nAnalysis for willingness to pay parameter k = ",wtp,"\n\n",sep="")
  print(EU_tab,digits=5)
  cat("\n")
  print(delta_tab,digits=5)
  cat("\n")
  print(comp_tab,digits=5)
  cat("\n")
  cat("Optimal intervention (max expected net benefit) for k = ",wtp,": ",
      he$interventions[he$best[idx]],"\n",sep="")
  cat("EVPI: ",format(he$evi[idx],digits=5),"\n",sep="")

  invisible(he)
}
