
#' Summary table for CEEF
#' 
#' @template args-he
#' @param frontier_data Frontier data
#' @param frontier_params Frontier parameters
#' @param include.ICER Should we include the ICER? default: FALSE
#' @param ... Additional arguments
#' 
#' @return Summary printed to console
#' @keywords internal print
#' 
ceef.summary <- function(he,
                         frontier_data,
                         frontier_params,
                         include.ICER = FALSE,
                         ...) {
  ceef.points <- frontier_data$ceef.points 
  orig.avg <- frontier_data$orig.avg
  
  flip <- frontier_params$flip
  
  ## tables adaptation and formatting
  no.ceef <- which(!1:he$n_comparators %in% ceef.points$comp)
  ## Interventions included
  if (ceef.points$comp[1] == 0)
    ceef.points <- ceef.points[-1, ]
  
  rownames(ceef.points) <-
    he$interventions[as.numeric(levels(ceef.points$comp)[ceef.points$comp])]
  
  if (!include.ICER) {
    ceef.points[, 5] <- atan(ceef.points[, 4]^(1*ifelse(!flip, 1, -1)))
    ceef.points <- ceef.points[, -3]
    colnames(ceef.points) <- c("Effectiveness", "Costs", "Increase slope", "Increase angle")
  } else {
    ICERs <- numeric(dim(ceef.points)[1])
    index <- as.numeric(levels(ceef.points$comp)[ceef.points$comp])
    for (i in seq_along(ICERs)) {
      if (ceef.points$comp[i] == he$ref)
        ICERs[i] <- NA_real_
      else
        ICERs[i] <- he$ICER[index[i] + ifelse(index[i]<he$ref, 0, -1)]
    }
    ceef.points[, 3] <- ICERs
    ceef.points[, 5] <- atan(ceef.points[, 4]^(1*ifelse(!flip, 1, -1)))
    colnames(ceef.points) <-
      c("Effectiveness",
        "Costs", paste0("ICER ", he$interventions[he$ref]," vs."),
        "Increase slope", "Increase angle")
  }
  if (flip) colnames(ceef.points)[1:2] <- colnames(ceef.points[2:1])
  
  ## Interventions not included
  if (length(no.ceef) > 0) {
    noceef.points <- data.frame(matrix(NA_real_, ncol = 4, nrow = length(no.ceef)))
    noceef.points[, 1:2] <- orig.avg[orig.avg$comp %in% no.ceef, -3]
    
    if (!include.ICER) {
      noceef.points <- noceef.points[, -3]
      colnames(noceef.points) <- c("Effectiveness", "Costs", "Dominance type")
    } else {
      ICERs <- numeric(dim(noceef.points)[1])
      for (i in seq_along(ICERs)) {
        if(no.ceef[i] == he$ref)
          ICERs[i] <- NA_real_
        else
          ICERs[i] <- he$ICER[no.ceef[i] + ifelse(no.ceef[i] < he$ref, 0, -1)]
      }
      noceef.points[, 3] <- ICERs
      colnames(noceef.points) <-
        c("Effectiveness", "Costs", paste0("ICER ", he$interventions[he$ref]," vs."), "Dominance type")
    }
    
    how.dominated <- rep("Extended dominance", length(no.ceef))
    
    for (i in seq_along(no.ceef))
      for (j in seq_len(dim(ceef.points)[1])) {
        ## if the product of the deltas is negative it is dominated
        ## cannot be dominant since not on the frontier 
        if ((noceef.points[i, 1] - ceef.points[j, 1])*(noceef.points[i, 2] - ceef.points[j, 2]) < 0) {
          how.dominated[i] <- "Absolute dominance"
          ## alternative:
          # how.dominated[i] <- paste0("Dominated by ",rownames(ceef.points)[j])
          break
        }
      }
    noceef.points[, ifelse(!include.ICER, 3, 4)] <- how.dominated
    rownames(noceef.points) <- he$interventions[no.ceef]
    
    if (flip) colnames(noceef.points)[1:2] <- colnames(noceef.points)[2:1]
  }
  
  ## Print the summary table
  cat("\nCost-effectiveness efficiency frontier summary \n\n")
  cat("Interventions on the efficiency frontier:\n")
  print(ceef.points, quote = FALSE, digits = 5, justify = "center")
  cat("\n")
  
  if (length(no.ceef) > 0) {
    cat("Interventions not on the efficiency frontier:\n")
    print(noceef.points, quote = FALSE, digits = 5, justify = "center")
  }
}

