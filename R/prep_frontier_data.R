
#' @template args-he
#' 
prep_frontier_data <- function(he,
                               threshold,
                               start.origin) {
  
  ## if threshold is NULL, then bound to pi/2, which is atan(Inf)
  ## else if positive, bound to the increase angle given the slope
  if (is.null(threshold)) {
    threshold <- pi/2
  } else {
    if (threshold <= 0) {
      warning(
        "The value of the cost-effectiveness threshold should be positive. The argument will be ignored.",
        call. = FALSE)
      threshold <- pi/2
    } else {
      threshold <- atan(threshold)
    }
  }
  
  # if the effectiveness is negative or
  # !start.origin then rescale
  # drop names
  means_e <- `names<-`(apply(he$e, 2, mean), NULL)
  means_c <- `names<-`(apply(he$c, 2, mean), NULL)
  
  ec_min <- c(mean_e = min(means_e),
              mean_c = means_c[which.min(means_e)],
              interv = which.min(means_e))
  
  e.neg <- ec_min["mean_e"] < 0
  c.neg <- any(means_c < 0)
  
  if (e.neg && !c.neg && start.origin) {
    message("Benefits are negative, the frontier will not start from the origins")
    start.origin <- FALSE
  }
  if (!e.neg && c.neg && start.origin) {
    message("Costs are negative, the frontier will not start from the origins")
    start.origin <- FALSE
  }
  if (e.neg && c.neg && start.origin) {
    message("Costs and benefits are negative, the frontier will not start from the origins")
    start.origin <- FALSE
  }
  
  # frontier calculation
  data.avg <-
    data.frame(
      "e.avg" = means_e - ifelse(start.origin, 0, ec_min["mean_e"]),
      "c.avg" = means_c - ifelse(start.origin, 0, ec_min["mean_c"]))
  
  orig.avg <- cbind(data.avg,
                    as.factor(c(1:dim(data.avg)[1])))
  
  names(orig.avg) <- c("e.orig", "c.orig", "comp")
  
  data.avg <- cbind(data.avg, orig.avg)
  
  # check for interventions with zero costs and effectiveness
  ##TODO: should this check if ALL interventions are 0?
  ce_zeros <-
    xor(data.avg["e.avg"] == 0,
        data.avg["c.avg"] == 0)

  comp <-
    ifelse(any(ce_zeros),
           yes = which(ce_zeros),
           no = 0)
  
  # contains the points connecting the frontier
  # always starts from the origin
  ceef.points <-
    data.frame(
      x = 0,
      y = 0,
      comp = comp)
  
  ##TODO: get some old output and compare/reverse engineer
  ##      what is this doing?
  # repeat {
  #   no_data <- prod(dim(data.avg)) == 0
  #   
  #   if (no_data) break
  #   
  #   slope <- atan(data.avg$c.avg/data.avg$e.avg)
  #   slope.min <- min(slope, na.rm = TRUE)
  #   
  #   if (slope.min > threshold) break
  #   
  #   interv_idx <- which(slope == slope.min)
  #   
  #   if (length(interv_idx) > 1)
  #     interv_idx <- interv_idx[which.min(data.avg$e.avg[interv_idx])]
  #   
  #   ceef.points <- with(data.avg,
  #                       rbind(ceef.points, orig.avg[interv_idx, ]))
  # 
  #   # move origin to current (e,c)
  #   data.avg[, c("e.avg", "c.avg")] <-
  #     data.avg[, c("e.orig", "c.orig")] - 
  #     matrix(rep(as.numeric(data.avg[interv_idx, c("e.orig", "c.orig")]),
  #                nrow(data.avg)),
  #            ncol = 2,
  #            byrow = TRUE)
  #   
  #   data.avg <- subset(data.avg, c.avg*e.avg > 0 & c.avg + e.avg > 0)
  # }
  
  #### OLD CODE
  repeat{
    if (prod(dim(data.avg)) == 0) break
    
    theta <- with(data.avg,atan(c.avg/e.avg))
    theta.min <- min(theta, na.rm = TRUE)
    
    if (theta.min > threshold) break
    index <- which(theta == theta.min)
    
    if (length(index) > 1)
      index=index[which.min(data.avg$e.avg[index])]
    ceef.points <- with(data.avg,
                        rbind(ceef.points, c(e.orig[index], c.orig[index], comp[index])))
    data.avg[, 1:2] <-
      data.avg[,3:4]-matrix(rep(as.numeric(data.avg[index, 3:4]), dim(data.avg)[1]), ncol = 2, byrow = TRUE)
    data.avg <- subset(subset(data.avg,c.avg*e.avg > 0), c.avg+e.avg > 0)
  }
  
  ####
  
  ceef.points$comp <- factor(ceef.points$comp)
  
  ceef.points$slope <- NA
  
  ## calculate slopes
  for (i in 2:dim(ceef.points)[1])
    ceef.points$slope[i] <- with(ceef.points, (y[i] - y[i-1])/(x[i] - x[i-1]))
  
  ## workaround for start.origin == FALSE: remove first row if slope is negative
  while (dim(ceef.points)[1] > 1 &&
         ceef.points$slope[2] < 0) {
    ceef.points <- ceef.points[-1, ]
    ceef.points$slope[1] <- NA
  }
  
  keep_intervs <- sort(c(he$ref, he$comp)) #NG
  ##TODO: why is the old code for fewer columns?
  # points
  scatter.data <- data.frame(
    e = c(he$e[, keep_intervs]), #-ifelse(!e.neg, 0, ec_min[1]),
    c = c(he$c[, keep_intervs]), #-ifelse(!e.neg, 0, ec_min[2]),
    comp = as.factor(rep(keep_intervs, each = he$n_sim)))
  # comp = as.factor(sort(rep(1:he$n_comparators, he$n_sim))))
  
  ## re-adjustment of data sets
  ceef.points[, 1] <- ceef.points[, 1] + ifelse(!e.neg, 0, ec_min[1])
  ceef.points[, 2] <- ceef.points[, 2] + ifelse(!e.neg, 0, ec_min[2])
  
  orig.avg[, 1] <- orig.avg[, 1] + ifelse(!e.neg, 0, ec_min[1])
  orig.avg[, 2] <- orig.avg[, 2] + ifelse(!e.neg, 0, ec_min[2])
  
  list(scatter.data = scatter.data,
       ceef.points = ceef.points,
       orig.avg = orig.avg)
}

