
#'
prep_frontier_data <- function(he,
                               start.origin) {
  browser()
  
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
  repeat {
    no_data <- prod(dim(data.avg)) == 0
    
    if (no_data) break
    
    wtp <- atan(data.avg$c.avg/data.avg$e.avg)
    wtp.min <- min(wtp, na.rm = TRUE)
    
    if (wtp.min > threshold) break
    
    index <- which(wtp == wtp.min)
    
    if (length(index) > 1)
      index <- index[which.min(data.avg$e.avg[index])]
    
    ceef.points <- with(data.avg,
                        rbind(ceef.points, orig.avg[index, ]))

    data.avg[, c("e.avg", "c.avg")] <-
      data.avg[, c("e.orig", "c.orig")] - 
      matrix(rep(as.numeric(data.avg[index, c("e.orig", "c.orig")]),
                 nrow(data.avg)),
             ncol = 2,
             byrow = TRUE)
    
    data.avg <- subset(data.avg, c.avg*e.avg > 0)
    data.avg <- subset(data.avg, c.avg + e.avg > 0)
  }
  
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
  
  keep_intervs <- c(he$ref, he$comp) #NG
  ##TODO: why is the old code for fewer columns?
  # points
  scatter.data <- data.frame(
    e = c(he$e[, keep_intervs]), #-ifelse(!e.neg, 0, ec_min[1]),
    c = c(he$c[, keep_intervs]), #-ifelse(!e.neg, 0, ec_min[2]),
    comp = as.factor(rep(keep_intervs, he$n_sim)))
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

