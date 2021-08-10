
#'
prep_frontier_data <- function(he) {
  
  ### no visible binding note
  c.avg <- e.avg <- x <- y <- e <- e.orig <- c.orig <- NA_real_
  
  ### if the effectiveness is negative or !start.from.origins, rescale
  ec.min <- with(he,
                 c(min(apply(e, 2, mean)),
                   apply(c, 2, mean)[which.min(apply(e, 2, mean))],
                   which.min(apply(e, 2, mean))))
  e.neg <- ec.min[1] < 0
  c.neg <- any(apply(he$c, 2, mean) < 0)
  
  if (e.neg && !c.neg && start.from.origins) {
    message("Benefits are negative, the frontier will not start from the origins")
    start.from.origins <- FALSE
  }
  if (!e.neg && c.neg && start.from.origins) {
    message("Costs are negative, the frontier will not start from the origins")
    start.from.origins <- FALSE
  }
  if (e.neg && c.neg && start.from.origins) {
    message("Costs and benefits are negative, the frontier will not start from the origins")
    start.from.origins <- FALSE
  }
  e.neg <- ifelse(start.from.origins, e.neg, TRUE)
  
  # frontier calculation
  data.avg <-
    data.frame(
      "e.avg" = apply(he$e, 2, mean) - ifelse(!e.neg, 0, ec.min[1]),
      "c.avg" = apply(he$c, 2, mean) - ifelse(!e.neg, 0, ec.min[2]))
  
  data.avg <- cbind(data.avg,
                    data.avg,
                    as.factor(c(1:dim(data.avg)[1])))
  
  names(data.avg)[3:5] <- c("e.orig", "c.orig", "comp")
  orig.avg <- data.avg[, 3:5]
  
  # check for interventions with zero costs and effectiveness
  comp <-
    ifelse(
      any(apply(data.avg[, 1:2], 1,
                function(x) isTRUE(sum(x) == 0 & prod(x) == 0))),
      which(apply(data.avg[, 1:2], 1, sum) == 0 &
              apply(data.avg[, 1:2], 1, prod) == 0), 0)
  
  # contains the points connecting the frontier
  # always starts from the origins
  ceef.points <-
    data.frame(
      x = 0,
      y = 0,
      comp = comp)
  
  repeat{
    if (prod(dim(data.avg)) == 0) break
    
    theta <- with(data.avg, atan(c.avg/e.avg))
    theta.min <- min(theta, na.rm = TRUE)
    
    if (theta.min > threshold) break
    index <- which(theta == theta.min)
    
    if (length(index) > 1)
      index <- index[which.min(data.avg$e.avg[index])]
    
    ceef.points <- with(data.avg,
                        rbind(ceef.points, c(e.orig[index], c.orig[index], comp[index])))
    data.avg[, 1:2] <-
      data.avg[, 3:4] - matrix(rep(as.numeric(data.avg[index, 3:4]), dim(data.avg)[1]),
                               ncol = 2,
                               byrow = TRUE)
    data.avg <- subset(subset(data.avg, c.avg*e.avg > 0), c.avg + e.avg > 0)
  }
  
  ceef.points$comp <- factor(ceef.points$comp)
  
  ceef.points$slope <- NA
  
  ## calculate slopes
  for (i in 2:dim(ceef.points)[1])
    ceef.points$slope[i] <- with(ceef.points, (y[i] - y[i-1])/(x[i] - x[i-1]))
  
  ## workaround for start.from.origins == FALSE: remove first row if slope is negative
  while (dim(ceef.points)[1] > 1 &&
         ceef.points$slope[2] < 0) {
    ceef.points <- ceef.points[-1, ]
    ceef.points$slope[1] <- NA
  }
  
  keep_intervs <- c(he$ref, he$comp) #NG
  ##TODO: why is the old code for fewer columns?
  # points
  scatter.data <- data.frame(
    e = c(he$e[, keep_intervs]), #-ifelse(!e.neg, 0, ec.min[1]),
    c = c(he$c[, keep_intervs]), #-ifelse(!e.neg, 0, ec.min[2]),
    comp = as.factor(rep(keep_intervs, he$n_sim)))
  # comp = as.factor(sort(rep(1:he$n_comparators, he$n_sim))))
  
  ## re-adjustment of data sets
  ceef.points[, 1] <- ceef.points[, 1] + ifelse(!e.neg, 0, ec.min[1])
  ceef.points[, 2] <- ceef.points[, 2] + ifelse(!e.neg, 0, ec.min[2])
  
  orig.avg[, 1] <- orig.avg[, 1] + ifelse(!e.neg, 0, ec.min[1])
  orig.avg[, 2] <- orig.avg[, 2] + ifelse(!e.neg, 0, ec.min[2])
  
  list(scatter.data = scatter.data,
       ceef.points = ceef.points,
       orig.avg = orig.avg)
}

