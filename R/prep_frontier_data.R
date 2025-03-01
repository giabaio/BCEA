
#' Prepare frontier data
#' 
#' @template args-he
#' @param threshold Cost-effectiveness threshold i.e angle of line. Must be >=0 or NULL.
#' @param start.origin Where should the frontier start from?
#' @return List with scatter.data, ceef.points, orig.avg
#' @seealso ceef.plot
#' @importFrom cli cli_alert_warning
#' @keywords internal
#' 
prep_frontier_data <- function(he,
                               threshold = NULL,
                               start.origin = TRUE) {
  ## if threshold is NULL, then bound to pi/2, which is atan(Inf)
  ## else if positive, bound to the increase angle given the slope
  if (is.null(threshold)) {
    threshold <- pi/2
  } else {
    if (threshold <= 0) {
      cli::cli_alert_warning(
        "The value of the cost-effectiveness threshold should be positive.
        The argument will be ignored.")
      threshold <- pi/2
    } else {
      threshold <- atan(threshold)
    }
  }
  
  ## quick fix:
  he$e <- he$e[, c(he$comp, he$ref)]
  he$c <- he$c[, c(he$comp, he$ref)]
  
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
  
  if (start.origin) {
    if (e.neg && !c.neg) {
      message("Benefits are negative, the frontier will not start from the origins")
      start.origin <- FALSE
    } else if (!e.neg && c.neg) {
      message("Costs are negative, the frontier will not start from the origins")
      start.origin <- FALSE
    } else if (e.neg && c.neg) {
      message("Costs and benefits are negative, the frontier will not start from the origins")
      start.origin <- FALSE
    } else if (min(means_c) != ec_min[2]) {
      message("Least effective intervention is not the cheapest, the frontier will not start from the origins")
      start.origin <- FALSE
    }
  }
  
  # frontier calculation
  data.avg <-
    data.frame(
      "e.avg" = means_e - ifelse(start.origin, ec_min["mean_e"], 0),
      "c.avg" = means_c - ifelse(start.origin, ec_min["mean_c"], 0))
  
  orig.avg <- cbind(data.avg,
                    as.factor(c(he$comp, he$ref)))
  
  names(orig.avg) <- c("e.orig", "c.orig", "comp")
  
  data.avg <- cbind(data.avg, orig.avg)
  
  ##TODO: where should this be used?  
  ##TODO: should this check if ALL interventions are 0?
  # check for interventions with zero costs and effectiveness
  # ce_zeros <-
  #   xor(data.avg["e.avg"] == 0,
  #       data.avg["c.avg"] == 0)
  # 
  # comp <-
  #   ifelse(any(ce_zeros),
  #          yes = which(ce_zeros),
  #          no = 0)
  
  # contains the points connecting the frontier
  # always starts from the origin
  ceef.points <-
    data.frame(
      x = 0,
      y = 0,
      comp = 0)
  
  ##TODO: new code
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
  repeat {
    if (prod(dim(data.avg)) == 0) break
    
    theta <- atan(data.avg$c.avg/data.avg$e.avg)
    theta.min <- min(theta, na.rm = TRUE)
    
    if (theta.min > threshold) break
    index <- which(theta == theta.min)
    
    if (length(index) > 1)
      index <- index[which.min(data.avg$e.avg[index])]
    
    ceef_orig_idx <- c(data.avg$e.orig[index],
                       data.avg$c.orig[index],
                       data.avg$comp[index])
    
    ceef.points <- rbind(ceef.points, ceef_orig_idx)
    
    rep_dataavg <- rep(as.numeric(data.avg[index, 3:4]), dim(data.avg)[1])
    
    data.avg[, 1:2] <-
      data.avg[, 3:4] - matrix(rep_dataavg, ncol = 2, byrow = TRUE)
    
    pos_prod <- dplyr::filter(data.avg, .data$c.avg*.data$e.avg > 0)
    data.avg <- dplyr::filter(pos_prod, .data$c.avg + .data$e.avg > 0)
  }
  
  ##############
  
  ceef.points$comp <- factor(ceef.points$comp)
  ceef.points$slope <- NA
  
  ## calculate slopes
  for (i in 2:dim(ceef.points)[1]) {
    ceef.points$slope[i] <-
      (ceef.points$y[i] - ceef.points$y[i - 1])/(ceef.points$x[i] - ceef.points$x[i - 1])
  }
  
  ## workaround for start.origin == FALSE: remove first row if slope is negative
  while (dim(ceef.points)[1] > 1 &&
         ceef.points$slope[2] < 0) {
    ceef.points <- ceef.points[-1, ]
    ceef.points$slope[1] <- NA
  }
  
  # points
  scatter.data <- data.frame(
    e = c(he$e) - ifelse(!start.origin, 0, ec_min["mean_e"]),
    c = c(he$c) - ifelse(!start.origin, 0, ec_min["mean_c"]),
    comp = as.factor(rep(c(he$comp, he$ref), each = he$n_sim)))
  compLabels <- he$interventions[scatter.data$comp |> unique() |> sort()]
    
  ## re-adjustment of data sets
  ceef.points[, "x"] <- ceef.points[, "x"] #+ ifelse(!e.neg, 0, ec_min["mean_e"])
  ceef.points[, "y"] <- ceef.points[, "y"] #+ ifelse(!e.neg, 0, ec_min["mean_c"])
  
  orig.avg[, "e.orig"] <- orig.avg[, "e.orig"] #+ ifelse(!e.neg, 0, ec_min["mean_e"])
  orig.avg[, "c.orig"] <- orig.avg[, "c.orig"] #+ ifelse(!e.neg, 0, ec_min["mean_c"])
  
  list(scatter.data = scatter.data,
       ceef.points = ceef.points,
       orig.avg = orig.avg,
       compLabels = compLabels)
}

