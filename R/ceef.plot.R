
#' @rdname ceef.plot
#' 
#' @param comparators Vector specifying the comparators to be included in the
#' frontier analysis. It must have a length > 1. Default as \code{NULL} includes
#' all the available comparators.
#' @template args-pos
#' @param start.from.origins Logical. Should the frontier start from the
#'  origins of the axes? The argument is reset to \code{FALSE} if the average
#'  effectiveness and/or costs of at least one comparator are negative.
#' @param threshold Specifies if the efficiency should be defined based on a
#'  willingness-to-pay threshold value. If set to \code{NULL} (the default), no
#'  conditions are included on the slope increase. If a positive value is passed
#'  as argument, to be efficient an intervention also requires to have an ICER
#'  for the comparison versus the last efficient strategy not greater than the
#'  specified threshold value. A negative value will be ignored with a warning.
#' @param flip Logical. Should the axes of the plane be inverted?
#' @param dominance Logical. Should the dominance regions be included in the
#'  plot?
#' @param relative Logical. Should the plot display the absolute measures (the
#'  default as \code{FALSE}) or the differential outcomes versus the reference
#'  comparator?
#' @param print.summary Logical. Should the efficiency frontier summary be
#'  printed along with the graph?  See Details for additional information.
#' @param graph_type A string used to select the graphical engine to use for
#'  plotting. Should (partial-)match the two options \code{"base"} or
#'  \code{"ggplot2"}. Default value is \code{"base"}.
#' @param ... If \code{graph_type="ggplot2"} and a named theme object is supplied,
#'  it will be added to the ggplot object. Ignored if \code{graph_type="base"}.
#'  Setting the optional argument \code{include.ICER} to \code{TRUE} will print
#'  the ICERs in the summary tables, if produced.
#' 
#' @return \item{ceplane}{ A ggplot object containing the plot. Returned only
#' if \code{graph_type="ggplot2"}. } The function produces a plot of the
#' cost-effectiveness efficiency frontier. The dots show the simulated values
#' for the intervention-specific distributions of the effectiveness and costs.
#' The circles indicate the average of each bivariate distribution, with the
#' numbers referring to each included intervention. The numbers inside the
#' circles are black if the intervention is included in the frontier and grey
#' otherwise. If the option \code{dominance} is set to \code{TRUE}, the
#' dominance regions are plotted, indicating the areas of dominance.
#' Interventions in the areas between the dominance region and the frontier are
#' in a situation of extended dominance.
#' @author Andrea Berardi, Gianluca Baio
#' @seealso \code{\link{bcea}}
#' @references
#' Baio G. (2012). Bayesian Methods in Health Economics. CRC/Chapman Hall, London.
#' 
#' IQWIG (2009). General methods for the Assessment of the Relation of Benefits
#' to Cost, Version 1.0. IQWIG, November 2009.
#' @concept "Health economic evaluation" "Multiple comparisons"
#' @importFrom graphics rect abline points legend box
#' @importFrom grDevices colours
#' 
#' @examples
#' 
#' ## create the bcea object m for the smoking cessation example
#' data(Smoking)
#' m <- bcea(e, c, ref = 4, Kmax = 500, interventions = treats)
#' 
#' ## produce plot
#' ceef.plot(m, graph_type = "base")
#' 
#' \donttest{
#' ## tweak the options
#' ## flip axis
#' ceef.plot(m,
#'           flip = TRUE,
#'           dominance = FALSE,
#'           start.from.origins = FALSE,
#'           print.summary = FALSE,
#'           graph_type = "base")
#'           
#' ## or use ggplot2 instead
#' if(require(ggplot2)){
#' ceef.plot(m,
#'           dominance = TRUE,
#'           start.from.origins = FALSE,
#'           pos = TRUE,
#'           print.summary = FALSE,
#'           graph_type = "ggplot2")
#'  }
#' }
#' 
#' @export
#' 
ceef.plot.bcea <- function(he,
                           comparators = NULL,
                           pos = c(1, 1),
                           start.from.origins = TRUE,
                           threshold = NULL,
                           flip = FALSE,
                           dominance = TRUE,
                           relative = FALSE,
                           print.summary = TRUE,
                           graph_type = c("base", "ggplot2"),
                           ...) {
   
   if (any(is.null(he$c)) || any(is.null(he$e)))
      stop("Please use the bcea() function from BCEA version >= 2.1-0 or attach the vectors e and c to the bcea object.
           Please see ?ceef.plot for additional details.", call. = FALSE)
   
   ## if threshold is NULL, then bound to pi/2, which is atan(Inf)
   ## else if positive, bound to the increase angle given the slope
   if (is.null(threshold))
      threshold <- pi/2
   else {
      if (threshold <= 0) {
         warning("The value of the cost-effectiveness threshold should be positive. The argument will be ignored.")
         threshold <- pi/2
      } else
         threshold <- atan(threshold)
   }
   # Gives you the possibility of suppressing the plot (if 'print.plot' set to FALSE)
   exArgs <- list(...)
   
   if (exists("print.plot", exArgs)) {
      print.plot <- exArgs$print.plot
   } else {
      print.plot <- TRUE}
   
   ### selects the comparators. No need for recursion
   if (!is.null(comparators)) {
      stopifnot(all(comparators %in% 1:he$n_comparators))
      # adjusts bcea object for the correct number of dimensions and comparators
      he$comp <- he$comp[comparators]
      he$n_comparators <- length(comparators)
      he$n_omparisons <- length(comparators) - 1
      he$interventions <- he$interventions[comparators]
      he$ref <- rank(c(he$ref, he$comp))[1]
      he$comp <- rank(c(he$ref, he$comp))[-1]
      he$change_comp <- TRUE
      ### bceanew
      he$e <- he$e[, comparators]
      he$c <- he$c[, comparators]
   }
   
   ### If the incremental analysis (relative to the reference) is required, needs to modify the BCEA object
   if (relative) {
      temp <- he
      temp$e <- temp$c <- matrix(NA, he$n_sim, he$n_comparators)
      temp$e[, he$ref] <- temp$c[, he$ref] <- rep(0, he$n_sim)
      temp$e[, -he$ref] <- -he$delta.e
      temp$c[, -he$ref] <- -he$delta.c
      he <- temp
   }
   
   stopifnot(he$n_comparators >= 2)
   
   base.graphics <- all(pmatch(graph_type,c("base","ggplot2")) != 2)
   
   ### no visible binding note
   c.avg <- e.avg <- x <- y <- e <- e.orig <- c.orig <- NA_real_
   
   ### if the effectiveness is negative or !start.from.origins, rescale
   ec.min <- with(he,
                  c(min(apply(e, 2, mean)),
                    apply(c,2,mean)[which.min(apply(e, 2, mean))],
                    which.min(apply(e, 2, mean))))
   e.neg <- ec.min[1] < 0
   c.neg <- any(apply(he$c, 2, mean) < 0)
   
   if (e.neg && !c.neg && start.from.origins) {
      message("Benefits are negative, the frontier will not start from the origins")
      start.from.origins <- FALSE
   }
   if (!e.neg && c.neg & start.from.origins) {
      message("Costs are negative, the frontier will not start from the origins")
      start.from.origins <- FALSE
   }
   if (e.neg && c.neg & start.from.origins) {
      message("Costs and benefits are negative, the frontier will not start from the origins")
      start.from.origins <- FALSE
   }
   e.neg <- ifelse(start.from.origins, e.neg, TRUE)
   
   ### frontier calculation
   data.avg <-
      data.frame(
         "e.avg" = apply(he$e, 2, mean) - ifelse(!e.neg, 0, ec.min[1]),
         "c.avg" = apply(he$c, 2, mean) - ifelse(!e.neg, 0, ec.min[2]))
   data.avg <- cbind(data.avg,
                     data.avg,as.factor(c(1:dim(data.avg)[1])))
   names(data.avg)[3:5] <- c("e.orig","c.orig","comp")
   orig.avg <- data.avg[, 3:5]
   ### check for interventions with zero costs and effectiveness
   comp <- ifelse(any(apply(data.avg[, 1:2],
                            1,
                            function(x) isTRUE(sum(x) == 0 & prod(x) == 0))),
                  which(apply(data.avg[, 1:2], 1, sum) == 0 & apply(data.avg[, 1:2], 1, prod) == 0), 0)
   ### contains the points connecting the frontier. Always starts from the origins
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
         data.avg[, 3:4] - matrix(rep(as.numeric(data.avg[index, 3:4]), dim(data.avg)[1]), ncol = 2, byrow = TRUE)
      data.avg <- subset(subset(data.avg, c.avg*e.avg > 0), c.avg + e.avg > 0)
   }
   ceef.points$comp <- factor(ceef.points$comp)
   
   ceef.points$slope <- NA
   ## calculate slopes
   for (i in 2:dim(ceef.points)[1])
      ceef.points$slope[i] <- with(ceef.points, (y[i] - y[i-1])/(x[i] - x[i-1]))
   
   ## workaround for start.from.origins == FALSE: remove first row if slope is negative
   while (dim(ceef.points)[1] > 1 && ceef.points$slope[2] < 0) {
      ceef.points <- ceef.points[-1, ]
      ceef.points$slope[1] <- NA
   }
   
   ## set data.frame for points
   scatter.data <- data.frame(
      e = c(he$e),#-ifelse(!e.neg,0,ec.min[1]),
      c = c(he$c),#-ifelse(!e.neg,0,ec.min[2]),
      comp = as.factor(sort(rep(1:he$n_comparators,he$n_sim))))
   
   ### re-adjustment of data sets
   ceef.points[,1] <- ceef.points[,1] + ifelse(!e.neg, 0, ec.min[1])
   ceef.points[,2] <- ceef.points[,2] + ifelse(!e.neg, 0, ec.min[2])
   orig.avg[,1] <- orig.avg[,1] + ifelse(!e.neg, 0, ec.min[1])
   orig.avg[,2] <- orig.avg[,2] + ifelse(!e.neg, 0, ec.min[2])
   
   ### Summary table function
   ceef.summary <- function(he, ceef.points, orig.avg, include.ICER = FALSE,...) {
      ## Tables adaptation and formatting
      no.ceef <- which(!1:he$n_comparators %in% ceef.points$comp)
      ## Interventions included
      if (ceef.points$comp[1] == 0)
         ceef.points <- ceef.points[-1, ]
      
      rownames(ceef.points) <-
         he$interventions[as.numeric(levels(ceef.points$comp)[ceef.points$comp])]
      
      if (!include.ICER) {
         ceef.points[, 5] <- atan(ceef.points[, 4]^(1*ifelse(!flip, 1, -1)))
         ceef.points <- ceef.points[, -3]
         colnames(ceef.points) <- c("Effectiveness","Costs","Increase slope","Increase angle")
      } else {
         ICERs <- numeric(dim(ceef.points)[1])
         index <- as.numeric(levels(ceef.points$comp)[ceef.points$comp])
         for (i in seq_along(ICERs)) {
            if (ceef.points$comp[i] == he$ref)
               ICERs[i] <- NA_real_
            else
               ICERs[i] <- he$ICER[index[i]+ifelse(index[i]<he$ref, 0, -1)]
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
         noceef.points[, 1:2] <- orig.avg[no.ceef, -3]
         
         if (!include.ICER) {
            noceef.points <- noceef.points[, -3]
            colnames(noceef.points) <- c("Effectiveness","Costs","Dominance type")
         } else {
            ICERs <- numeric(dim(noceef.points)[1])
            for (i in seq_along(ICERs)) {
               if(no.ceef[i] == he$ref)
                  ICERs[i] <- NA_real_
               else
                  ICERs[i] <- he$ICER[no.ceef[i] + ifelse(no.ceef[i] < he$ref, 0, -1)]
            }
            noceef.points[,3] <- ICERs
            colnames(noceef.points) <-
               c("Effectiveness","Costs", paste0("ICER ",he$interventions[he$ref]," vs."),"Dominance type")
         }
         
         how.dominated <- rep("Extended dominance",length(no.ceef))
         for (i in seq_along(no.ceef))
            for (j in 1:dim(ceef.points)[1]) {
               ## if the product of the deltas is negative it is dominated
               ## cannot be dominant since not on the frontier 
               if((noceef.points[i, 1] - ceef.points[j, 1])*(noceef.points[i, 2] - ceef.points[j, 2]) < 0) {
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
      
      ### Print the summary table
      cat("\nCost-effectiveness efficiency frontier summary \n\n")
      cat("Interventions on the efficiency frontier:\n")
      print(ceef.points, quote = FALSE, digits = 5, justify = "center")
      cat("\n")
      
      if (length(no.ceef) > 0){
         cat("Interventions not on the efficiency frontier:\n")
         print(noceef.points, quote = FALSE, digits = 5, justify = "center")
      }
   }
   if (print.summary)
      ceef.summary(he,ceef.points,orig.avg,...)
   
   
   # plots -------------------------------------------------------------------
   
   colour <- colours()[floor(seq(262, 340, length.out = he$n_comparators))]  # gray scale
   
   if (base.graphics) {
      if (print.plot) {
         ceef_plot_base(pos,
                        scatter.data,
                        flip,
                        relative,
                        dominance,
                        ceef.points,
                        colour,
                        orig.avg)
      }
   } else {
      if (print.plot) {
         ceef_plot_ggplot(he,
                          ceef.points,
                          scatter.data,
                          exArgs,
                          dominance,
                          relative,
                          orig.avg,
                          colour,
                          flip,
                          pos, ...)
      }
   }
}


#' Cost-Effectiveness Efficiency Frontier (CEAF) Plot
#' 
#' Back compatibility with BCEA previous versions:
#' The \code{bcea} objects did not include the generating \code{e} and \code{c}
#' matrices in BCEA versions <2.1-0. This function is not compatible with
#' objects created with previous versions. The matrices can be appended to
#' \code{bcea} objects obtained using previous versions, making sure that the
#' class of the object remains unaltered.
#' 
#' The argument \code{print.summary} allows for printing a brief summary of the
#' efficiency frontier, with default to \code{TRUE}. Two tables are plotted,
#' one for the interventions included in the frontier and one for the dominated
#' interventions. The average costs and clinical benefits are included for each
#' intervention. The frontier table includes the slope for the increase in the
#' frontier and the non-frontier table displays the dominance type of each
#' dominated intervention. Please note that the slopes are defined as the
#' increment in the costs for a unit increment in the benefits even if
#' \code{flip = TRUE} for consistency with the ICER definition. The angle of
#' increase is in radians and depends on the definition of the axes, i.e. on
#' the value given to the \code{flip} argument.
#' 
#' If the argument \code{relative} is set to \code{TRUE}, the graph will not
#' display the absolute measures of costs and benefits. Instead the axes will
#' represent differential costs and benefits compared to the reference
#' intervention (indexed by \code{ref} in the \code{\link{bcea}} function).
#' 
#' @template args-he
#' @export
#' 
ceef.plot <- function(he, ...) {
   UseMethod('ceef.plot', he)
}

