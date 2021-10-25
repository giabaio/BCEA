
#' @rdname evppi
#'
#' @export
#'
evppi.bcea <- function(he,
                       param_idx,
                       input,
                       N = NULL,
                       plot = FALSE,
                       residuals = TRUE, ...) {
  
  if (is.null(colnames(input))) {
    colnames(input) <- paste0("theta", 1:dim(input)[2])
  }
  if (is.numeric(param_idx[1]) | is.integer(param_idx[1])) {
    params <- colnames(input)[param_idx]
  } else {
    params <- param_idx
    for (i in seq_along(params)) {
      param_idx[i] <- which(colnames(input) == params[i])
    }
    class(param_idx) <- "numeric"
  }
  if (is.null(N)) {
    N <- he$n_sim
  }
  
  robust <- NULL
  extra_args <- list(...)
  
  suppress.messages <- 
    if (!exists("suppress.messages", where = extra_args)) {
      FALSE
    } else {
      extra_args$suppress.messages
    }
  
  extra_args$select <- 
    if (!exists("select", where = extra_args)) {
      if (N >= he$n_sim) {
        seq_len(he$n_sim)
      } else {
        sample(seq_len(he$n_sim), size = N, replace = FALSE)
      }
    }
  
  inputs <- data.frame(input)[extra_args$select, ]
  
  # Sets default for method of calculation
  # If number of params <=4, then use GAM, if not defaults to INLA/SPDE
  
  if (!exists("method", where = extra_args)) {
    
    extra_args$method <-
      if (length(param_idx) <= 4) {
        list(rep("GAM", he$n_comparators - 1),
             rep("GAM", he$n_comparators - 1))
      } else {
        list(rep("INLA", he$n_comparators - 1),
             rep("INLA", he$n_comparators - 1))
      }
    
    print(paste("method:", extra_args$method))
    
  }
  
  if (!inherits(extra_args$method, "list")) {
    
    if (extra_args$method != "sad" & extra_args$method != "so") {
      if (length(extra_args$method) > 1) {
        extra_args$method <- list(extra_args$method,
                                  extra_args$method)
      } else {
        extra_args$method <-
          list(rep(extra_args$method, he$n_comparators - 1),
               rep(extra_args$method, he$n_comparators - 1))
      }
    }
  }
  
  if (inherits(extra_args$method, "list")) {
    
    len_methods <-
      length(extra_args$method[[1]]) +
      length(extra_args$method[[2]])
    
    if (len_methods != 2*(he$n_comparators - 1)) {
      stop(paste("The argument 'method' must be a list of length 2 with",
                 he$n_comparators - 1, "elements each."), call. = FALSE)
    }
  }
  
  if (!exists("int.ord", where = extra_args)) {
    extra_args$int.ord <-
      list(rep(1, he$n_comparators - 1),
           rep(1, he$n_comparators - 1))
  }
  
  if (!inherits(extra_args$int.ord, "list")) {
    
    extra_args$int.ord <-
      list(
        rep(extra_args$int.ord[1], he$n_comparators - 1),
        rep(extra_args$int.ord[2], he$n_comparators - 1)
      )
  }
  
  if (!inherits(extra_args$method, "list")) {
    if (extra_args$method == "sal" || extra_args$method == "sad") {
      method <- "Sadatsafavi et al"
      n.blocks <- NULL
      
      n_seps <-
        if (!exists("n_seps", where = extra_args)) {
          1
        } else {
          extra_args$n_seps}
      
      if (length(params) == 1) {
        d <- he$n_comparators
        n <- he$n_sim
        w <- params
        param <- inputs[, w]
        o <- order(param)
        param <- param[o]
        nSegs <- matrix(1, d, d)
        nSegs[1, 2] <- n_seps
        nSegs[2, 1] <- n_seps
        res <- segPoints <- numeric()
        
        for (k in seq_along(he$k)) {
          nbs <- he$U[, k, ]
          nbs <- nbs[o, ]
          for (i in seq_len(d - 1)) {
            for (j in (i + 1):d) {
              cm <- cumsum(nbs[, i] - nbs[, j])/n
              
              if (nSegs[i, j] == 1) {
                l <- which.min(cm)
                u <- which.max(cm)
                if (cm[u] - max(cm[1], cm[n]) > min(cm[1],
                                                    cm[n]) - cm[l]) {
                  segPoint <- u
                } else {
                  segPoint <- l
                }
                if (segPoint > 1 && segPoint < n) {
                  segPoints <- c(segPoints, segPoint)
                }
              }
              
              if (nSegs[i, j] == 2) {
                distMaxMin <- 0
                distMinMax <- 0
                minL <- Inf
                maxL <- -Inf
                for (sims in seq_len(n)) {
                  if (cm[sims] > maxL) {
                    maxLP <- sims
                    maxL <- cm[sims]
                  } else {
                    if (maxL - cm[sims] > distMaxMin) {
                      distMaxMin <- maxL - cm[sims]
                      segMaxMinL <- maxLP
                      segMaxMinR <- sims
                    }
                  }
                  if (cm[sims] < minL) {
                    minLP <- sims
                    minL <- cm[sims]
                  } else {
                    if (cm[sims] - minL > distMinMax) {
                      distMinMax <- cm[sims] - minL
                      segMinMaxL <- minLP
                      segMinMaxR <- sims
                    }
                  }
                }
                siMaxMin <- cm[segMaxMinL] + distMaxMin + (cm[n] - cm[segMaxMinR])
                siMinMax <- -cm[segMaxMinL] + distMinMax - (cm[n] - cm[segMinMaxR])
                
                if (siMaxMin > siMinMax) {
                  segPoint <- c(segMaxMinL, segMaxMinR)
                }
                else {
                  segPoint <- c(segMinMaxL, segMinMaxR)
                }
                if (segPoint[1] > 1 && segPoint[1] < n) {
                  segPoints <- c(segPoints, segPoint[1])
                }
                if (segPoint[2] > 1 && segPoint[2] < n) {
                  segPoints <- c(segPoints, segPoint[2])
                }
              }
            }
          }
          if (length(segPoints) > 0) {
            segPoints2 <- unique(c(0, segPoints[order(segPoints)], n))
            res[k] <- 0
            for (j in seq_len(length(segPoints2) - 1)) {
              res[k] <-
                res[k] + max(colSums(
                  matrix(nbs[(1 + segPoints2[j]):segPoints2[j + 1], ],
                         ncol = d)))/n
            }
            res[k] <- res[k] - max(colMeans(nbs))
          }
          else {
            res[k] <- 0
          }
        }
      }
      if (length(params) > 1) {
        res <- list()
        for (lp in seq_along(params)) {
          d <- he$n_comparators
          n <- he$n_sim
          w <- params[lp]
          param <- inputs[, w]
          o <- order(param)
          param <- param[o]
          nSegs <- matrix(1, d, d)
          nSegs[1, 2] <- n_seps
          nSegs[2, 1] <- n_seps
          temp <- segPoints <- numeric()
          
          for (k in seq_along(he$k)) {
            nbs <- he$U[, k, ]
            nbs <- nbs[o, ]
            
            for (i in seq_len(d - 1)) {
              for (j in (i + 1):d) {
                cm <- cumsum(nbs[, i] - nbs[, j])/n
                if (nSegs[i, j] == 1) {
                  l <- which.min(cm)
                  u <- which.max(cm)
                  if (cm[u] - max(cm[1], cm[n]) > min(cm[1], cm[n]) - cm[l]) {
                    segPoint <- u
                  } else {
                    segPoint <- l
                  }
                  if (segPoint > 1 && segPoint < n) {
                    segPoints <- c(segPoints, segPoint)
                  }
                }
                if (nSegs[i, j] == 2) {
                  distMaxMin <- 0
                  distMinMax <- 0
                  minL <- Inf
                  maxL <- -Inf
                  for (sims in seq_len(n)) {
                    if (cm[sims] > maxL) {
                      maxLP <- sims
                      maxL <- cm[sims]
                    } else {
                      if (maxL - cm[sims] > distMaxMin) {
                        distMaxMin <- maxL - cm[sims]
                        segMaxMinL <- maxLP
                        segMaxMinR <- sims
                      }
                    }
                    if (cm[sims] < minL) {
                      minLP <- sims
                      minL <- cm[sims]
                    } else {
                      if (cm[sims] - minL > distMinMax) {
                        distMinMax <- cm[sims] - minL
                        segMinMaxL <- minLP
                        segMinMaxR <- sims
                      }
                    }
                  }
                  siMaxMin <- cm[segMaxMinL] + distMaxMin + (cm[n] - cm[segMaxMinR])
                  siMinMax <- -cm[segMaxMinL] + distMinMax - (cm[n] - cm[segMinMaxR])
                  if (siMaxMin > siMinMax) {
                    segPoint <- c(segMaxMinL, segMaxMinR)
                  } else {
                    segPoint <- c(segMinMaxL, segMinMaxR)
                  }
                  if (segPoint[1] > 1 && segPoint[1] < n) {
                    segPoints <- c(segPoints, segPoint[1])
                  }
                  if (segPoint[2] > 1 && segPoint[2] < n) {
                    segPoints <- c(segPoints, segPoint[2])
                  }
                }
              }
            }
            if (length(segPoints) > 0) {
              segPoints2 <- unique(c(0, segPoints[order(segPoints)], n))
              temp[k] <- 0
              for (j in seq_len(length(segPoints2) - 1)) {
                temp[k] <-
                  temp[k] + max(colSums(
                    matrix(nbs[(1 + segPoints2[j]):segPoints2[j + 1], ],
                           ncol = d)))/n
              }
              temp[k] <- temp[k] - max(colMeans(nbs))
            } else {
              temp[k] <- 0
            }
          }
          res[[lp]] <- temp
        }
        names(res) <- params
      }
      
      res <- list(evppi = res,
                  index = params,
                  parameters = params,
                  k = he$k,
                  evi = he$evi,
                  method = method)
    }
    if (extra_args$method == "so") {
      method <- "Strong & Oakley (univariate)"
      n_seps <- NULL
      if (!exists("n.blocks", where = extra_args)) {
        stop("Please specify the param_idx 'n.blocks' to use the Strong and Oakley univariate method",
             call. = FALSE)
      } else {
        n.blocks <- extra_args$n.blocks
      }
      S <- he$n_sim
      J <- S/extra_args$n.blocks
      check <- S%%extra_args$n.blocks
      if (check > 0) {
        stop("number of simulations/number of blocks must be an integer.
             Please select a different value for n.blocks \n", call. = FALSE)
      }
      D <- he$n_comparators
      if (length(param_idx) == 1) {
        sort.order <- order(inputs[, params])
        sort.U <- array(NA, dim(he$U))
        evpi <- res <- numeric()
        for (i in seq_along(he$k)) {
          evpi[i] <- he$evi[i]
          sort.U[, i, ] <- he$U[sort.order, i, ]
          U.array <- array(sort.U[, i, ],
                           dim = c(J, extra_args$n.blocks, D))
          mean.k <- apply(U.array, c(2, 3), mean)
          partial.info <- mean(apply(mean.k, 1, max))
          res[i] <- partial.info - max(apply(he$U[, i, ], 2, mean))
        }
      }
      if (length(param_idx) > 1) {
        res <- list()
        for (j in seq_along(param_idx)) {
          sort.order <- order(inputs[, params[j]])
          sort.U <- array(NA, dim(he$U))
          evpi <- evppi.temp <- numeric()
          
          for (i in seq_along(he$k)) {
            evpi[i] <- he$evi[i]
            sort.U[, i, ] <- he$U[sort.order, i, ]
            U.array <- array(sort.U[, i, ],
                             dim = c(J, n.blocks, D))
            mean.k <- apply(U.array, c(2, 3), mean)
            partial.info <- mean(apply(mean.k, 1, max))
            evppi.temp[i] <- partial.info - max(apply(he$U[, i, ], 2, mean))
          }
          res[[j]] <- evppi.temp
        }
        names(res) <- params
      }
      
      res <-
        list(evppi = res,
             index = params,
             parameters = params,
             k = he$k,
             evi = he$evi,
             method = method)
    }
  }
  
  if (inherits(extra_args$method, "list")) {
    time <- list()
    time[[1]] <- list()
    time[[2]] <- list()
    
    fit.full <- vector("list")
    fit.full[[1]] <- matrix(
      data = 0,
      nrow = length(extra_args$select),
      ncol = he$n_comparators)
    
    fit.full[[2]] <-
      matrix(data = 0,
             nrow = length(extra_args$select),
             ncol = he$n_comparators)
    
    for (k in 1:2) {
      for (l in seq_len(he$n_comparisons)) {
        
        x <-
          prep.x(he = he,
                 seq_rows = extra_args$select,
                 k = k,
                 l = l)
        
        method <- toupper(extra_args$method[[k]][l])
        
        if (method == "GAM" || method == "G") {
          method <- "GAM"
          mesh <- robust <- NULL
          if (!requireNamespace("mgcv", quietly = TRUE)) {
            stop("You need to install the package 'mgcv'. Please run in your R terminal:\n
                 install.packages('mgcv')", call. = FALSE)
          }
          if (requireNamespace("mgcv", quietly = TRUE)) {
            if (!suppress.messages) {
              cat("\n")
              cat("Calculating fitted values for the GAM regression \n")
            }
            
            inp <- names(inputs)[param_idx]
            
            form <- 
              if (exists("formula", where = extra_args)) {
                extra_args$formula
              } else {
                paste("te(", paste(inp, ",", sep = "",
                                   collapse = ""), "bs='cr')")
              }
            
            fit <- fit.gam(parameter = param_idx,
                           inputs = inputs,
                           x = x,
                           form = form)
          }
        }
        if (method == "GP") {
          mesh <- NULL
          if (!suppress.messages) {
            cat("\n")
            cat("Calculating fitted values for the GP regression \n")
            # If the number of simulations to be used to estimate the
            # hyper-params is set then use that, else use N/2
          }
          
          n_sim <- 
            if (!exists("n_sim", where = extra_args)) {
              N/2
            } else {
              extra_args$n_sim
            }
          
          fit <- fit.gp(parameter = param_idx,
                        inputs = inputs,
                        x = x,
                        n.sim = n_sim)
        }
        if (method == "INLA") {
          if (!requireNamespace("INLA", quietly = TRUE)) {
            stop("You need to install the packages 'INLA'. Please run in your R terminal:\n 
                 install.packages('INLA',repos=c(getOption('repos'),INLA='https://inla.r-inla-download.org/R/stable'), dep=TRUE)\n
                 If you are under MS Windows, you may need to install the most recent version\n
                 of 'Rtools' - see https://cran.r-project.org/bin/windows/Rtools/", call. = FALSE)
          }
          
          if (requireNamespace("INLA", quietly = TRUE)) {
            if (!is.element("INLA", (.packages()))) {
              attachNamespace("INLA")
            }
            if (length(param_idx) < 2) {
              stop("The INLA method can only be used with 2 or more params", call. = FALSE)
            }
            if (!suppress.messages) {
              cat("\n")
              cat("Finding projections \n")
            }
            projections <- make.proj(parameter = param_idx,
                                     inputs = inputs,
                                     x = x,
                                     k = k,
                                     l = l)
            data <- projections$data
            if (!suppress.messages) {
              cat("Determining Mesh \n")
            }
            
            cutoff <- 
              if (!exists("cutoff", where = extra_args)) {
                0.3
              } else {
                extra_args$cutoff
              }
            
            convex.inner <- 
              if (!exists("convex.inner", where = extra_args)) {
                0.4
              } else {
                extra_args$convex.inner
              }
            
            convex.outer <- 
              if (!exists("convex.outer", where = extra_args)) {
                -0.7
              } else {
                extra_args$convex.outer
              }
            
            max.edge <-   
              if (!exists("max.edge", where = extra_args)) {
                0.7
              } else {
                extra_args$max.edge
              }
            
            mesh <-
              make.mesh(
                data = data,
                convex.inner = convex.inner,
                convex.outer = convex.outer,
                cutoff = cutoff,
                max.edge = max.edge
              )
            plot.mesh(mesh = mesh$mesh,
                      data = data,
                      plot = plot)
            if (!suppress.messages) {
              cat("Calculating fitted values for the GP regression using INLA/SPDE \n")
            }
            if (exists("h.value", where = extra_args)) {
              h.value <- extra_args$h.value
            }
            else {
              h.value <- 5e-05
            }
            if (exists("robust", where = extra_args)) {
              if (extra_args$robust) {
                family <- "T"
                robust <- TRUE
              }
              else {
                family <- "gaussian"
                robust <- FALSE
              }
            } else {
              family <- "gaussian"
              robust <- FALSE
            }
            if (exists("int.ord", where = extra_args)) {
              int.ord <- extra_args$int.ord[[k]][l]
            }
            else {
              int.ord <- 1
            }
            fit <- fit.inla(
              parameter = param_idx,
              inputs = inputs,
              x = x,
              mesh = mesh$mesh,
              data.scale = data,
              int.ord = int.ord,
              convex.inner = convex.inner,
              convex.outer = convex.outer,
              cutoff = cutoff,
              max.edge = max.edge,
              h.value = h.value,
              family = family
            )
          }
        }
        fit.full[[k]][,l] <- fit$fitted
        
        # calculate time taken
        if (method == "INLA") {
          time. <- c(projections$time, mesh$time, fit$time)
          time. <- sum(time.)
          time[[k]][l] <- (time.)
        } else {
          time. <- fit$time
          time[[k]][l] <- time.
        }
      }
    }
    if (!suppress.messages) cat("Calculating EVPPI \n")
    
    comp <- compute.evppi(he = he, fit.full = fit.full)
    
    name <- prepare.output(parameters = params, inputs = inputs)
    
    time[[3]] <- comp$time
    names(time) <- c("Fitting for Effects",
                     "Fitting for Costs",
                     "Calculating EVPPI")
    names(extra_args$method) <- c("Methods for Effects", "Methods for Costs")
    
    if (residuals) {
      res <- list(
        evppi = comp$EVPPI,
        index = params,
        k = he$k,
        evi = he$evi,
        parameters = name,
        time = time,
        method = extra_args$method,
        fitted.costs = fit.full[[2]],
        fitted.effects = fit.full[[1]],
        select = extra_args$select)
    } else {
      res <- list(
        evppi = comp$EVPPI,
        index = params,
        k = he$k,
        evi = he$evi,
        parameters = name,
        time = time,
        method = extra_args$method)
    }
  }
  
  structure(res, class = "evppi")
}


#' @rdname evppi
#' @export
#'
evppi.default <- function(he, ...) {
  stop("No method available", call. = FALSE)
}

