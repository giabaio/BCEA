# evppi() helper functions ------------------------------------------------


#' Prepare Delta arrays
#' 
#' @template args-he 
#' @param seq_rows Rows of (e,c) to keep
#' @param k e or c? 1 or 2.
#' @param l Columns of (e,c) to keep
#' @seealso \code{\link{evppi}}
#' @keywords internal
#' 
prep.x <- function(he,
                   seq_rows,
                   k,
                   l){
  if (k == 1) {
    x <- as.matrix(he$delta_e)[seq_rows, l]
  }
  if (k == 2) {
    x <- as.matrix(he$delta_c)[seq_rows, l]
  }
  return(x)
}


#' Gaussian Additive Model Fitting
#' 
#' @param parameter Parameter
#' @param inputs Inputs
#' @param x Response variable
#' @param form Formula
#' 
#' @return List
#' 
#' @importFrom stats update
#' @seealso \code{\link{evppi}}
#' 
fit.gam <- function(parameter,
                    inputs,
                    x,
                    form) {
  tic <- proc.time()
  model <- mgcv::gam(update(formula(x ~ .),
                            formula(paste(".~", form))),
                     data = data.frame(inputs))
  hat <- model$fitted
  
  ##TODO: should this be used instead of hat?
  # N <- nrow(inputs)
  # p <- length(parameter)
  # fitted <- matrix(hat, nrow = N, ncol = p)
  
  formula <- form
  toc <- proc.time() - tic
  time <- toc[3]
  names(time) <- "Time to fit GAM regression (seconds)"
  
  list(fitted = hat,
       formula = formula,
       fit = model,
       time = time)
}

#' Gaussian Process Fitting
#' 
#' @param hyperparams Hyperparameters
#' @param parameter Parameters
#' @param x Response variable
#' @param input.matrix Input data matrix
#' 
#' @importFrom stats dist dnorm
#' @seealso \code{\link{evppi}}
#' 
post.density <- function(hyperparams,
                         parameter,
                         x,
                         input.matrix) {
  
  dinvgamma <- function(x, alpha, beta) {
    (beta^alpha)/gamma(alpha) *
      x^(-alpha - 1) *
      exp(-beta/x)
  }
  
  N <- length(x)
  p <- length(parameter)
  H <- cbind(1, input.matrix)
  q <- ncol(H)
  a.sigma <- 0.001
  b.sigma <- 0.001
  a.nu <- 0.001
  b.nu <- 1
  delta <- exp(hyperparams)[1:p]
  nu <- exp(hyperparams)[p + 1]
  A <- exp(-(as.matrix(dist(t(t(input.matrix)/delta),
                            upper = TRUE, diag = TRUE))^2))
  Astar <- A + nu * diag(N)
  chol_Astar <- chol(Astar)
  y <- backsolve(t(chol_Astar),(x), upper.tri = FALSE)
  x. <- backsolve(t(chol_Astar), H, upper.tri = FALSE)
  tHAstarinvH <- t(x.) %*% (x.)
  betahat <- solve(tHAstarinvH) %*% t(x.) %*% y
  residSS <- y %*% y - t(y) %*% x. %*% betahat - t(betahat) %*%
    t(x.) %*% y + t(betahat) %*% tHAstarinvH %*% betahat
  prior <- prod(dnorm(log(delta), 0, sqrt(1e+05))) *
    dinvgamma(nu, a.nu, b.nu)
  l <- -sum(log(diag(chol_Astar))) - 1/2 * log(det(tHAstarinvH)) -
    (N - q + 2 * a.sigma)/2 * log(residSS/2 + b.sigma) + log(prior)
  names(l) <- NULL
  
  return(l)
}


#' Estimate hyperparameters
#' 
#' @param x x
#' @param input.matrix Input matrix
#' @param parameter Parameters
#' @param n.sim Number of simulations 
#' 
#' @importFrom stats optim
#' @seealso \code{\link{evppi}}
#' @keywords internal
#' 
estimate.hyperparams <- function(x,
                                 input.matrix,
                                 parameter,
                                 n.sim) {
  p <- length(parameter)
  initial.values <- rep(0, p + 1)
  repeat {
    log.hyperparams <-
      optim(initial.values,
            fn = post.density,
            parameter = parameter,
            x = x[1:n.sim],
            input.matrix = input.matrix[1:n.sim, ],
            method = "Nelder-Mead",
            control = list(fnscale = -1,
                           maxit = 10000, trace = 0))$par
    
    if (sum(abs(initial.values - log.hyperparams)) < 0.01) {
      hyperparams <- exp(log.hyperparams)
      break
    }
    initial.values <- log.hyperparams
  }
  
  hyperparams
}

#' Fit Gaussian Process
#' 
#' @param parameter Parameters
#' @param inputs Inputs
#' @param x Response variable
#' @param n.sim Number of simulations 
#' @return list
#' @importFrom stats dist
#' @seealso \code{\link{evppi}}
#' 
fit.gp <- function(parameter,
                   inputs,
                   x,
                   n.sim) {
  tic <- proc.time()
  p <- length(parameter)
  input.matrix <- as.matrix(inputs[, parameter, drop = FALSE])
  colmin <- apply(input.matrix, 2, min)
  colmax <- apply(input.matrix, 2, max)
  colrange <- colmax - colmin
  input.matrix <- sweep(input.matrix, 2, colmin, "-")
  input.matrix <- sweep(input.matrix, 2, colrange, "/")
  N <- nrow(input.matrix)
  H <- cbind(1, input.matrix)
  q <- ncol(H)
  
  hyperparams <-
    estimate.hyperparams(
      x = x,
      input.matrix = input.matrix,
      parameter = parameter,
      n.sim = n.sim)
  
  delta.hat <- hyperparams[1:p]
  nu.hat <- hyperparams[p + 1]
  A <- exp(-(as.matrix(dist(t(t(input.matrix)/delta.hat),
                            upper = TRUE, diag = TRUE))^2))
  Astar <- A + nu.hat * diag(N)
  Astarinv <- chol2inv(chol(Astar))
  
  rm(Astar)
  gc()
  
  AstarinvY <- Astarinv %*% x
  tHAstarinv <- t(H) %*% Astarinv
  tHAHinv <- solve(tHAstarinv %*% H)
  betahat <- tHAHinv %*% (tHAstarinv %*% x)
  Hbetahat <- H %*% betahat
  resid <- x - Hbetahat
  fitted <- Hbetahat + A %*% (Astarinv %*% resid)
  
  ##TODO: should this be used somewhere?
  # AAstarinvH <- A %*% t(tHAstarinv)
  
  sigmasqhat <-
    as.numeric(t(resid) %*% Astarinv %*% resid)/(N - q - 2)
  
  rm(A, Astarinv, AstarinvY, tHAstarinv, tHAHinv,
     Hbetahat, resid, sigmasqhat)
  gc()
  
  toc <- proc.time() - tic
  time <- toc[3]
  names(time) <- "Time to fit GP regression (seconds)"
  
  list(fitted = fitted,
       time = time,
       fit = NULL,
       formula = NULL)
}


#' INLA Fitting
#'
#' @param parameter Parameter
#' @param inputs Inputs
#' @param x Response variable
#' @param k k
#' @param l l
#' @return list
#' @seealso \code{\link{evppi}}
#'
make.proj <- function(parameter,
                      inputs,
                      x,
                      k,
                      l) {
  tic <- proc.time()
  scale <- 8 / (range(x)[2] - range(x)[1])
  scale.x <- scale * x - mean(scale * x)
  
  # generate basic function
  bx <- bf(scale.x, case = "poly", 2)
  
  # principle fitted components
  fit1 <-
    pfc(scale(inputs[, parameter]), scale.x, bx, structure = "iso")
  fit2 <-
    pfc(scale(inputs[, parameter]), scale.x, bx, structure = "aniso")
  fit3 <-
    pfc(scale(inputs[, parameter]), scale.x, bx, structure = "unstr")
  
  struc <-
    c("iso", "aniso", "unstr")[
      which(c(fit1$aic, fit2$aic, fit3$aic) == min(fit1$aic, fit2$aic, fit3$aic))]
  AIC.deg <- array()
  
  for (i in 2:7) {
    bx <- bf(scale.x, case = "poly", i)
    fit <-
      pfc(scale(inputs[, parameter]), scale.x, bx, structure = struc)
    AIC.deg[i] <- fit$aic
  }
  
  deg <- which(AIC.deg == min(AIC.deg, na.rm = TRUE))
  d <- min(dim(inputs[, parameter])[2], deg)
  
  ##TODO: where should this be used?
  # by <- bf(scale.x, case = "poly", deg)
  
  # likelihood-based dimension reduction
  comp.d <-
    ldr(
      scale(inputs[, parameter]),
      scale.x,
      bx,
      structure = struc,
      model = "pfc",
      numdir = d,
      numdir.test = TRUE)
  
  dim.d <- which(comp.d$aic == min(comp.d$aic)) - 1
  
  comp <-
    ldr(
      scale(inputs[, parameter]),
      scale.x,
      bx,
      structure = struc,
      model = "pfc",
      numdir = 2)
  
  toc <- proc.time() - tic
  time <- toc[3]
  
  if (dim.d > 2){
    cur <- c("effects", "costs")
    warning(
      paste(
        "The dimension of the sufficient reduction for the incremental",
        cur[k], ", column", l, ", is", dim.d,
        ".Dimensions greater than 2 imply that the EVPPI approximation using INLA may be inaccurate.
        Full residual checking using diag.evppi is required."))
  }
  names(time) <- "Time to fit find projections (seconds)"
  
  list(data = comp$R,
       time = time,
       dim = dim.d)
}


#' Mesh Plot
#' 
#' Option of interactively saving the plot.
#' 
#' @param mesh Mesh
#' @param data Data
#' @param plot Create plot? logical
#'
#' @importFrom utils select.list
#' @importFrom grDevices dev.off
#' @seealso \code{\link{evppi}}
#' 
plot.mesh <- function(mesh, data, plot) {
  
  if (!plot) return()
  
  cat("\n")
  choice <- select.list(c("yes", "no"),
                        title = "Would you like to save the graph?",
                        graphics = FALSE)
  if (choice == "yes") {
    exts <- c("jpeg", "pdf", "bmp", "png", "tiff")
    ext <- select.list(exts,
                       title = "Please select file extension",
                       graphics = FALSE)
    name <- paste0(getwd(), "/mesh.", ext)
    txt <- paste0(ext, "('", name, "')")
    eval(parse(text = txt))
    txt <- paste0("Graph saved as: ", name)
    cat(txt)
    on.exit(dev.off())
  }
  
  cat("\n")
  plot(mesh)
  points(data,
         col = "blue",
         pch = 19,
         cex = 0.8)
}


#' Make Mesh
#' 
#' Fit using INLA methods.
#' 
#' @param data Data
#' @param convex.inner convex.inner 
#' @param convex.outer convex.outer 
#' @param cutoff Cut-off value
#' @param max.edge Maximum edge 
#' @return list
#' @seealso \code{\link{evppi}}
#' 
make.mesh <- function(data,
                      convex.inner,
                      convex.outer,
                      cutoff,
                      max.edge) {
  tic <- proc.time()
  inner <- suppressMessages({
    INLA::inla.nonconvex.hull(data, convex = convex.inner)
  })
  outer <- INLA::inla.nonconvex.hull(data, convex = convex.outer)
  mesh <-
    INLA::inla.mesh.2d(
      loc = data,
      boundary = list(inner, outer),
      max.edge = c(max.edge, max.edge),
      cutoff = c(cutoff))
  toc <- proc.time() - tic
  time <- toc[3]
  names(time) <- "Time to fit determine the mesh (seconds)"
  
  list(mesh = mesh,
       pts = data,
       time = time)
}


#' Fit INLA
#' 
#' @param parameter Parameters
#' @param inputs Inputs
#' @param x Response variable
#' @param mesh Mesh 
#' @param data.scale data.scale 
#' @param int.ord int.ord 
#' @param convex.inner convex.inner 
#' @param convex.outer convex.outer 
#' @param cutoff Cut-off 
#' @param max.edge Maximum edge 
#' @param h.value h.value 
#' @param family family 
#' @return list
#' @importFrom stats as.formula
#' @seealso \code{\link{evppi}}
#' 
fit.inla <- function(parameter,
                     inputs,
                     x,
                     mesh,
                     data.scale,
                     int.ord,
                     convex.inner,
                     convex.outer,
                     cutoff,
                     max.edge,
                     h.value,
                     family) {
  tic <- proc.time()
  inputs.scale <-
    scale(inputs, apply(inputs, 2, mean), apply(inputs, 2, sd))
  scale <- 8 / (range(x)[2] - range(x)[1])
  scale.x <- scale * x - mean(scale * x)
  A <-
    INLA::inla.spde.make.A(mesh = mesh,
                           loc = data.scale,
                           silent = 2L)
  spde <-
    INLA::inla.spde2.matern(
      mesh = mesh,
      alpha = 2)
  stk.real <-
    INLA::inla.stack(
      tag = "est",
      data = list(y = scale.x),
      A = list(A, 1),
      effects = list(s = 1:spde$n.spde,
                     data.frame(
                       b0 = 1, x = cbind(data.scale, inputs.scale)
                     ))
    )
  data <- INLA::inla.stack.data(stk.real)
  ctr.pred <- INLA::inla.stack.A(stk.real)
  
  inp <- names(stk.real$effects$data)[parameter + 4]
  form <- paste(inp, "+", sep = "", collapse = "")
  formula <- paste("y~0+(",
                   form,
                   "+0)+b0+f(s,model=spde)",
                   sep = "",
                   collapse = "")
  if (int.ord[1] > 1) {
    formula <- paste(
      "y~0+(",
      form,
      "+0)^",
      int.ord[1],
      "+b0+f(s,model=spde)",
      sep = "",
      collapse = "")
  }
  Result <- suppressMessages({
    INLA::inla(
      as.formula(formula),
      data = data,
      family = family,
      control.predictor = list(A = ctr.pred, link = 1),
      control.inla = list(h = h.value),
      control.compute = list(config = TRUE)
    )
  })
  fitted <-
    (Result$summary.linear.predictor[1:length(x), "mean"] + mean(scale * x)) / scale
  fit <- Result
  toc <- proc.time() - tic
  time <- toc[3]
  names(time) <- "Time to fit INLA/SPDE (seconds)"
  
  list(
    fitted = fitted,
    model = fit,
    time = time,
    formula = formula,
    mesh = list(mesh = mesh, pts = data.scale))
}


#' Compute EVPPI
#' @template args-he
#' @param fit.full fit.full
#' @return list
#' @seealso \code{\link{evppi}}
#'
compute.evppi <- function(he, fit.full) {
  EVPPI <- array()
  tic <- proc.time()
  for (i in seq_along(he$k)) {
    NB.k <- -(he$k[i]*fit.full[[1]] - fit.full[[2]])
    EVPPI[i] <- (mean(apply(NB.k, 1, max, na.rm = TRUE)) -
                   max(apply(NB.k, 2, mean, na.rm = TRUE)))
  }
  toc <- proc.time() - tic
  time <- toc[3]
  names(time) <- "Time to compute the EVPPI (in seconds)"
  
  list(EVPPI = EVPPI,
       time = time)
}


#' Prepare output
#' 
#' @param parameters Parameters
#' @param inputs Inputs
#' @return name
#' @seealso \code{\link{evppi}}
#'
prepare.output <- function(parameters,
                           inputs) {
  
  if (length(parameters) == 1) {
    
    name <- 
      if (is.numeric(parameters)) {
        colnames(inputs)[parameters]
      } else {
        parameters}
    
  } else {
    if (is.numeric(parameters)) {
      n_params <- length(parameters)
      end <- colnames(inputs)[parameters[n_params]]
      name.mid <-
        paste(colnames(inputs)[parameters[1:n_params - 1]],
              ", ",
              sep = "",
              collapse = " ")
      name <- paste(name.mid, "and ", end, sep = "",
                    collapse = " ")
    } else {
      n_params <- length(parameters)
      end <- parameters[n_params]
      name.mid <- paste(parameters[1:n_params - 1], ", ",
                        sep = "",
                        collapse = " ")
      name <- paste(name.mid, "and ", end,
                    sep = "",
                    collapse = " ")
    }
  }
  
  name
}

