
# Package: ldr
# Type: Package
# Title: Methods for likelihood-based dimension reduction in regression
# Version: 1.3.3
# Date: 2014-06-06
# Author: Kofi Placid Adragni, Andrew Raim
# Maintainer: Kofi Placid Adragni <kofi@umbc.edu>
# Description: Functions, methods, and data sets for fitting likelihood-based dimension reduction in regression,
# using principal fitted components (pfc), likelihood acquired directions (lad), covariance reducing models (core).
# URL: https://www.jstatsoft.org/v61/i03/
#   License: GPL (>= 2)
# Packaged: 2021-10-08 16:32:42 UTC; Nathan
# Repository: https://github.com/cran/ldr
# Date/Publication: 2014-10-29 16:36:14
#
# Covariance Reduction
#
# Method to reduce sample covariance matrices to an informational core that is sufficient to
# characterize the variance heterogeneity among different populations.
#
#' @importFrom stats cov
#' @importFrom utils tail
#' 
core <- function(X, y, Sigmas=NULL, ns=NULL, numdir=2, numdir.test=FALSE, ...) {
  mf <- match.call()
  
  if (!is.null(Sigmas))
  {
    if (is.null(ns)) stop("Number of observations per class must be provided")
    
    p <- dim(Sigmas[[1]])[1]; hlevels <- length(Sigmas)
  }
  
  if (is.null(Sigmas) & is.null(ns))
  {
    if (is.factor(y)) y <- as.integer(y)
    
    hlevels <- length(unique(y)); p <- ncol(X)
    
    Sigmas <-list(); ns <- vector(length=hlevels)
    
    for (h in 1:hlevels) 
    {
      ns[h] <- sum(y==h)
      
      Sigmas[[h]] <- cov(as.matrix(X[y==h,], ncol=p))
    }
  } 
  
  n <- sum(ns); ff <- ns/n; d <- numdir
  
  if (!numdir.test)
  {
    fit <- one.core(d, p, hlevels, ff, Sigmas, ns, ...)
    
    ans <- list(Gammahat=fit$Gammahat, Sigmahat = fit$Sigmahat, Sigmashat = fit$Sigmashat, 
                loglik=fit$loglik, aic=fit$aic, bic=fit$bic, numpar=fit$numpar, numdir=d, 
                model="core", call=match.call(expand.dots = TRUE), numdir.test=numdir.test)
    
    class(ans) <- "core"
    
    return(invisible(ans))
  }
  aic <- bic <- numpar <- loglik <- vector(length=d+1)
  
  Gammahat <- Sigmahat <- Sigmashat <- list()
  
  loglik <- numpar <- aic <- bic <- numeric(d+1)
  
  for (i in 0:d)
  {
    if (!is.null(mf$verbose)) cat("Running CORE for numdir =", i, "\n")
    
    fit <- one.core(i, p, hlevels, ff, Sigmas, ns, ...)
    
    Gammahat[[i+1]] <-fit$Gammahat
    
    Sigmahat[[i+1]] <- fit$Sigmahat
    
    Sigmashat[[i+1]] <- fit$Sigmashat
    
    loglik[i+1] <- fit$loglik
    
    numpar[i+1] <- fit$numpar
    
    aic[i+1] <- fit$aic
    
    bic[i+1] <- fit$bic
  }
  ans <- list(Gammahat=Gammahat, Sigmahat = Sigmahat, Sigmashat = Sigmashat,
              loglik=loglik, aic=aic, bic=bic, numpar=numpar, numdir=d, model="core",
              call=match.call(expand.dots = TRUE), numdir.test=numdir.test)
  
  class(ans) <- "core"
  
  return(invisible(ans))
}


#' 
one.core <- function(d, p, hlevels, ff, Sigmas, ns, ...) {
  if (d == 0)
  {
    Gamma.hat <- NULL
    
    Sigma.hat <- matrix(0, p, p)
    
    for (g in 1:hlevels) {Sigma.hat <- Sigma.hat + ff[g] * Sigmas[[g]]}
    
    term0 <- 0
    
    term1 <- n/2 * log(det(Sigma.hat))
    
    loglik <- term0 - term1
  }
  else if (d == p)
  {
    Gamma.hat <- diag(p)
    
    Sigma.hat <- matrix(0, p, p)
    
    for (g in 1:hlevels){Sigma.hat <- Sigma.hat + ff[g] * Sigmas[[g]]}
    
    term0 <- 0
    
    term1 <- n/2 * log(det(Sigma.hat))
    
    term2 <- n/2 * log(det(Sigma.hat))
    
    term3 <- 0
  
    for (g in 1:hlevels){term3 <- term3 + ns[g]/2 * log(det(Sigmas[[g]]))}
    
    loglik <- Re(term0 - term1 + term2 - term3)
  }
  else 
  {
    objfun <- function(W)
    {
      Q <- W$Qt;	d <- W$dim[1]; p <- W$dim[2]
      
      Sigmas <- W$Sigmas
      
      n <- sum(W$ns)
      
      U <- matrix(Q[,1:d], ncol=d)
      
      V <- matrix(Q[,(d+1):p], ncol=(p-d))
      
      Sigma.hat <- matrix(0, p, p)
      
      for (g in 1:hlevels){Sigma.hat <- Sigma.hat + ff[g] * Sigmas[[g]]}
      
      Ps <- projection(U, diag(p))
      
      # Objective function
      
      term0 <- 0
      
      term1 <- n/2 * log(det(Sigma.hat))
      
      term2 <- n/2 * log(det(t(U) %*% Sigma.hat %*% U))
      
      term3 <- 0
      
      for (g in 1:hlevels){term3 <- term3 + ns[g]/2 * log(det(t(U) %*% Sigmas[[g]] %*% U))}
      
      value <- Re(term0 - term1 + term2 - term3)
      
      return(list(value=value))
    }
    objfun <- assign("objfun", objfun, envir=.BaseNamespaceEnv)
    
    W <- list(dim=c(d, p), Sigmas=Sigmas, ns=ns)
    
    grassmann <- GrassmannOptim(objfun, W, ...)
    
    Gamma.hat <- matrix(grassmann$Qt[,1:d], ncol = d)
    
    loglik <- tail(grassmann$fvalues, n = 1)
  }
  
  Sigma.hat <- matrix(0, p, p)
  
  for (g in 1:hlevels){Sigma.hat <- Sigma.hat + ff[g] * Sigmas[[g]]}
  
  if (d != 0){Ps.hat <- projection(Gamma.hat, Sigma.hat)}
  
  Sigmas.hat <- list()
  
  for (g in 1:hlevels)
  {
    if (d == 0)
    {
      Sigmas.hat[[g]] <- Sigma.hat
    }
    else
    {
      Sigmas.hat[[g]] <- Sigma.hat + t(Ps.hat) %*% ( Sigmas[[g]] - Sigma.hat) %*% Ps.hat
    }
  }
  numpar <- p*(p+1)/2 + d*(p-d) + (hlevels-1)*d*(d+1)/2
  
  aic <- -2*loglik + 2 * numpar
  
  bic <- -2*loglik + log(n) * numpar
  
  return(list(Gammahat=Gamma.hat, Sigmahat = Sigma.hat, Sigmashat = Sigmas.hat,
              loglik=loglik, numpar=numpar, aic=aic, bic=bic))
}


#
projection <- function(alpha, Sigma) {
  return(alpha %*% solve(t(alpha) %*% Sigma %*% alpha) %*% 
           t(alpha) %*% Sigma)
}

