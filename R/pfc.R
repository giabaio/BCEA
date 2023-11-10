
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
# Principal fitted components
# 
# Principal fitted components model for sufficient dimension reduction.
# This function estimates all parameters in the model.
# 
#' @importFrom stats cov
#' 

pfc <- function(X,
                y,
                fy = NULL,
                numdir
                = NULL,
                structure = c("iso", "aniso", "unstr", "unstr2"),
                eps_aniso = 1e-3,
                numdir.test = FALSE,
                ...) {
  
  "%^%"<-function(M, pow) 
  { 
    if (prod(dim(M)==list(1,1))) return( as.matrix(M^pow) )
    eigenM = eigen(M) 
    return(eigenM$vectors%*%diag(c(eigenM$values)^pow)%*%t(eigenM$vectors))  
  }
  
  Trace<-function(X)
  {	
    if (!is.matrix(X)) stop("Argument to Trace is not a matrix in pfc")
    return(sum(diag(X))) 
  }
  
  if (is.null(fy)) {fy <- scale(y, TRUE, TRUE); numdir <- 1}
  r <- dim(fy)[2]
  X <- as.matrix(X)
  op <- dim(X)
  n <- op[1]
  p <- op[2] 
  eff.numdir <- min(numdir, r, p)	
  
  vnames <- dimnames(X)[[2]]
  if (is.null(vnames)) vnames <- paste("X", 1:p, sep="")
  if (p==1) return(onepfc(X=X, y=y, fy=fy, p, numdir.test))
  
  Muhat <- apply(X, 2, mean)
  Xc <-  scale(X, TRUE, FALSE)
  
  P_F <- fy%*%solve(t(fy)%*%fy)%*%t(fy)
  
  Sigmahat <- cov(X)
  Sigmahat_fit <- cov(P_F%*%X)
  Sigmahat_res <- Sigmahat - Sigmahat_fit
  
  if (structure=="iso")
  {
    iso <- function(i)
    {
      ev <- eigen(Sigmahat)
      ev.fit <- eigen(Sigmahat_fit)
      all_evalues <-ev.fit$values
      evalues <- all_evalues[1:i]
      sigma2hat <- Re(sum(ev$values)/p)
      
      Gammahat <- Re(matrix(ev.fit$vectors[,1:i], ncol=i))
      dimnames(Gammahat) <- list(vnames, paste("Dir", 1:i, sep=""))
      Betahat <-Re(t(Gammahat)%*%t(Xc)%*%fy%*%solve(t(fy)%*%fy))
      sigma2hat <- Re((sum(ev$values)-sum(evalues))/p)
      Deltahat <- sigma2hat*diag(1, p)
      dimnames(Deltahat) <- list(vnames, vnames)
      
      loglik <- - 0.5*n*p*(1+log(2*pi*sigma2hat))
      numpar <- p + (p-i)*i + i*dim(fy)[2] + 1
      aic <- -2*loglik + 2*numpar
      bic <- -2*loglik + log(n)*numpar
      
      return(list(Betahat=Betahat, Gammahat=Gammahat, Deltahat=Deltahat, 
                  evalues=evalues, loglik=loglik, aic=aic, bic=bic, numpar=numpar))
    }
    
    if (identical(numdir.test, FALSE))
    {
      out <- iso(eff.numdir)
      
      ans <- list(R=X%*%orthonorm(out$Gammahat), Muhat=Muhat, Betahat=out$Betahat, Gammahat=out$Gammahat, 
                  Deltahat=out$Deltahat, loglik=out$loglik, aic=out$aic, bic=out$bic, numpar=out$numpar, 
                  numdir=eff.numdir, evalues=out$evalues, structure="iso", y=y, fy=fy,  
                  Xc=Xc, call=match.call(expand.dots=TRUE), numdir.test=numdir.test)
      
      class(ans) <- "pfc"
      return(ans)
    }
    
    if (identical(numdir.test, TRUE))
    {
      aic <- bic <- numpar <- loglik <- vector(length=eff.numdir+1)
      Betahat <- Deltahat <- Gammahat <-vector("list")
      
      # No fitting values (eff.numdir=0)
      ev <- eigen(Sigmahat)
      sigma2hat <- sum(ev$values)/p
      loglik[1] <- - 0.5*n*p*(1+log(2*pi*sigma2hat))
      numpar[1] <- p + 1
      aic[1] <- -2*loglik[1] + 2*numpar[1]
      bic[1] <- -2*loglik[1] + log(n)*numpar[1]
      
      for (i in 1:eff.numdir)
      {
        fit <- iso(i)
        Betahat[[i]] <-fit$Betahat
        Gammahat[[i]] <-fit$Gammahat
        Deltahat[[i]] <- fit$Deltahat
        loglik[i+1] <- fit$loglik
        numpar[i+1] <- fit$numpar
        aic[i+1] <- fit$aic
        bic[i+1] <- fit$bic
      }
      ans <- list(R=X%*%orthonorm(Gammahat[[eff.numdir]]), Muhat=Muhat, Betahat=Betahat, Gammahat=Gammahat, 
                  Deltahat=Deltahat, loglik=loglik, aic=aic, bic=bic, numpar=numpar, 
                  numdir=eff.numdir, model="pfc", evalues=fit$evalues, structure="iso", 
                  y=y, fy=fy,  Xc=Xc, call=match.call(), numdir.test=numdir.test)
      
      class(ans)<- "pfc"
      return(ans)
    } 
  }
  
  if (structure=="aniso")
  {
    aniso = function(X, y, fy, d, eps_aniso=1e-3, numdir.test)
    {
      vnames <- dimnames(X)[[2]]
      if (is.null(vnames)) vnames <- paste("X", seq_len(ncol(X)), sep="")
      op <- dim(X)
      n <- op[1]
      p <- op[2]
      
      # Initial Step
      fit <- pfc(X=X, y=y, fy=fy, numdir=d, structure="iso", numdir.test=numdir.test)
      
      if (identical(numdir.test, FALSE))
      {
        Betahatx <- fit$Betahat
        Gammahatx <- fit$Gammahat
        Xc <- scale(X, TRUE, FALSE) - fy%*%t(Gammahatx%*%Betahatx)
        deltahat <- diag(cov(Xc))
        
        repeat
        {
          Xnew = X%*%((1/sqrt(deltahat))*diag(p))
          fit <- pfc(X=Xnew, y=y, fy=fy, numdir=d, structure="iso", numdir.test=FALSE)
          Betahatx <- fit$Betahat
          Gammahatx <- (diag(p)*sqrt(deltahat))%*%fit$Gammahat 
          Xc <- scale(X, TRUE, FALSE) - fy%*%t(Gammahatx%*%Betahatx)
          deltahat0 <- diag(t(Xc)%*%(Xc)/n)
          if (sum(abs(deltahat-deltahat0)) < eps_aniso) break
          deltahat <- deltahat0 
        }
        dimnames(Gammahatx) <- list(vnames, paste("Dir", 1:d, sep=""))
        Deltahat <- deltahat*diag(p)
        dimnames(Deltahat) <- list(vnames, vnames)
        
        loglik <- - 0.5*n*p*(1+log(2*pi)) - 0.5*n*log(prod(deltahat))
        numpar <- p + d*(p-d) + ncol(fy)*d + p 
        aic <- -2*loglik + 2*numpar
        bic <- -2*loglik + log(n)*numpar
        
        ans <- list(Betahat=Betahatx, Gammahat=orthonorm(Gammahatx), Deltahat=Deltahat, evalues=fit$evalues, 
                    loglik=loglik, aic=aic, bic=bic, numpar=numpar, numdir.test=numdir.test)
        
        return(ans)
      }
      
      Deltahat <- Betahat <- Gammahat <- vector("list")
      aic <- bic <- numpar <- loglik <- vector(length=eff.numdir + 1)
      
      # No fitting values (eff.numdir=0)
      ev <- eigen(Sigmahat)
      loglik[1] <- - 0.5*n*p*(1+log(2*pi)) - 0.5*n*log(prod(ev$values))
      numpar[1] <- p + p
      aic[1] <- -2*loglik[1] + 2*numpar[1]
      bic[1] <- -2*loglik[1] + log(n)*numpar[1]
      
      for (i in 1:eff.numdir)
      {
        Betahatx <- fit$Betahat[[i]]
        Gammahatx <- fit$Gammahat[[i]] 
        Xc <- scale(X, TRUE, FALSE) - fy%*%t(Gammahatx%*%Betahatx)
        deltahat <- diag(t(Xc)%*%(Xc)/n)
        
        repeat
        {
          Xnew = X%*%((1/sqrt(deltahat))*diag(p))
          fit2 <- pfc(X=Xnew, y=y, fy=fy, numdir=i, structure="iso", numdir.test=FALSE)
          Betahatx <- fit2$Betahat
          Gammahatx <- (diag(p)*sqrt(deltahat))%*%fit2$Gammahat 
          Xc <- scale(X, TRUE, FALSE) - fy%*%t(Gammahatx%*%Betahatx)
          deltahat0 <- diag(t(Xc)%*%(Xc)/n)
          if (sum(abs(deltahat-deltahat0)) < eps_aniso) break
          deltahat <- deltahat0 
        }
        
        Deltahat[[i]] <- deltahat*diag(p)
        dimnames(Deltahat[[i]]) <- list(vnames, vnames)
        
        loglik[i+1] = - 0.5*n*p*(1+log(2*pi)) - 0.5*n*log(prod(deltahat))
        numpar[i+1] <- p + (p-i)*i + i*dim(fy)[2] + p
        aic[i+1] <- -2*loglik[i+1] + 2*numpar[i+1]
        bic[i+1] <- -2*loglik[i+1] + log(n)*numpar[i+1]
        Betahat[[i]] <- Betahatx
        Gammahat[[i]] <- orthonorm(Gammahatx)
        dimnames(Gammahat[[i]]) <- list(vnames, paste("Dir", 1:i, sep=""))
      }
      ans <- list(Betahat=Betahat, Gammahat=Gammahat, Deltahat=Deltahat, evalues=fit2$evalues, 
                  loglik=loglik, aic=aic, bic=bic, numpar=numpar, numdir.test=numdir.test)
      
      return(ans)	
    }
    
    fit <- aniso(X=X, y=y, fy=fy, d=eff.numdir, eps_aniso=eps_aniso, numdir.test=numdir.test)
    
    ans <- list(Muhat=Muhat, Betahat=fit$Betahat, Gammahat=fit$Gammahat, Deltahat=fit$Deltahat, model="pfc",
                loglik=fit$loglik, aic=fit$aic, bic=fit$bic, numpar=fit$numpar, numdir=eff.numdir, 
                evalues=fit$evalues, structure="aniso", Xc=Xc, y=y, fy=fy,  call=match.call(), numdir.test=fit$numdir.test)
    
    if (numdir.test==FALSE) ans$R <- X%*%orthonorm(((fit$Deltahat)%^%(-1))%*%fit$Gammahat) else 
      ans$R <- X%*%orthonorm(((fit$Deltahat[[eff.numdir]])%^%(-1))%*%fit$Gammahat[[eff.numdir]])
    
    class(ans)<- "pfc"
    
    return(ans)
  }
  
  if (structure=="unstr")
  {
    unstr<-function(i)
    {
      sqrt_Sigmahat_res <- Sigmahat_res%^%0.5 
      Inv_Sqrt_Sigmahat_res <- solve(sqrt_Sigmahat_res)
      lf_matrix <- Inv_Sqrt_Sigmahat_res%*%Sigmahat_fit%*%Inv_Sqrt_Sigmahat_res
      all_evalues <- eigen(lf_matrix, symmetric=T)$values
      evalues <- all_evalues[1:i]
      
      Vhat <- eigen(lf_matrix, symmetric=T)$vectors
      Vhati <- matrix(Vhat[,1:i], ncol=i)
      Gammahat <- (Sigmahat_res%^%0.5)%*%Vhati%*%solve((t(Vhati)%*%Sigmahat_res%*%Vhati)%^%0.5)  
      dimnames(Gammahat)<- list(vnames, paste("Dir", 1:i, sep=""))
      
      Khat<-diag(0, p) 
      if (i < min(ncol(fy),p)) {diag(Khat)[(i+1):min(ncol(fy), p )]<- all_evalues[(i+1):min(ncol(fy), p)]}
      Deltahat <- sqrt_Sigmahat_res%*%Vhat%*%(diag(p)+Khat)%*%t(Vhat)%*%sqrt_Sigmahat_res
      dimnames(Deltahat) <- list(vnames, vnames)
      Betahat <- ((t(Vhati)%*%Sigmahat_res%*%Vhati)%^%0.5)%*%t(Vhati)%*%solve(Sigmahat_res%^%0.5)%*%t(Xc)%*%fy%*% solve(t(fy)%*%fy)
      
      temp0 <- -(n*p/2)*(1 + log(2*pi))
      temp1 <- -(n/2)*log(det(Sigmahat_res)) 
      temp2 <- 0
      
      if (i < min(ncol(fy),p)) temp2 <- -(n/2)*sum(log(1 + all_evalues[(i+1):p]))
      loglik <- temp0 + temp1 + temp2
      numpar <- p + (p-i)*i + i*ncol(fy) + p*(p+1)/2
      aic <- -2*loglik + 2*numpar
      bic <- -2*loglik + log(n)*numpar
      
      return(list(Betahat=Betahat, Gammahat=Gammahat, Deltahat=Deltahat, evalues=evalues, 
                  loglik=loglik, aic=aic, bic=bic, numpar=numpar))
    }
    
    if (identical(numdir.test, FALSE))
    {
      out <- unstr(eff.numdir)
      
      ans <- list(R=X%*%orthonorm(solve(out$Deltahat)%*%out$Gammahat), Muhat=Muhat, Betahat=out$Betahat, Gammahat=out$Gammahat, 
                  Deltahat=out$Deltahat, evalues=out$evalues, loglik=out$loglik, aic=out$aic, bic=out$bic, numpar=out$numpar, 
                  numdir=eff.numdir, model="pfc", structure="unstr", y=y, fy=fy, Xc=Xc,  call=match.call(), numdir.test=numdir.test)
      
      class(ans) <- "pfc"	
      return(ans)
    }
    
    aic <- bic <- numpar <- loglik <- vector(length=eff.numdir+1)
    evalues <- vector(length=eff.numdir)
    Betahat <- Deltahat <- Gammahat <-vector("list") 
    
    loglik[1] <- - 0.5*n*p*(1+log(2*pi)) - 0.5*n*log(det(Sigmahat)) 
    numpar[1] <- p + p*(p+1)/2
    aic[1] <- -2*loglik[1] + 2*numpar[1]
    bic[1] <- -2*loglik[1] + log(n)*numpar[1]
    Deltahat[[1]] <- Sigmahat
    dimnames(Deltahat[[1]]) <- list(vnames, vnames) 
    
    for (i in 1:eff.numdir)
    {
      fit <- unstr(i)
      Betahat[[i]] <-fit$Betahat 
      Gammahat[[i]] <-fit$Gammahat 
      Deltahat[[i]] <- fit$Deltahat
      loglik[i+1] <- fit$loglik 
      numpar[i+1] <- fit$numpar
      aic[i+1] <- fit$aic
      bic[i+1] <- fit$bic	
    }
    ans <- list(R=X%*%orthonorm(solve(Deltahat[[eff.numdir]])%*%Gammahat[[eff.numdir]]), Muhat=Muhat, 
                Betahat=Betahat, Gammahat=Gammahat, Deltahat=Deltahat, evalues=fit$evalues, loglik=loglik, 
                aic=aic, bic=bic, numpar=numpar, numdir=eff.numdir, model="pfc", structure="unstr", y=y, 
                fy=fy, Xc=Xc, call=match.call(), numdir.test=numdir.test)
    
    class(ans)<- "pfc"
    return(ans)
  }
  else if (structure=="unstr2")
  {
    unstr2 <- function(i)
    {
      objfun <- function(W)
      {
        Qt <- W$Qt
        dc <- W$dim[1] 
        p <- ncol(Qt)
        S <- W$Sigmas
        U <- matrix(Qt[,1:dc], ncol=dc)	
        V <- matrix(Qt[,(dc+1):p], ncol=(p-dc))
        value <- -(n/2)*(p*log(2*pi)+p+log(det(t(V)%*%S$Sigmahat%*%V))+ log(det(t(U)%*%S$Sigmahat_res%*%U)))
        
        terme1 <- solve(t(U)%*%S$Sigmahat_res%*%U)%*%(t(U)%*%S$Sigmahat_res%*%V)
        terme2 <- (t(U)%*%S$Sigmahat%*%V)%*%solve(t(V)%*%S$Sigmahat%*%V)
        
        gradient <- 2*(terme1 - terme2)
        return(list(value=value, gradient=gradient))
      }
      sigmas <- list(Sigmahat=Sigmahat, Sigmahat_fit=Sigmahat_fit, Sigmahat_res=Sigmahat_res, p=p, n=n)
      
      W <- list(Qt = svd(Sigmahat_fit)$u, dim=c(numdir, p),
                Sigmas=list(Sigmahat=Sigmahat, Sigmahat_fit=Sigmahat_fit, 
                            Sigmahat_res=Sigmahat_res, p=p, n=n))
      
      objfun <- assign("objfun", objfun, envir=.BaseNamespaceEnv) 
      grassoptim <- GrassmannOptim(objfun, W,...)
      
      Gammahat <- matrix(grassoptim$Qt[,1:i], ncol=i, dimnames=list(vnames, paste("Dir", 1:i, sep=""))) 
      
      Gammahat0 <- matrix(grassoptim$Qt[, (i+1):p], ncol=p-i, dimnames=list(vnames, paste("Dir", (i+1):p, sep="")))
      
      Betahat <- t(Gammahat)%*%t(Xc)%*%fy%*%solve(t(fy)%*%fy)
      Omegahat <- t(Gammahat)%*%Sigmahat_res%*%Gammahat
      Omegahat0 <-t(Gammahat0)%*%Sigmahat%*%Gammahat0
      Deltahat <- Gammahat%*%Omegahat%*%t(Gammahat) + Gammahat0%*%Omegahat0%*%t(Gammahat0)
      dimnames(Deltahat) <- list(vnames, vnames)
      
      temp0 <- -(n*p/2)*(1+log(2*pi))
      temp1 <- -(n/2)*log(det(t(Gammahat)%*%Sigmahat_res%*%Gammahat)) 
      temp2 <- -(n/2)*log(det(t(Gammahat0)%*%Sigmahat%*%Gammahat0)) 
      
      loglik <- temp0 + temp1 + temp2 
      numpar <- p + (p-i)*i + i*dim(fy)[2] + i*(i+1)/2 + (p-i)*(p-i+1)/2 
      aic <- -2*loglik + 2*numpar
      bic <- -2*loglik + log(n)*numpar
      ev.fit <- eigen(Sigmahat_fit) 
      evalues <- ev.fit$values[1:i]
      
      return(list(Betahat=Betahat, Gammahat=Gammahat, Gammahat0=Gammahat0, Omegahat=Omegahat, Omegahat0=Omegahat0, 
                  Deltahat=Deltahat, evalues=evalues, loglik=loglik, aic=aic, bic=bic, numpar=numpar))
    }
    
    if (identical(numdir.test, FALSE))
    {
      out <- unstr2(numdir)
      
      ans <- list(R = X%*%out$Gammahat, Muhat=Muhat, Betahat=out$Betahat, Gammahat=out$Gammahat, 
                  Gammahat0=out$Gammahat0, Omegahat=out$Omegahat, Omegahat0=out$Omegahat0, 
                  Deltahat=out$Deltahat, evalues=out$evalues, loglik=out$loglik, aic=out$aic,
                  bic=out$bic, numpar=out$numpar, numdir=numdir, model="pfc", structure="unstr2", 
                  y=y, fy=fy, Xc=Xc,  call=match.call(), numdir.test=numdir.test)
      
      class(ans) <- "pfc"	
      return(ans)
    }
    
    aic <- bic <- numpar <- loglik <- vector(length=numdir+1)
    Betahat <- Deltahat <- Gammahat <- Gammahat0 <- Omegahat <- Omegahat0 <- vector("list") 
    
    loglik[1] <- -(n*p/2)*(log(2*pi) + (1+log(Trace(Sigmahat)/p)))
    numpar[1] <- p + p*(p+1)/2
    aic[1] <- -2*loglik[1] + 2*numpar[1]
    bic[1] <- -2*loglik[1] + log(n)*numpar[1]
    
    for(m in 1:numdir)
    {
      fit <- unstr2(m)
      Betahat[[m]] <-fit$Betahat 
      Gammahat[[m]] <-fit$Gammahat 
      Omegahat[[m]] <- fit$Omegahat 
      Omegahat0[[m]] <- fit$Omegahat0
      Deltahat[[m]] <- fit$Deltahat 
      loglik[m+1] <- fit$loglik 
      numpar[m+1] <- fit$numpar
      aic[m+1] <- fit$aic 
      bic[m+1] <- fit$bic	
    }
    ans <- list(R = X%*%Gammahat[[numdir]], evalues=fit$evalues, loglik =loglik, aic=aic, bic=bic, numdir=numdir, 
                numpar=numpar, Muhat=Muhat, Betahat=Betahat, Gammahat=Gammahat,	
                Gammahat0=Gammahat0, Omegahat=Omegahat, Omegahat0=Omegahat0, 
                Deltahat=Deltahat, model="pfc", structure="unstr2", 
                y=y, fy=fy, Xc=Xc,  call=match.call(), numdir.test=numdir.test)
    
    class(ans)<- "pfc"
    return(ans)
  }
  
}


#' @importFrom stats lm as.formula
#' 
onepfc <- function(X, y, fy, p, numdir.test) {
  # X is univariate predictor
  nobs <- length(X)
  r <- dim(fy)[2]
  
  P_F <- fy %*% solve(t(fy) %*% fy) %*% t(fy)
  Xc <- scale(X, TRUE, FALSE)
  Sigmahat_fit <- (1/nobs)*t(Xc) %*% P_F %*% (Xc)
  ev.fit <- eigen(Sigmahat_fit)
  
  temp.dat <- data.frame(cbind(X, fy))
  xnam <- paste("xx", 1:r, sep="")
  names(temp.dat) <- c("yy", xnam)
  fm.lm <- as.formula( paste("yy ~ ", paste(xnam, collapse= "+")))
  summary.fm <- summary(lm(fm.lm, data=temp.dat))
  
  Betahat <- matrix(summary.fm$coefficients[2:(r+1),1], ncol=r)
  Gammahat <- matrix(1, ncol=1, nrow=1)
  Deltahat <- matrix(summary.fm$sigma^2, ncol=1, nrow=1)
  Muhat <- matrix(summary.fm$coefficients[1,1], ncol=1)
  
  loglik <- - 0.5*n*(1+log(2*pi*summary.fm$sigma^2))
  numpar <- p  + dim(fy)[2] + 1
  aic <- -2*loglik + 2*numpar
  bic <- -2*loglik + log(n)*numpar
  
  ans <- list(R=X, Muhat=Muhat, Betahat=Betahat, Gammahat=Gammahat, Deltahat=Deltahat, 
              loglik=loglik, aic=aic, bic=bic, numpar=numpar, numdir=1, model="pfc", 
              call=match.call(), structure="iso", y=y, fy=fy, Xc=Xc, numdir.test=numdir.test)
  
  class(ans)<- "pfc"
  
  return(ans)
}

