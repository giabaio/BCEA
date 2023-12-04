
#' GrassmannOptim
#'
#' This function is taken from the GrassmannOptim package
#' by Kofi Placid Adragni and Seongho Wu
#' https://cran.r-project.org/web/packages/GrassmannOptim/index.html
#' 
#' @param objfun objfun
#' @param W W
#' @param sim_anneal sim_anneal
#' @param temp_init temp_init 
#' @param cooling_rate cooling_rate
#' @param max_iter_sa max_iter_sa
#' @param eps_conv eps_conv 
#' @param max_iter max_iter 
#' @param eps_grad eps_grad 
#' @param eps_f eps_f 
#' @param verbose verbose 
#' @importFrom stats rnorm runif
#' @importFrom Matrix Matrix expm
#' @keywords internal 
#' @return List
#'
GrassmannOptim <-
  function (objfun, W, sim_anneal = FALSE, temp_init = 20, cooling_rate = 2, 
            max_iter_sa = 100, eps_conv = 1e-05, max_iter = 100, eps_grad = 1e-05, 
            eps_f = .Machine$double.eps, verbose = FALSE) 
  {
    call <- match.call()
    if ((is.null(W$Qt)) & (is.null(W$dim))) stop("Missing initial values")
    orthonorm <- function (u) 
    {
      if (is.null(u)) return(NULL)
      if (!(is.matrix(u))) u <- as.matrix(u)
      dd <- dim(u)
      n <- dd[1]
      p <-dd[2]
      
      if (prod(abs(La.svd(u)$d) > 1e-08) == 0) stop("collinears vectors in orthonorm")
      if (n < p)
      {
        warning("There are too much vectors to orthonormalize in orthonorm.")
        u <- as.matrix(u[, 1:p])
        n <- p
      }
      v <- u
      if (p > 1)
      {
        for (i in 2:p)
        {
          coef.proj <- c(crossprod(u[, i], v[, 1:(i - 1)]))/diag(crossprod(v[, 1:(i - 1)]))
          v[, i] <- u[, i] - matrix(v[, 1:(i - 1)], nrow = n) %*% matrix(coef.proj, nrow = i - 1)
        }
      }
      coef.proj <- 1/sqrt(diag(crossprod(v)))
      return(t(t(v) * coef.proj))
    }
    
    if (!is.null(W$Qt))
    {
      Qt <- orthonorm(W$Qt)
      p <- nrow(Qt)
      d <- W$dim[1]
    }
    else 
    {
      dimx <- W$dim
      p = dimx[2]
      d <- dimx[1]
      tempQ <- matrix(rnorm(p^2), ncol = p)
      Qt <- Re(eigen(t(tempQ) %*% tempQ)$vectors)
    }
    
    GetA <- function(alpha, p, d) 
    {
      A <- matrix(0, p, p)
      for (i in 1:d) 
      {
        for (j in (d + 1):p) 
        {
          Eij <- matrix(0, p, p)
          Eij[i, j] <- 1
          Eij[j, i] <- -1
          A <- A + alpha[i, j] * Eij
        }
      }
      return(round(A, digits = 4))
    }
    
    getGradient <- function(objfun, W, fvalue, eps_grad) 
    {
      alpha <- objfun(W)$gradient
      if (is.null(alpha)) 
      {
        Qt <- W$Qt
        p <- nrow(Qt)
        d <- W$dim[1]
        alpha <- matrix(0, nrow = d, ncol = (p - d))
        for (i in 1:d) 
        {
          for (j in (d + 1):p) 
          {
            Q_tilde <- Qt
            Q_tilde[, i] <- cos(eps_grad)*Qt[, i]-sin(eps_grad)*Qt[, j]
            W$Qt <- Q_tilde
            f_tilde <- round(objfun(W)$value, digits = 5)
            alpha[i, j - d] <- (f_tilde - fvalue)/eps_grad
          }
        }
      }
      return(alpha)
    }
    
    max_objfun <- function(All_Qt, W) 
    {
      nlength <- length(All_Qt)
      L <- vector(length = nlength)
      d <- W$dim[1]
      for (i in 1:nlength) 
      {
        if (is.na(sum(All_Qt[[i]]))) L[i] <- NA
        else 
        {
          W$Qt <- All_Qt[[i]]
          L[i] <- objfun(W)$value
        }
      }
      L[abs(L) == Inf] = NA
      if (sum(is.na(L)) == nlength) return(list(status = "allNA"))
      index <- min(which(L == max(L, na.rm = TRUE)))
      return(list(Qt = All_Qt[index][[1]], L = round(L[index], digits = 5), index = index, status = "OK"))
    }
    
    if (verbose) cat("Initialization...", "\n")
    
    if (sim_anneal) 
    {
      seq_delta <- runif(1) * exp(seq(-10, 0, by = 2)) %x% c(-1, 1)
      length_seq <- length(seq_delta)
      temperature <- temp_init
      if (verbose) 
      {
        cat("Simulated Annealing...", "This may take a while.")
        cat("\nInitial temperature=", temp_init, "\n")
        cat("Cooling...\n")
        cat("Current temperature:\n")
      }
      while (temperature > 0.1) 
      {
        for (i in 1:max_iter_sa) 
        {
          W$Qt <- Qt
          alpha <- matrix(0, p, p)
          fvalue <- objfun(W)$value
          ws <- matrix(rnorm(d * (p - d)), nrow = d, ncol = (p - d))
          temp_alpha <- getGradient(objfun, W, fvalue, eps_grad) + sqrt(temperature) * ws
          alpha[1:d, (d + 1):p] <- temp_alpha
          matA <- GetA(alpha, p, d)
          candidates_Qt <- vector("list")
          Expms <- lapply(seq_delta, function(delta) matrix(attributes(expm(Matrix(-delta * matA)))$x, nrow = nrow(matA), ncol = ncol(matA)))
          
          for (j in 1:length_seq) 
          {
            candidates_Qt[[j]] <- orthonorm(Qt %*% t(Expms[[j]]))
            if (is.na(det(candidates_Qt[[j]]))) 
              candidates_Qt[[j]] <- NA
          }
          gridmax <- max_objfun(candidates_Qt, W)
          
          if (gridmax$status != "allNA") 
          {
            candidate_fvalue <- gridmax$L
            diff_ratio <- exp((candidate_fvalue - fvalue)/temperature)
            selector <- as.numeric(runif(1) < min(diff_ratio, 1))
            newQt <- gridmax$Qt
            Qt <- selector * newQt + (1 - selector) * Qt
          }
        }
        temperature <- temperature/cooling_rate
        if (verbose) cat(temperature, "\n")
      }
    }
    
    
    norm_grads <- NULL
    W$Qt <- Qt
    fvalue <- objfun(W)$value
    alpha <- matrix(0, p, p)
    temp_alpha <- getGradient(objfun, W, fvalue, eps_grad)
    alpha[1:d, (d + 1):p] <- temp_alpha
    iter = 1
    norm_grad <- sum(diag(t(alpha) %*% alpha))
    if (verbose) 
    {
      cat(sprintf("%s %s %s", "iter", "   loglik", "         gradient"), "\n")
      cat(sprintf("%4.0i\t%1.4e\t%1.4e\t", iter, fvalue, norm_grad), "\n")
    }
    new_fvalue <- fvalue
    fvalues <- fvalue
    norm_grads <- norm_grad
    if ((norm_grad <= eps_conv)) 
    {
      converged = TRUE
      return(invisible(
        list(
          Qt = round(Qt, digits = 4),
          d = d,
          norm_grads = norm_grads,
          fvalues = fvalues,
          converged = converged,
          call = call)
      ))
    }
    
    repeat 
    {
      iter = iter + 1
      matA <- GetA(alpha, p, d)
      candidates_Qt <- vector("list")
      seq_delta <- runif(1) * exp(seq(-10, 0, by = 2)) %x% c(-1, 1)
      length_seq <- length(seq_delta)
      Expms <- lapply(seq_delta, function(delta) matrix(attributes(expm(Matrix(-delta * matA)))$x, nrow = nrow(matA), ncol = ncol(matA)))
      
      for (j in 1:length_seq) 
      {
        candidates_Qt[[j]] <- orthonorm(Qt %*% t(Expms[[j]]))
        if (is.na(det(candidates_Qt[[j]]))) candidates_Qt[[j]] <- NA
      }
      
      gridmax <- max_objfun(candidates_Qt, W)
      candidate_Qt <- gridmax$Qt
      candidate_fvalue <- gridmax$L
      candidate_W <- W
      candidate_W$Qt <- candidate_Qt
      temp_alpha <- getGradient(objfun, candidate_W, candidate_fvalue, eps_grad)
      alpha[1:d, (d + 1):p] <- temp_alpha
      norm_grad <- sum(diag(t(alpha) %*% alpha))
      
      if ((norm_grad <= eps_conv)) 
      {
        if (verbose) 
        {
          cat(sprintf("%4.0i\t%1.4e\t%1.4e\t", iter, candidate_fvalue, norm_grad), "\n")
        }
        converged = TRUE
        break
      }
      
      if ((candidate_fvalue - fvalue) > eps_f) 
      {
        if (verbose) 
        {
          cat(sprintf("%4.0i\t%1.4e\t%1.4e\t", iter, candidate_fvalue, norm_grad), "\n")
        }
        fvalue <- candidate_fvalue
        Qt <- candidate_Qt
        W$Qt <- Qt
        fvalues <- c(fvalues, fvalue)
        norm_grads <- c(norm_grads, norm_grad)
      }
      if (iter >= max_iter) 
      {
        if (verbose) message("Convergence may not have been reached.\nMaximum iterations is reached")
        converged = FALSE
        break
      }
    }
    invisible(
      list(
        Qt = round(Qt, digits = 4),
        d = d,
        norm_grads = norm_grads,
        fvalues = fvalues,
        converged = converged,
        call = call)
    )
  }
