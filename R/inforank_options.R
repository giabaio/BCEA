
#
inforank_options <- function(he,
                             inp,
                             wtp,
                             rel,
                             extra_args) {
  
  parameter <- inp$parameters
  input <- inp$mat
  
  if (is.null(wtp)) wtp <- he$k[min(which(he$k >= he$ICER))]
  
  if (is.character(parameter[1])) {
    parameters <- array()
    for (i in seq_along(parameter)) {
      parameters[i] <- which(colnames(input) == parameter[i])
    }
  } else {
    parameters <- parameter
  }
  
  parameter <- colnames(input)[parameters]
  
  # needs to exclude parameters with weird behaviour (ie all 0s)
  w <-
    unlist(lapply(parameter,
                  function(x) which(colnames(input) == x)))
  
  if (length(w) == 1) return()
  
  input <- input[, w]
  chk1 <- which(apply(input, 2, "var") > 0)   # only takes those with var > 0
  
  # check those with < 5 possible values (would break GAM)
  tmp <- lapply(1:dim(input)[2], function(x) table(input[, x]))
  chk2 <- which(unlist(lapply(tmp, function(x) length(x) >= 5)) == TRUE)
  names(chk2) <- colnames(input[, chk2])
  
  # can do the analysis on a smaller number of PSA runs
  if (exists("N", where = extra_args)) {
    N <- extra_args$N
  } else {
    N <- he$n_sim
  }
  
  if (any(!is.na(N)) & length(N) > 1) {
    select <- N
  } else {
    N <- min(he$n_sim,N, na.rm = TRUE)
    
    select <- 
      if (N == he$n_sim) {
        1:he$n_sim
      } else {
        sample(1:he$n_sim, size = N, replace = FALSE)} 
  }
  
  m <- he
  m$k <- wtp
  x <- list()
  
  for (i in seq_along(chk2)) {
    x[[i]] <-
      quiet(
        evppi(he = m,
              param_idx = chk2[i],
              input = input,
              N = N))
  }
  
  scores <- 
    if (rel) {
      unlist(lapply(x,
                    function(x) x$evppi/x$evi[which(he$k == wtp)]))
    } else {
      unlist(lapply(x,
                    function(x) x$evppi))
    }
  
  list(rel = rel,
       wtp = wtp,
       x = x,
       chk2 = chk2,
       scores = scores)
}

