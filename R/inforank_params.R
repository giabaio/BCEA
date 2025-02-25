
#' Prepare Info Rank plot parameters
#' 
#' @template args-he
#' @param inp Inputs
#' @param wtp Willingness to pay
#' @param rel Relative size
#' @param howManyPars How mnay parameters to use?
#' @param extra_args Additional arguments
#' 
#' @importFrom purrr map_int
#' @importFrom dplyr slice arrange desc
#' @keywords internal
#' 
inforank_params <- function(he,
                            inp,
                            wtp = NULL,
                            rel,
                            howManyPars,
                            extra_args) {
  
  parameter <- inp$parameters
  input <- inp$mat
  
  ##TODO: what to do for multiple ICER?
  if (is.null(wtp)) wtp <- he$k[min(which(he$k >= he$ICER[1]))]
  
  if (!wtp %in% he$k)
    stop("wtp must be from values computed in call to bcea().", call. = FALSE)
  
  if (is.character(parameter[1])) {
    parameters <- array()
    for (i in seq_along(parameter)) {
      parameters[i] <- which(colnames(input) == parameter[i])
    }
  } else {
    parameters <- parameter
  }
  
  parameter <- colnames(input)[parameters]
  
  # exclude parameters with weird behaviour (ie all 0s)
  w <-
    map_int(parameter, function(x) which(colnames(input) == x))
  
  if (length(w) == 1) return()
  
  input <- input[, w]
  
  ##TODO: what does this do? where is this used?
  # chk1 <- which(apply(input, 2, "var") > 0)   # only takes those with var > 0
  
  # check those with < 5 possible values (would break GAM)
  tmp <- lapply(seq_len(dim(input)[2]), function(x) table(input[, x]))
  chk2 <- which(unlist(lapply(tmp, function(x) length(x) >= 5)) == TRUE)
  names(chk2) <- colnames(input[, chk2])
  
  N <- he$n_sim
  
  ##TODO: what does select do?
  if (any(!is.na(N)) && length(N) > 1) {
    # select <- N
  } else {
    N <- min(he$n_sim,N, na.rm = TRUE)
    
    # select <- 
    #   if (N == he$n_sim) {
    #     1:he$n_sim
    #   } else {
    #     sample(1:he$n_sim, size = N, replace = FALSE)} 
  }
  
  m <- he
  m$k <- wtp
  x <- list()
  
  for (i in seq_along(chk2)) {
    x[[i]] <-
      quiet(
        BCEA::evppi(he = m,
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
  
  xlab <- ifelse(rel,
                 "Proportion of total EVPI",
                 "Absolute value of the EVPPI")
  
  tit <- paste0("Info-rank plot for willingness to pay = ", wtp)
  
  xlim <- c(0, range(scores)[2])
  
  res <-
    data.frame(
      parameter = names(chk2),
      info = scores)
  
  res <- dplyr::arrange(res, dplyr::desc(.data$info))
  
  change_num_pars <-
    !is.null(howManyPars) &&
    is.numeric(howManyPars) &&
    howManyPars > 0
  
  if (change_num_pars) {
    howManyPars <- min(howManyPars, nrow(res))
    res <- dplyr::slice(res, 1:howManyPars)
  }
  
  res <- res[order(res$info), ]
  
  modifyList(
    list(res = res,
         scores = scores,
         chk2 = chk2,
         tit = tit,
         xlim = xlim,
         xlab = xlab,
         howManyPars = howManyPars),
    extra_args)
}

