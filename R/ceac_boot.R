#' 
#' #' IB Bootstrapping
#' #' 
#' #' @template args-he
#' #' @param k Willingness to pay
#' #' @param R Number of samples
#' #' 
#' ib_boot <- function (he, k, R) {
#'   
#'   k_idx <- which(he$k == k)
#'   ci <- vector(mode = "numeric", length = R)
#'   
#'   for (i in seq_len(R)) {
#'     
#'     idx <- sample(1:he$n_sim, replace = TRUE)
#'     s <- he$ib[k_idx, , ][idx] > 0
#'     ci[i] <- sum(s)/he$n_sim
#'   }
#'   ci
#' }
#' 
#' 
#' #' CEAC Bootstrapping
#' #' 
#' #' @template args-he
#' #' @param R Number of samples
#' #' 
#' #' @importFrom stats quantile
#' #' 
#' #' @examples 
#' #' res <- ceac_boot(he, R)
#' #'
#' #' plot(res[, 1] , type = "l")
#' #' lines(res[, 2] , type = "l")
#' #'
#' ceac_boot <- function(he, R) {
#'   
#'   ci <- matrix(, nrow = length(he$k), ncol = 2)
#'   
#'   for (i in seq_along(he$k)) {
#'     
#'     ib <- ib_boot(he, k = he$k[i], R)
#'     
#'     ci[i, ] <- quantile(ib, probs = c(0.025, 0.975))
#'   }
#'   ci
#' }
#' 
#' 
