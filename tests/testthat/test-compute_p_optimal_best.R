#

test_that("compute_p_optimal_best", {
  
  # Mock data
  he <- list(
    comp = 1:2, # Comparator interventions
    n_comparators = 3,
    n_sim = 4,
    ref = 3,    # Reference intervention
    k = c(1, 2), # Willingness-to-pay thresholds
    U = array(c(10, 12, 8,
                15, 10, 9,
                11, 13, 7,
                14, 11, 6),
              dim = c(4, 2, 3)) # Dimensions: sim x k x interventions
  )
  
  result <- compute_p_optimal_best(he)

  best1 <- which.max(c(mean(he$U[,1,1]), mean(he$U[,1,2])))
  p_k1 <- mean(max.col(he$U[,1,], ties.method = "first") == best1)

  best2 <- which.max(c(mean(he$U[,2,1]), mean(he$U[,2,2])))
  p_k2 <- mean(max.col(he$U[,2,], ties.method = "first") == best2)
  
  expect_equivalent(result, c(p_k1, p_k2))
})
