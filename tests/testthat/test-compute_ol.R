

test_that("compute_ol", {
  
  n_sim <- 3
  K <- 2
  
  U <- array(c(1,1,1,1,1,1,
               0,0,0,0,0,0),
             dim = c(3,2,2))     # sim, k, ints
  
  Ustar <- array(c(1,1,1,
                   1,1,1),
                 dim = c(3,2))   # sim, k
  
  # no loss i.e all optimal
  best <- c(1,1)
  
  expect_equal(
    compute_ol(n_sim,
               K,
               Ustar,
               U,
               best),
    array(c(0,0,0,
            0,0,0), dim = c(3,2)))
  
  # all loss
  best <- c(2,2)
  
  expect_equal(
    compute_ol(n_sim,
               K,
               Ustar,
               U,
               best),
    array(c(1,1,1,
            1,1,1), dim = c(3,2)))
  
})
