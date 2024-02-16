
# opportunity loss

# library(dplyr)
# library(reshape2)



test_that("simple data input parameters", {
  
  load(test_path("testdata", "ce.RData"))
  
  # only one intervention
  # limiting case

  U <- array(c(1,1,1,
               1,1,1),
             dim = c(3,2,1))     # sim, k, ints
  
  Ustar <- array(c(1,1,1,
                   1,1,1),
                 dim = c(3,2))   # sim, k
  
  # no loss i.e all optimal
  best <- c(1,1)                 # k
  
  expect_equal(
    compute_ol(Ustar,
               U,
               best),
    array(c(0,0,0,
            0,0,0),
          dim = c(3,2)))
  
  U <- array(c(1,1,1,1,1,1,
               0,0,0,0,0,0),
             dim = c(3,2,2))
  
  Ustar <- array(c(1,1,1,
                   1,1,1),
                 dim = c(3,2))
  
  # no loss i.e all optimal
  best <- c(1,1)
  
  expect_equal(
    compute_ol(Ustar,
               U,
               best),
    array(c(0,0,0,
            0,0,0),
          dim = c(3,2)))
  
  # all loss
  best <- c(2,2)
  
  expect_equal(
    compute_ol(Ustar,
               U,
               best),
    array(c(1,1,1,
            1,1,1),
          dim = c(3,2)))
  
})

test_that("call via bcea", {
  
  load(test_path("testdata", "ce.RData"))
  
  res <- 
    bcea(e = eff,
         c = cost)
  
  ol <- res$ol
  n_sim <- nrow(cost)
  
  expect_equal(dim(ol), c(n_sim, length(res$k)))
  
  expect_true(all(ol >= 0))
})

test_that("errors in dimensions", {
  
  U <- array(c(1,1,1,1,1,1,
               0,0,0,0,0,0),
             dim = c(3,2,2))
  
  Ustar <- array(c(1,1,1,
                   1,1,1),
                 dim = c(3,2))
  
  # too long
  best <- c(1,1,1)
  
  expect_error(
    compute_ol(Ustar,
               U,
               best))
  # too short
  best <- 1
  
  expect_error(
    compute_ol(Ustar,
               U,
               best))
    
  # k out of bounds
  best <- c(0,1)
  
  expect_error(
    compute_ol(Ustar,
               U,
               best))

  best <- c(1,3)
  
  expect_error(
    compute_ol(Ustar,
               U,
               best))
})
