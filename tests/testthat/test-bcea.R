
# library(BCEA)
library(dplyr)


load("ce.RData")


test_that("input errors", {
  
  expect_error(
    bcea(eff, cost[c(1,2,1), ],
         plot = FALSE),
    regexp = "eff and cost are not the same dimensions.")
  
  expect_error(
    bcea(
      eff, cost[, c(1,2,1)],
      plot = FALSE),
    regexp = "eff and cost are not the same dimensions.")
  
  expect_error(
    bcea(eff[c(1,2,1), ], cost,
         plot = FALSE),
    regexp = "eff and cost are not the same dimensions.")
  
  expect_error(
    bcea(eff[, c(1,2,1)], cost,
         plot = FALSE),
    regexp = "eff and cost are not the same dimensions.") 
  
  expect_error(
    bcea(eff, cost,
         interventions = c("aaa"),
         plot = FALSE),
    regexp = "interventions names wrong length.")
  
  expect_error(
    bcea(eff, cost,
         ref = 0,
         plot = FALSE),
    regexp = "reference is not in available interventions.")
  
  expect_error(
    bcea(eff, cost,
         ref = 3,
         plot = FALSE),
    regexp = "reference is not in available interventions.")
  
  # expect_error(bcea(e, c, ref = 1.1, plot = FALSE),
  #              regexp = "reference is not in available interventions.")
  
  expect_error(
    bcea(c(0,0), c(1,2),
         plot = FALSE),
    regexp = "eff and cost must be matrices.")
  
  expect_error(
    bcea(matrix(c(0,0)), matrix(c(1,2)),
         plot = FALSE),
    regexp = "Require at least 2 comparators.")
})


# realistic input data

test_that("basic return", {
  
  res <- 
    bcea(e = eff,
         c = cost)
  
  expect_s3_class(res, "bcea")
  expect_type(res, "list")
  
  expect_length(res, 24)
  expect_named(res,
               c("n_sim","n_comparators","n_comparisons","delta_e","delta_c",
                 "ICER","Kmax","k","ceac","ib","eib","kstar","best","U","vi",
                 "Ustar","ol","evi","interventions","ref","comp","step","e","c"))
  
  expect_equal(res$n_sim, nrow(cost))
  
  expect_length(res$delta_c, nrow(cost))
  expect_length(res$delta_e, nrow(eff))
  
  expect_equal(nrow(res$U), nrow(eff))
  expect_equal(nrow(res$vi), nrow(eff))
  expect_equal(nrow(res$Ustar), nrow(eff))
  expect_equal(nrow(res$e), nrow(eff))
  expect_equal(nrow(res$c), nrow(cost))
  
  num_k <- length(res$k)
  
  expect_length(res$ce, num_k) ########
  expect_length(res$eib, num_k) ###
  expect_length(res$evi, num_k) ###
  
  expect_length(res$best, num_k)
  
  ###
  expect_equal(nrow(res$ib), num_k)  ##TODO: should we swap rows and columns to match other variables?

  expect_equal(ncol(res$vi), num_k)
  expect_equal(ncol(res$Ustar), num_k)
  expect_equal(ncol(res$ol), num_k)
  
})


test_that("ib", {
  
  # single wtp
  
  c_tmp <- matrix(c(0, 0, 100, 10), nrow = 2)
  e_tmp <- matrix(c(0, 0, 1, -2), nrow = 2)
  
  res <- 
    bcea(e = e_tmp,
         c = c_tmp, wtp = 5)
  
  k <- 5
  n_comparisons <- 1
  delta_e <- c(-1, 2)
  delta_c <- c(-100, -10) # this actually a saving for intervention
  n_sim <- 2
  
  ib_1 <- k*delta_e[1] - delta_c[1] # 5*(-1) - (-100) = 95
  ib_2 <- k*delta_e[2] - delta_c[2] # 5*2 - (-10) = 20
  
  expect_equivalent(c(ib_1, ib_2), res$ib)
  
  
  # multiple wtp
  
  k <- c(5, 10)
  K <- 2
  
  res <- 
    bcea(e = e_tmp,
         c = c_tmp, wtp = k)
  
  ib_1 <- k*delta_e[1] - delta_c[1] # 95, 10*(-1) - (-100) = 90
  ib_2 <- k*delta_e[2] - delta_c[2] # 20, 10*2 - (-10) = 30
  
  expect_equivalent(cbind(ib_1, ib_2), drop(res$ib))
  
  
  # multiple comparisons
  
  c_tmp <- matrix(c(0, 0, 100, 10, 0, 1), nrow = 2)
  e_tmp <- matrix(c(0, 0, 1, -2, -3, -4), nrow = 2)
  n_comparisons <- 2
  
  res <- 
    bcea(e = e_tmp,
         c = c_tmp, wtp = k)
  
  # sim x comprison
  delta_e <- matrix(c(-1,3,
                      2,4), nrow = 2, byrow = TRUE)
  delta_c <- matrix(c(-100,  0,
                      -10, -1), nrow = 2,  byrow = TRUE)
  
  ib_11 <- k*delta_e[1,1] - delta_c[1,1] # 15 30
  ib_12 <- k*delta_e[1,2] - delta_c[1,2] # 15 30
  ib_21 <- k*delta_e[2,1] - delta_c[2,1] # 15 30
  ib_22 <- k*delta_e[2,2] - delta_c[2,2] # 21 41
  
  expect_equivalent(cbind(ib_11, ib_21), res$ib[,,1 ])
  expect_equivalent(cbind(ib_12, ib_22), res$ib[,,2 ])
})

