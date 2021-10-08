
## retains ordering?
## names outputs?


# library(BCEA)
library(dplyr)
library(reshape2)


load("ce.RData")


test_that("input errors", {
  
  expect_dims_error <- function(x, y) {
    expect_error(
      bcea(x, y,
           plot = FALSE),
      regexp = "eff and cost are not the same dimensions.")
  }
  
  expect_dims_error(eff, cost[c(1,2,1), ])
  expect_dims_error(eff, cost[, c(1,2,1)])
  expect_dims_error(eff[c(1,2,1), ], cost)
  expect_dims_error(eff[, c(1,2,1)], cost)
  
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
               c("n_sim", "n_comparators", "n_comparisons", "delta_e", "delta_c",
                 "ICER", "Kmax", "k", "ceac", "ib", "eib", "kstar", "best", "U", "vi",
                 "Ustar", "ol", "evi", "ref", "comp", "step", "interventions", "e", "c"))
  
  expect_equal(res$n_sim, nrow(cost))
  
  expect_n_sim_equal <- function (object)
    expect_equal(NROW(object), nrow(cost))
  
  expect_k_equal <- function (object)
    expect_equal(NROW(object), length(res$k))
  
  expect_n_sim_equal(res$delta_c)
  expect_n_sim_equal(res$delta_e)
  expect_n_sim_equal(res$U)
  expect_n_sim_equal(res$vi)
  expect_n_sim_equal(res$Ustar)
  expect_n_sim_equal(res$e)
  expect_n_sim_equal(res$c)
  
  expect_k_equal(res$ce)
  expect_k_equal(res$eib)
  expect_k_equal(res$evi)
  expect_k_equal(res$best)
  expect_k_equal(res$ib)
  
  ##TODO: should we swap rows and columns to match other variables?
  expect_k_equal(t(res$vi))
  expect_k_equal(t(res$Ustar))
  expect_k_equal(t(res$ol))
})


test_that("ib", {
  
  # single wtp
  
  c_tmp <- matrix(c(0, 0, 100, 10), nrow = 2)
  e_tmp <- matrix(c(0, 0, 1,   -2), nrow = 2)
  
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
  K <- length(k)
  
  res <- 
    bcea(e = e_tmp,
         c = c_tmp, wtp = k)
  
  ib_1 <- k*delta_e[1] - delta_c[1] # 95, 10*(-1) - (-100) = 90
  ib_2 <- k*delta_e[2] - delta_c[2] # 20, 10*2 - (-10) = 30
  
  expect_equivalent(cbind(ib_1, ib_2), drop(res$ib))
  
  
  # multiple comparisons
  
  c_tmp <- matrix(c(0, 0, 100, 10,  0,  1), nrow = 2)
  e_tmp <- matrix(c(0, 0,   1, -2, -3, -4), nrow = 2)
  n_comparisons <- 2
  
  res <- 
    bcea(e = e_tmp,
         c = c_tmp, wtp = k)
  
  # sim x comprison
  delta_e <- matrix(c(-1, 3,
                      2, 4), nrow = 2, byrow = TRUE)
  delta_c <- matrix(c(-100,  0,
                      -10,  -1), nrow = 2,  byrow = TRUE)
  
  ib_11 <- k*delta_e[1, 1] - delta_c[1, 1] # 15 30
  ib_12 <- k*delta_e[1, 2] - delta_c[1, 2] # 15 30
  ib_21 <- k*delta_e[2, 1] - delta_c[2, 1] # 15 30
  ib_22 <- k*delta_e[2, 2] - delta_c[2, 2] # 21 41
  
  expect_equivalent(cbind(ib_11, ib_21), res$ib[, , 1])
  expect_equivalent(cbind(ib_12, ib_22), res$ib[, , 2])
})


library(rstan)
library(R2WinBUGS)
library(R2jags)

test_that("jags, bugs, stan methods", {

  ##TODO: missing cost error
  # bcea(jagsfit)
  
  # mocked inputs
  load("data/bugsfit.RData")
  load("data/jagsfit.RData")
  load("data/stanfit.RData")
  
  expect_s3_class(bcea.rjags(jagsfit), class = "bcea")
  expect_s3_class(bcea.bugs(bugsfit), class = "bcea")
  expect_s3_class(bcea.rstan(stanfit), class = "bcea")
})

