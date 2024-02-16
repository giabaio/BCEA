
# value of information

# library(dplyr)
# library(reshape2)



test_that("simple data", {
  
  load(test_path("testdata", "ce.RData"))
  
  # only one intervention
  # limiting case
  U <- array(c(1,1,1,1,1,1),
             dim = c(3,2,1))     # sim, k, ints
  
  Ustar <- array(c(1,1,1,
                   1,1,1),
                 dim = c(3,2))   # sim, k
  
  expect_error(
    compute_vi(Ustar, U))
  
  U <- array(c(1,1,1,1,1,1,
               0,0,0,0,0,0),
             dim = c(3,2,2))
  
  Ustar <- array(c(1,1,1,
                   1,1,1),
                 dim = c(3,2))
  
  expect_equal(
    compute_vi(Ustar, U),
    array(c(0,0,0,0,
            0,0,0,0),
          dim = c(3,2)))
  
  U <- array(c(1,1,1,1,1,1,
               2,2,2,2,2,2),
             dim = c(3,2,2))
  
  expect_equal(
    compute_vi(Ustar, U),
    array(c(-1,-1,-1,-1,
            -1,-1,-1,-1),
          dim = c(3,2)))
  
})

test_that("call via bcea", {
  
  load(test_path("testdata", "ce.RData"))
  
  res <- 
    bcea(e = eff,
         c = cost)
  
  vi <- res$vi
  n_sim <- nrow(cost)
  
  expect_equal(dim(vi), c(n_sim, length(res$k)))
})

test_that("errors in dimensions", {
  
  U <- array(c(1,1,1,1,1,1,
               0,0,0,0,0,0),
             dim = c(3,2,2))
  
  Ustar <- array(c(1,1,
                   1,1),
                 dim = c(2,2))
  
  expect_error(
    compute_vi(Ustar, U))
  
  Ustar <- array(c(1,1,1,
                   1,1,1,
                   1,1,1),
                 dim = c(3,3))
  
  expect_error(
    compute_vi(Ustar, U))
})

test_that("using sim_table", {
  
  load(test_path("testdata", "ce.RData"))
  
  res <- 
    bcea(e = eff,
         c = cost)
  
  tab <- sim_table(res)$Table
  
  ol <- tab$`U*` - tab$U2
  vi <- tab$`U*` - mean(tab$U2)
  
  expect_equivalent(apply(tab[,c("OL","VI")], 2, mean),
                    c(mean(vi), mean(ol)))
})
