#

test_that("best_interv_given_k works with single comparator", {
  eib <- c(10, -5, 20)
  ref <- 1
  comp <- 2
  
  result <- best_interv_given_k(eib, ref, comp)
  
  expect_equal(result, c(1, 2, 1))
})

test_that("best_interv_given_k works with multiple comparators and matrix input", {
  eib <- matrix(c(-10, 5, 20, -15), nrow = 2, byrow = TRUE)
  ref <- 1
  comp <- c(2, 3)
  
  result <- best_interv_given_k(eib, ref, comp)
  
  expect_equal(result, c(2, 3))
})

