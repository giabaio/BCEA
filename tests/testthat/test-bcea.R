
# library(BCEA)

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


test_that("basic input values", {
  
  # dat <- data.frame(eff = c(0,0),
  #                   c = c(1,2))
  # 
  # bcea(dat$eff, dat$c)
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
  
  expect_length(res$delta_c, nrow(cost))
  expect_length(res$delta_e, nrow(eff))
  
  expect_equal(nrow(res$U), nrow(eff))
  expect_equal(nrow(res$vi), nrow(eff))
  expect_equal(nrow(res$Ustar), nrow(eff))
  expect_equal(nrow(res$e), nrow(eff))
  expect_equal(nrow(res$c), nrow(cost))
})
