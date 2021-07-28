
# expected incremental benefit plot

# library(BCEA)


data("Smoking")

he <- bcea(e, c, ref = 4, Kmax = 500)

test_that("comparison groups base R", {
  
  expect_silent(eib.plot(he, comparison = 2))
  expect_silent(eib.plot(he, comparison = c(2,3)))
  
  expect_error(eib.plot(he, comparison = 4),
               "Can't select Reference group. Change Reference first.")
  
  expect_error(eib.plot(he, comparison = c(1,4)),
               "Can't select Reference group. Change Reference first.")
  
  expect_error(eib.plot(he, comparison = 0),
               "Comparison index not in available comparisons.")
  
  expect_error(eib.plot(he, comparison = c(1,0)),
               "Comparison index not in available comparisons.")
})

