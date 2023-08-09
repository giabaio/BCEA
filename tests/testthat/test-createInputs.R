

test_that("createInputs bad arguments", {

  data(Vaccine, package = "BCEA")
  treats <- c("Status quo", "Vaccination")
  
  m <- bcea(e.pts, c.pts, ref = 2, interventions = treats)
  
  dat <- vaccine_mat[, 1:5]
  datNA <-  cbind(dat, NA)
  
  expect_equal(createInputs(dat), createInputs(datNA))
  expect_message(createInputs(datNA), regexp = "Dropped any columns containing NAs")
})

