
data(Vaccine)

test_that("setKmax", {
  
  m <- bcea(e = e,
            c = c,
            ref = 2,
            interventions = treats,
            Kmax = 50000,
            plot = FALSE)
  
  setKmax(m) <- 20000
  
  expect_equal(m$Kmax, 20000)
  expect_equal(max(m$k), 20000)
  expect_equal(m$step, 40)
})


test_that("setReferenceGroup", {
  
  m <- bcea(e = e,
            c = c,
            ref = 2,
            interventions = treats,
            Kmax = 50000,
            plot = FALSE)
  
  setReferenceGroup(m) <- 1
  
  expect_equal(m$ref, 1)
  expect_equal(m$comp, 2)  
})