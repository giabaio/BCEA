
# library(dplyr)

# data(Vaccine)

test_that("setKmax", {

  load(test_path("testdata", "ce.RData"))
  
  m <- bcea(e = eff,
            c = cost,
            ref = 2,
            Kmax = 50000,
            plot = FALSE)
  
  setKmax(m) <- 20000
  
  expect_equal(m$Kmax, 20000)
  expect_equal(max(m$k), 20000)
  expect_equal(m$step, 40)
})


test_that("setReferenceGroup", {
  
  load(test_path("testdata", "ce.RData"))
  
  m <- bcea(e = eff,
            c = cost,
            ref = 2,
            Kmax = 50000,
            plot = FALSE)
  
  setReferenceGroup(m) <- 1
  
  expect_equal(m$ref, 1)
  expect_equal(m$comp, 2)  
})


test_that("setComparison", {
  
  load(test_path("testdata", "ce.RData"))
  
  m_r1 <- bcea(e = eff,
               c = cost,
               Kmax = 50000,
               plot = FALSE)
  
  m_c2 <- bcea(e = eff,
               c = cost,
               .comparison = 2, 
               Kmax = 50000,
               plot = FALSE)

  expect_equivalent(m_r1, m_c2)
  
  m_r2 <- bcea(e = eff,
               c = cost,
               ref = 2,
               Kmax = 50000,
               plot = FALSE)
  
  m_r2c1 <- bcea(e = eff,
                 c = cost,
                 .comparison = 1, 
                 ref = 2,
                 Kmax = 50000,
                 plot = FALSE)
  
  expect_equivalent(m_r2, m_r2c1)
  
  expect_error(
    bcea(e = eff,
         c = cost,
         .comparison = 1, 
         Kmax = 50000,
         plot = FALSE),
    "Can't select Reference group. Change Reference first.")

  expect_error(
    bcea(e = eff,
         c = cost,
         .comparison = 2, 
         ref = 2,
         Kmax = 50000,
         plot = FALSE),
    "Can't select Reference group. Change Reference first.")
})
