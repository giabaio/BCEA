
library(BCEA)

data(Vaccine)

res <- bcea(e = e,
            c = c,    
            ref = 2,  
            interventions = treats,
            Kmax = 50000,
            plot = FALSE)

test_that("inputs", {
  
  expect_error(bcea(e, c[-1, ], plot = FALSE),
               regexp = "e and c are not the same dimensions.")
  
  expect_error(bcea(e, c[, -1, drop=FALSE], plot = FALSE),
               regexp = "e and c are not the same dimensions.")
  
  expect_error(bcea(e[-1, ], c, plot = FALSE),
               regexp = "e and c are not the same dimensions.")
  
  expect_error(bcea(e[, -1, drop=FALSE], c, plot = FALSE),
               regexp = "e and c are not the same dimensions.") 
  
  expect_error(bcea(e, c, interventions = c("aaa"), plot = FALSE),
               regexp = "interventions names wrong length.")
  
})