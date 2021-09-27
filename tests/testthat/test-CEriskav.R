
# risk aversion

# library(BCEA)
library(dplyr)
library(reshape2)

##TODO:...

data(Smoking, package = "BCEA")

treats <- c("No intervention", "Self-help", "Individual counselling", "Group counselling")
bcea_smoke <- bcea(e, c, ref = 4, interventions = treats, Kmax = 500)

test_that("zero risk aversion is equal to default", {
  
  r <- 0
  CEriskav(bcea_smoke) <- r
  
  expect_equivalent(bcea_smoke$U[,,1], bcea_smoke$Ur[,,1,1], tolerance = 0.0001)
  expect_equivalent(bcea_smoke$U[,,2], bcea_smoke$Ur[,,2,1], tolerance = 0.0001)
  expect_equivalent(bcea_smoke$U[,,3], bcea_smoke$Ur[,,3,1], tolerance = 0.0001)
  expect_equivalent(bcea_smoke$U[,,4], bcea_smoke$Ur[,,4,1], tolerance = 0.0001)
})

