
# risk aversion

# library(BCEA)
library(dplyr)
library(reshape2)

##TODO:...

load("ce.RData")

test_that("", {
  
  data(Smoking)
  
  treats <- c("No intervention", "Self-help", "Individual counselling", "Group counselling")
  bcea_smoke <- bcea(e, c, ref = 4, interventions = treats, Kmax = 500)
  
  r <- c(0, 0.005, 0.020, 0.035)
  CEriskav(bcea_smoke) <- r
})
