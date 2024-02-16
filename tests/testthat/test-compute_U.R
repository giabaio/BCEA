
# library(dplyr)
# library(reshape2)

##TODO:

test_that("compute_U", {
  
  load(test_path("testdata", "ce.RData"))
  
  # single wtp
  
  # c_tmp <- matrix(c(0, 0, 100, 10), nrow = 2)
  # e_tmp <- matrix(c(0, 0, 1, -2), nrow = 2)
  # 
  # res <- 
  #   bcea(e = e_tmp,
  #        c = c_tmp, k = 5)
  # 
  # k <- 5
  # n_comparisons <- 1
  # delta_e <- c(-1, 2)
  # delta_c <- c(-100, -10) # this actually a saving for intervention
  # n_sim <- 2
  # 
  # ib_1 <- k*delta_e[1] - delta_c[1] # 5*(-1) - (-100) = 95
  # ib_2 <- k*delta_e[2] - delta_c[2] # 5*2 - (-10) = 20
  # 
  # expect_equivalent(c(ib_1, ib_2), res$ib)
  # 
  # 
  # # multiple wtp
  # 
  # k <- c(5, 10)
  # K <- 2
  # 
  # res <- 
  #   bcea(e = e_tmp,
  #        c = c_tmp, k = k)
  # 
  # ib_1 <- k*delta_e[1] - delta_c[1] # 95, 10*(-1) - (-100) = 90
  # ib_2 <- k*delta_e[2] - delta_c[2] # 20, 10*2 - (-10) = 30
  # 
  # expect_equivalent(cbind(ib_1, ib_2), drop(res$ib))
  # 
  # 
  # # multiple comparisons
  # 
  # c_tmp <- matrix(c(0, 0, 100, 10, 0, 1), nrow = 2)
  # e_tmp <- matrix(c(0, 0, 1, -2, -3, -4), nrow = 2)
  # n_comparisons <- 2
  # 
  # res <- 
  #   bcea(e = e_tmp,
  #        c = c_tmp, k = k)
  # 
  # # sim x comprison
  # delta_e <- matrix(c(-1,3,
  #                     2,4), nrow = 2, byrow = TRUE)
  # delta_c <- matrix(c(-100,  0,
  #                     -10, -1), nrow = 2,  byrow = TRUE)
  # 
  # ib_11 <- k*delta_e[1,1] - delta_c[1,1] # 15 30
  # ib_12 <- k*delta_e[1,2] - delta_c[1,2] # 15 30
  # ib_21 <- k*delta_e[2,1] - delta_c[2,1] # 15 30
  # ib_22 <- k*delta_e[2,2] - delta_c[2,2] # 21 41
  # 
  # expect_equivalent(cbind(ib_11, ib_21), res$ib[,,1 ])
  # expect_equivalent(cbind(ib_12, ib_22), res$ib[,,2 ])
})

