#

test_that("from function examples", {

  # load sample jags output
  load(testthat::test_path("testdata/statins_base.RData"))
  load(testthat::test_path("testdata/statins_HC.RData"))

  interventions <- c("Atorvastatin", "Fluvastatin",
                     "Lovastatin", "Pravastatin",
                     "Rosuvastatin", "Simvastatin")

  m1 <- bcea(eff = statins_base$sims.list$effect,
             cost = statins_base$sims.list$cost.tot,
             ref = 1,
             interventions = interventions)

  m2 <- bcea(eff = statins_HC$sims.list$effect,
             cost = statins_HC$sims.list$cost.tot,
             ref = 1,
             interventions = interventions)

  models <- list("Base" = statins_base,
                 "Half Cauchy" = statins_HC)

  effects <- list(statins_base$sims.list$effect,
                  statins_HC$sims.list$effect)
  
  costs <- list(statins_base$sims.list$cost.tot,
                statins_HC$sims.list$cost.tot)

  m3 <- struct.psa(models = models,
                   effect = effects,
                   cost = costs,
                   ref = 1,
                   interventions = interventions)

  m3 <- struct.psa(models = models,
                   effect = effects,
                   cost = costs,
                   ref = 1,
                   interventions = interventions,
                   w = c(0.5, 0.5))

  ## all weight on a single model
  
  m3 <- struct.psa(models = models,
                   effect = effects,
                   cost = costs,
                   ref = 1,
                   interventions = interventions,
                   w = c(0, 1))

  # remove additional elements
  m3_bcea <- m3[!names(m3) %in% c("w", "DIC")]
  
  expect_equivalent(m2, m3_bcea)

  m3 <- struct.psa(models = models,
                   effect = effects,
                   cost = costs,
                   ref = 1,
                   interventions = interventions,
                   w = c(1, 0))

  m3_bcea <- m3[!names(m3) %in% c("w", "DIC")]
  
  expect_equivalent(m1, m3_bcea)
  
  ##TODO: why do you need to pass the models when w provided?
  # m3 <- struct.psa(effect = effects,
  #                  cost = costs,
  #                  ref = 1,
  #                  interventions = interventions,
  #                  w = c(0, 1))
  
  
})
