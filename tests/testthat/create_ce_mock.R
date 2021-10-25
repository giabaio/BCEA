
# create jags, BUGS and Stan mocked
# cost and eff posterior samples
# to test bcea() methods


##TODO: why doesn't this MC forward sampling work?
# cat("model {
#      eff ~ dunif(0, 1)
#      cost ~ dnorm(0, 1)
#     }", file = "ce_mock.txt")

## ... a work-around
cat("model {
    for (i in 1:2) {
    
     cost[i] ~ dunif(0, 1)
     eff[i] ~ dunif(0, 1)
     
     x[i] ~ dnorm(cost[i], 1)
     y[i] ~ dnorm(eff[i], 1)
    }
    }", file = "ce_mock.txt")

writeLines(
    "data {
        real x[2];
        real y[2];
    }
    
    parameters {
        real cost[2];
        real eff[2];
    }
    
    model {
    for (i in 1:2) {
     cost[i] ~ uniform(0, 1);
     eff[i] ~ uniform(0, 1);
     
     x[i] ~ normal(cost[i], 1);
     y[i] ~ normal(eff[i], 1);
    }
  }
    ", con = "ce_mock.stan")

model.params <- c("cost", "eff")
model.data <- list(x = c(1,1), y = c(1,1))

jagsfit <- jags(data = model.data,
                parameters.to.save = model.params,
                n.iter = 10,
                n.chains = 1,
                model.file = "ce_mock.txt")

bugsfit <- openbugs(data = model.data,
                    parameters.to.save = model.params,
                    n.iter = 10,
                    n.chains = 1,
                    inits = list(list(cost = 0, eff = 0)),
                    model.file = "ce_mock.txt")

stanfit <- stan(data = model.data,
                pars = model.params,
                chains = 1,
                iter = 10,
                init = 0,
                file = "ce_mock.stan")

save(jagsfit, file = test_path("data", "jagsfit.RData"))
save(bugsfit, file = test_path("data", "bugsfit.RData"))
save(stanfit, file = test_path("data", "stanfit.RData"))

