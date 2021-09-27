
library(R2jags)
library(R2WinBUGS)
library(dplyr)
library(rstan)
library(MCMCvis)

##TODO: why doesn't this MC forward sampling work?
# cat("model {
#      eff ~ dunif(0, 1)
#      cost ~ dnorm(0, 1)
#     }", file = "ce_mock.txt")

## ... a work-around
cat("model {
     cost ~ dunif(0, 1)
     eff ~ dunif(0, 1)
     
     x ~ dnorm(cost, 1)
     y ~ dnorm(eff, 1)
    }", file = "ce_mock.txt")

writeLines(
    "data {
        real x;
        real y;
    }
    
    parameters {
        real cost;
        real eff;
    }
    
    model {
     cost ~ uniform(0, 1);
     eff ~ uniform(0, 1);
     
     x ~ normal(cost, 1);
     y ~ normal(eff, 1);
    }
    ", con = "ce_mock.stan")

model.params <- c("cost", "eff")

jagsfit <- jags(data = list(x = 1, y = 1),
                parameters.to.save = model.params,
                n.iter = 10,
                model.file = "ce_mock.txt")

bugsfit <- openbugs(data = list(x = 1, y = 1),
                    parameters.to.save = model.params,
                    n.iter = 10,
                    n.chains = 1,
                    inits = list(list(cost = 0, eff = 0)),
                    model.file = "ce_mock.txt")

stanfit <- stan(data = list(x = 1, y = 1),
                pars = stan.params,
                iter = 10,
                init = 0,
                file = "ce_mock.stan")

# as.mcmc(jagsfit) %>% 
# as.matrix()
# 
# attach.jags(jagsfit)

