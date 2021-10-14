data {
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
    
