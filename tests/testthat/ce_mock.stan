data {
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
    
