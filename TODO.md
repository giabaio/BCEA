# TO-DO list

## October 2025

* In `bcea.default.R`, check whether we *need* to keep the methods specific to `bugs`, `rstan` and `rjags`. Do we even **really** need them? And if we can get rid of them, can the dependencies on `rstan`, `R2jags` and `R2OpenBUGS` be simply removed?

* In `CreateInputs.R`, is the idea that the user **must** pass as input a matrix? If so, that's fine (which means we probably do not need to worry about the methods specific to `stan` or `jags/bugs`). BUT: we should explain in the help function that the inputs must come as a reformatting of the original objects and be a matrix?

## September 2025

* Check the code all-round to see where we can find bottlenecks in computation and make things quicker.

* Should we make `ggplot` the default for graphs (instead of base)?
