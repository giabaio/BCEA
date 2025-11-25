# Create Inputs for EVPI Calculation

Creates an object containing the matrix with the parameters simulated
using the MCMC procedure (using JAGS, BUGS or Stan) and a vector of
parameters (strings) that can be used to perform the expected value of
partial information analysis. In the process, `createInputs` also checks
for linear dependency among columns of the PSA samples or columns having
constant values and removes them to only leave the fundamental
parameters (to run VoI analysis). This also deals with simulations
stored in a `.csv` or `.txt` file (e.g. as obtained using bootstrapping
from a non-Bayesian model).

## Usage

``` r
# Default S3 method
createInputs(inputs, print_is_linear_comb = TRUE)

createInputs(inputs, print_is_linear_comb = TRUE)

# S3 method for class 'rjags'
createInputs(inputs, print_is_linear_comb = TRUE)

# S3 method for class 'bugs'
createInputs(inputs, print_is_linear_comb = TRUE)

# S3 method for class 'stanfit'
createInputs(inputs, print_is_linear_comb = TRUE)

# S3 method for class 'data.frame'
createInputs(inputs, print_is_linear_comb = TRUE)

# S3 method for class 'numeric'
createInputs(inputs, print_is_linear_comb = TRUE)
```

## Arguments

- inputs:

  An object containing suitable simulations for the model parameters. It
  can be a `rjags` or `bugs` object (obtained directly by running
  `R2jags`, `rjags`, `R2OpenBUGS` or `R2WinBUGS`). For those objects,
  which are in the class `rjags` or `bugs`, the user can simply pass
  them to `createInputs()`, which will now how to process them. If the
  user has run their model using `rstan`, then they should pre-process
  the output using the `rstan::extract()` function; so if `rstan` is
  used to run a Bayesian model and save the results to, say, the object
  `fit`, then the user should first create another object
  `tmp=rstan::extract(fit) |> as.data.frame()` and then pass this onto
  `createInputs()`. Finally, this function handles inputs from
  data-frames of vectors directly.

- print_is_linear_comb:

  Logical indicator. If set to `TRUE` (default) then prints the output
  of the procedure trying to assess whether there are some parameters
  that are a linear combination of others (in which case they are
  removed).

## Value

- mat:

  Data.frame containing all the simulations for all the monitored
  parameters

- parameters:

  Character vectors of the names of all the monitored parameters

## See also

[`bcea()`](https://n8thangreen.github.io/BCEA/reference/bcea.md),
[`evppi()`](https://n8thangreen.github.io/BCEA/reference/evppi.md)

## Author

Gianluca Baio, Anna Heath and Mark Strong
