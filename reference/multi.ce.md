# Cost-effectiveness Analysis With Multiple Comparison

Computes and plots the probability that each of the `n_int`
interventions being analysed is the most cost-effective and the
cost-effectiveness acceptability frontier.

## Usage

``` r
# S3 method for class 'bcea'
multi.ce(he)
```

## Arguments

- he:

  A `bcea` object containing the results of the Bayesian modelling and
  the economic evaluation.

## Value

Original `bcea` object (list) of class "pairwise" with additional:

- p_best_interv:

  A matrix including the probability that each intervention is the most
  cost-effective for all values of the willingness to pay parameter

- ceaf:

  A vector containing the cost-effectiveness acceptability frontier

## See also

[`bcea()`](https://n8thangreen.github.io/BCEA/reference/bcea.md),
[`ceaf.plot()`](https://n8thangreen.github.io/BCEA/reference/ceaf.plot.md)

## Author

Gianluca Baio

## Examples

``` r
# See Baio G., Dawid A.P. (2011) for a detailed description of the 
# Bayesian model and economic problem

# Load the processed results of the MCMC simulation model
data(Vaccine)
 
# Runs the health economic evaluation using BCEA

m <- bcea(e=eff, c=cost,    # defines the variables of 
                            #  effectiveness and cost
      ref=2,                # selects the 2nd row of (e,c) 
                            #  as containing the reference intervention
      interventions=treats, # defines the labels to be associated 
                            #  with each intervention
      Kmax=50000,           # maximum value possible for the willingness 
                            #  to pay threshold; implies that k is chosen 
                            #  in a grid from the interval (0,Kmax)
      plot=FALSE            # inhibits graphical output
)

mce <- multi.ce(m)          # uses the results of the economic analysis

ceac.plot(mce)

ceaf.plot(mce)

```
