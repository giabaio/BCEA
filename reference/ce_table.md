# Cost-effectiveness summary statistics table

As is commonly shown in a journal paper.

## Usage

``` r
ce_table(he, wtp = 25000, ...)
```

## Arguments

- he:

  A `bcea` object containing the results of the Bayesian modelling and
  the economic evaluation.

- wtp:

  Willingness to pay

- ...:

  Additional parameters

## Examples

``` r
data(Vaccine)

# Runs the health economic evaluation using BCEA
m <- bcea(
      e=eff,
      c=cost,               # defines the variables of 
                            #  effectiveness and cost
      ref=2,                # selects the 2nd row of (e, c) 
                            #  as containing the reference intervention
      interventions=treats, # defines the labels to be associated 
                            #  with each intervention
      Kmax=50000,           # maximum value possible for the willingness 
                            #  to pay threshold; implies that k is chosen 
                            #  in a grid from the interval (0, Kmax)
)
ce_table(m)
#>                  cost          eff  delta.c      delta.e     ICER      INB
#> Vaccination 14.691446 -0.000805370       NA           NA       NA       NA
#> Status Quo   9.655464 -0.001055946 5.035983 0.0002505764 20097.59 1.228428
```
