# Compute Value of Information

The difference between the maximum utility computed for the current
parameter configuration \\U^\*\\ and the utility of the intervention
which is associated with the maximum utility overall.

## Usage

``` r
compute_vi(Ustar, U)
```

## Arguments

- Ustar:

  Maximum utility value (sim x k)

- U:

  Net monetary benefit (sim x k x interv)

## Value

Array with dimensions (sim x k)

## Details

The value of obtaining additional information on the parameter
\\\theta\\ to reduce the uncertainty in the decisional process. It is
defined as:

\$\$\textrm{VI}(\theta) := U^\*(\theta) - \mathcal{U}^\*\$\$

with \\U^\*(\theta)\\ the maximum utility value for the given simulation
among all comparators and \\\mathcal{U}^\*(\theta)\\ the expected
utility gained by the adoption of the cost-effective intervention.

## See also

[`compute_ol()`](https://n8thangreen.github.io/BCEA/reference/compute_ol.md)
