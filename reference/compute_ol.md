# Compute Opportunity Loss

The difference between the maximum utility computed for the current
parameter configuration (e.g. at the current simulation) \\U^\*\\ and
the current utility of the intervention associated with the maximum
utility overall.

## Usage

``` r
compute_ol(Ustar, U, best)
```

## Arguments

- Ustar:

  Maximum utility value (sim x k)

- U:

  Net monetary benefit (sim x k x interv)

- best:

  Best intervention for given willingness-to-pay (k)

## Value

Array with dimensions (sim x k)

## Details

In mathematical notation, \$\$\textrm{OL}(\theta) := U^\*(\theta) -
U(\theta^\tau)\$\$

where \\\tau\\ is the intervention associated with the overall maximum
utility and \\U^\*(\theta)\\ is the maximum utility value among the
comparators in the given simulation. The opportunity loss is a
non-negative quantity, since \\U(\theta^\tau)\leq U^\*(\theta)\\.

In all simulations where the intervention is more cost-effective (i.e.
when incremental benefit is positive), then \\\textrm{OL}(\theta) = 0\\
as there would be no opportunity loss, if the parameter configuration
were the one obtained in the current simulation.

## See also

[`compute_vi()`](https://n8thangreen.github.io/BCEA/reference/compute_vi.md)
