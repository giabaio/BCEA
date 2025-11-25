# Compute Incremental Benefit

Sample of incremental net monetary benefit for each willingness-to-pay
threshold, \\k\\, and comparator.

## Usage

``` r
compute_IB(df_ce, k)
```

## Arguments

- df_ce:

  Dataframe of cost and effectiveness deltas

- k:

  Vector of willingness to pay values

## Value

Array with dimensions (k x sim x ints)

## Details

Defined as:

\$\$IB = u(e,c; 1) - u(e,c; 0).\$\$

If the net benefit function is used as utility function, the definition
can be re-written as

\$\$IB = k\cdot\Delta_e - \Delta_c.\$\$

## See also

[`compute_EIB()`](https://n8thangreen.github.io/BCEA/reference/compute_EIB.md)
