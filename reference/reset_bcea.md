# Resets a modified `bcea` object

Useful to return to a standard `bcea` object, after having applied the
setters for `mixedAn` or `CEriskav`

## Usage

``` r
reset_bcea(he)
```

## Arguments

- he:

  A modified `bcea` object, which now is also in the class `mixedAn`, or
  `CEriskav` or both. returns The original `bcea` object, stripped of
  extra elements (created by `mixedAn` or `CEriskav` or both), so that
  the original methods apply

## References

Baio G, Dawid AP (2011). “Probabilistic sensitivity analysis in health
economics.” *Stat. Methods Med. Res.*, 1–20. ISSN 1477-0334,
[doi:10.1177/0962280211419832](https://doi.org/10.1177/0962280211419832)
, <https://pubmed.ncbi.nlm.nih.gov/21930515/>.

Baio G (2013). *Bayesian Methods in Health Economics*. CRC.

## See also

[`bcea()`](https://n8thangreen.github.io/BCEA/reference/bcea.md),
[`mixedAn()`](https://n8thangreen.github.io/BCEA/reference/mixedAn-set.md),
[`CEriskav()`](https://n8thangreen.github.io/BCEA/reference/CEriskav_assign.md)

## Author

Gianluca Baio

## Examples

``` r
data(Vaccine)
# Creates the "standard" BCEA object
m <- bcea(eff, cost, ref=2, interventions=treats)
class(m)
#> [1] "bcea" "list"

# Applies analysis for risk aversion
CEriskav(m) <- c(0.001, 0.002, 0.003)
# Now m has changed nature
class(m)
#> [1] "CEriskav" "bcea"     "list"    
# The default method for plot will be the specialised `CEriskav`, but can 
# also specifically request a method, like
# plot.bcea(m)

# More simply, we can revert to the original object
m <- reset_bcea(m)
class(m)
#> [1] "bcea" "list"
```
