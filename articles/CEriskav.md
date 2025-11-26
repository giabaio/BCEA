# Risk Aversion Analysis

Set-up analysis using smoking cessation data set.

``` r
data(Smoking)

treats <- c("No intervention", "Self-help", "Individual counselling", "Group counselling")
bcea_smoke <- bcea(eff, cost, ref = 4, interventions = treats, Kmax = 500)
```

Run the risk aversion analysis straight away with both the base R and
ggplot2 versions of plots.

``` r
r <- c(0, 0.005, 0.020, 0.035)
CEriskav(bcea_smoke) <- r

plot(bcea_smoke)
```

![](CEriskav_files/figure-html/unnamed-chunk-3-1.png)![](CEriskav_files/figure-html/unnamed-chunk-3-2.png)

``` r
plot(bcea_smoke, graph = "ggplot")
```

![](CEriskav_files/figure-html/unnamed-chunk-3-3.png)![](CEriskav_files/figure-html/unnamed-chunk-3-4.png)

Notice that the first value is asymptotically zero but the function
handles that for us. Previously, you had to use something like 1e-10
which was a bit awkward.

Now we modify the comparison group so that it doesn’t contain 2
(“self-help”) anymore. We still keep the same first comparison though
“no intervention”.

``` r
setComparisons(bcea_smoke) <- c(1,3)
```

If we rerun the analysis we should see that the output is exactly the
same.

``` r
CEriskav(bcea_smoke) <- r

plot(bcea_smoke)
```

![](CEriskav_files/figure-html/unnamed-chunk-5-1.png)![](CEriskav_files/figure-html/unnamed-chunk-5-2.png)

``` r
plot(bcea_smoke, graph = "ggplot")
```

![](CEriskav_files/figure-html/unnamed-chunk-5-3.png)![](CEriskav_files/figure-html/unnamed-chunk-5-4.png)

What happens when we only have one risk adjustment value? Set it to zero
so this should be exactly the same as the baseline `bcea` case.

``` r
r <- 0
CEriskav(bcea_smoke) <- r

plot(bcea_smoke)
```

![](CEriskav_files/figure-html/unnamed-chunk-6-1.png)![](CEriskav_files/figure-html/unnamed-chunk-6-2.png)

``` r

plot(bcea_smoke, graph = "ggplot")
```

![](CEriskav_files/figure-html/unnamed-chunk-6-3.png)![](CEriskav_files/figure-html/unnamed-chunk-6-4.png)

``` r

bcea_smoke0 <- bcea(eff, cost, ref = 4, interventions = treats, Kmax = 500)
eib.plot(bcea_smoke0, comparison = 1)
```

![](CEriskav_files/figure-html/unnamed-chunk-6-5.png)

``` r
evi.plot(bcea_smoke0)
```

![](CEriskav_files/figure-html/unnamed-chunk-6-6.png)

Check that unusual or meaningless values for `r` are handled gracefully.
At present the are just calculated and plotting exactly the same way.
*should we limit values?*

``` r
# negative
r <- -0.005
CEriskav(bcea_smoke) <- r
plot(bcea_smoke)
```

![](CEriskav_files/figure-html/unnamed-chunk-7-1.png)![](CEriskav_files/figure-html/unnamed-chunk-7-2.png)

``` r

# large
r <- 2
CEriskav(bcea_smoke) <- r
plot(bcea_smoke)
```

![](CEriskav_files/figure-html/unnamed-chunk-7-3.png)![](CEriskav_files/figure-html/unnamed-chunk-7-4.png)

If we select a new set of comparison interventions what will happen?
There are specified in a different order and for other plots this makes
very little difference (perhaps changing the line types). However, it is
different for
[`CEriskav()`](https://n8thangreen.github.io/BCEA/reference/CEriskav_assign.md)
because only the first comparison intervention is plotted so changing
the order changes the plot. We can see this by swapping “non
intervention” and “individual counselling” interventions from the
analysis above.

``` r
setComparisons(bcea_smoke) <- c(3,1)
```

``` r
r <- c(0, 0.005, 0.020, 0.035)
CEriskav(bcea_smoke) <- r

plot(bcea_smoke)
```

![](CEriskav_files/figure-html/unnamed-chunk-9-1.png)![](CEriskav_files/figure-html/unnamed-chunk-9-2.png)

``` r
plot(bcea_smoke, graph = "ggplot")
```

![](CEriskav_files/figure-html/unnamed-chunk-9-3.png)![](CEriskav_files/figure-html/unnamed-chunk-9-4.png)

The previous version of
[`CEriskav()`](https://n8thangreen.github.io/BCEA/reference/CEriskav_assign.md)
had a `comparison` argument where you could specify the single
intervention to plot and if this wasn’t set then it defaulted to the
first. It seems neater to separate the definition of the analysis with
the plotting so now if you did want to specify a different comparison
intervention when using
[`CEriskav()`](https://n8thangreen.github.io/BCEA/reference/CEriskav_assign.md)
then you would have to use `setComparison()` first and then call the
plotting function.

Check legend position argument:

``` r
# base R
plot(bcea_smoke, pos = c(1,0))
```

![](CEriskav_files/figure-html/unnamed-chunk-10-1.png)![](CEriskav_files/figure-html/unnamed-chunk-10-2.png)

``` r
plot(bcea_smoke, pos = c(1,1))
```

![](CEriskav_files/figure-html/unnamed-chunk-10-3.png)![](CEriskav_files/figure-html/unnamed-chunk-10-4.png)

``` r

plot(bcea_smoke, pos = TRUE)
```

![](CEriskav_files/figure-html/unnamed-chunk-10-5.png)![](CEriskav_files/figure-html/unnamed-chunk-10-6.png)

``` r
plot(bcea_smoke, pos = FALSE)
```

![](CEriskav_files/figure-html/unnamed-chunk-10-7.png)![](CEriskav_files/figure-html/unnamed-chunk-10-8.png)

``` r

plot(bcea_smoke, pos = "topleft")
```

![](CEriskav_files/figure-html/unnamed-chunk-10-9.png)![](CEriskav_files/figure-html/unnamed-chunk-10-10.png)

``` r
plot(bcea_smoke, pos = "topright")
```

![](CEriskav_files/figure-html/unnamed-chunk-10-11.png)![](CEriskav_files/figure-html/unnamed-chunk-10-12.png)

``` r
plot(bcea_smoke, pos = "bottomleft")
```

![](CEriskav_files/figure-html/unnamed-chunk-10-13.png)![](CEriskav_files/figure-html/unnamed-chunk-10-14.png)

``` r
plot(bcea_smoke, pos = "bottomright")
```

![](CEriskav_files/figure-html/unnamed-chunk-10-15.png)![](CEriskav_files/figure-html/unnamed-chunk-10-16.png)

``` r

# ggplot2
plot(bcea_smoke, graph = "ggplot", pos = c(1,0))
```

![](CEriskav_files/figure-html/unnamed-chunk-10-17.png)![](CEriskav_files/figure-html/unnamed-chunk-10-18.png)

``` r
plot(bcea_smoke, graph = "ggplot", pos = c(1,1))
```

![](CEriskav_files/figure-html/unnamed-chunk-10-19.png)![](CEriskav_files/figure-html/unnamed-chunk-10-20.png)

``` r

plot(bcea_smoke, graph = "ggplot", pos = TRUE)
```

![](CEriskav_files/figure-html/unnamed-chunk-10-21.png)![](CEriskav_files/figure-html/unnamed-chunk-10-22.png)

``` r
plot(bcea_smoke, graph = "ggplot", pos = FALSE)
```

![](CEriskav_files/figure-html/unnamed-chunk-10-23.png)![](CEriskav_files/figure-html/unnamed-chunk-10-24.png)

``` r

plot(bcea_smoke, graph = "ggplot", pos = "top")
```

![](CEriskav_files/figure-html/unnamed-chunk-10-25.png)![](CEriskav_files/figure-html/unnamed-chunk-10-26.png)

``` r
plot(bcea_smoke, graph = "ggplot", pos = "bottom")
```

![](CEriskav_files/figure-html/unnamed-chunk-10-27.png)![](CEriskav_files/figure-html/unnamed-chunk-10-28.png)

``` r
plot(bcea_smoke, graph = "ggplot", pos = "left")
```

![](CEriskav_files/figure-html/unnamed-chunk-10-29.png)![](CEriskav_files/figure-html/unnamed-chunk-10-30.png)

``` r
plot(bcea_smoke, graph = "ggplot", pos = "right")
```

![](CEriskav_files/figure-html/unnamed-chunk-10-31.png)![](CEriskav_files/figure-html/unnamed-chunk-10-32.png)
