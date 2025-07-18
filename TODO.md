# TO-DO list

## July 2025

* There's a `make.report.R` file, which defines a few functions used to run `BCEA` in the background and save the output
onto a `pdf` report. I *think* these are legacy from when `BCEAweb` was just a function in `BCEA` (rather than a package itself),
so probably OK to remove this file as well as the `Suggest` to `rmarkdown` and `markdown`?

* Check the code all-round to see where we can find bottlenecks in computation and make things quicker.

* Do we *need* to make `rstan` a full `Import`? We use it in a couple of places --- `CreateInputs` and `bcea.rstan`, 
but in the former, the method defined does not really use any `rstan`-specific functions... Seems a bit of a bummer 
to make a strong dependence on a heavy package for a relatively marginal facility?...

* Similarly, `vdiffr` (which is only a `Suggest`) is only used in one of the tests... Could we actually remove it?

* Do we **need** to have `Rdpack` as an `Import`? We use it in various places as 
   ```
   #' @importFrom Rdpack reprompt
   ```
   but I wonder whether we really do anything with it, given that it doesn't seem to be called directly by any function??
   
* Should we make `ggplot` the default for graphs (instead of base)?

* The folder `revdep` is outdated. Should we just remove? Or write a script that keeps it up-to-date?