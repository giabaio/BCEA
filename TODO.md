# TO-DO list

## July 2025

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
   
* 