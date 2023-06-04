
# Resubmission (2.4.3)

* compute.evppi() was mismatched with the dplyr compute() function. We've changed the 
function name to compute_evppi() to avoid confusion since it is only an internal function.

CHECK message:

Flavor: r-devel-linux-x86_64-debian-gcc
Check: S3 generic/method consistency, Result: NOTE
  Mismatches for apparent methods not registered:
  compute:
    function(x, ...)
  compute.evppi:
    function(he, fit.full)
  
* Similarly, the internal function plot.mesh() is not a plot method but was being considered so,
so we have renamed to plot_mesh()

CHECK message:

  plot:
    function(x, ...)
  plot.mesh:
    function(mesh, data, plot)
  See section 'Registering S3 methods' in the 'Writing R Extensions'
  manual.

## R CMD check results

0 errors | 0 warnings | 1 note

* Any notes about using INLA has been condoned in previous versions, as they are only suggested.
 
CHECK message:

  Suggests or Enhances not in mainstream repositories:
    INLA
  Availability using Additional_repositories specification:
    INLA   yes   https://inla.r-inla-download.org/R/stable/

Flavor: r-devel-linux-x86_64-debian-gcc, r-devel-windows-x86_64
Check: package dependencies, Result: NOTE
  Package suggested but not available for checking: 'INLA'

## Downstream dependencies

We checked 2 reverse dependencies (missingHE, heesim), comparing R CMD check results across CRAN and dev versions of this package.

* We saw 0 new problems
* We failed to check 0 packages



