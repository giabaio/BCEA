
# Resubmission (2.4.4)

* Suggested package wasn't used conditionally in unit test causing CRAN check error. `MCMCvis` is now moved to Required packages.


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

