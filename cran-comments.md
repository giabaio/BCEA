## R CMD check results
There were no ERRORs or WARNINGs. 

* Any notes about using INLA has been condoned in previous versions, as they are only suggested.

* This is a resubmission after addressing a previous error. So CRAN-pretest message "Insufficient package version (submitted: 2.4.1, existing: 2.4.1)" not relevant in this case.

* bug in rstan package hot fixed, where rstan was trying to forcefully load libtbbmalloc_proxy.dylib at runtime. (see R-package-devel thread https://stat.ethz.ch/pipermail/r-package-devel/2021q4/007532.html)