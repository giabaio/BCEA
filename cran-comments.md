## R CMD check results
There were no ERRORs or WARNINGs. 

* This is a resubmission after addressing a previous error. So CRAN-pretest message "Insufficient package version (submitted: 2.4.1, existing: 2.4.1)" not relevant in this case.

* Bug in rstan package hot fixed, where rstan was trying to forcefully load libtbbmalloc_proxy.dylib at runtime. (Discovered after very helpful input from R-package-devel thread https://stat.ethz.ch/pipermail/r-package-devel/2021q4/007532.html)

> Simon Urbanek (simon.urbanek@r-project.org) wrote:
>
> "no action is needed on your end since it's not your fault. It was good of you to have the test there because it unearthed the issue. I have re-run the test with hot-fixed rstan and it passes the check so you're good as far as I'm concerned. More urgently we need an update from rstan and stanette (.onUnoad needs corresponding fix in both cases as well). This is a serious bug in rstan - apparently they only do that on macOS which explains why other platforms don't see it."

* Any notes about using INLA has been condoned in previous versions, as they are only suggested.
