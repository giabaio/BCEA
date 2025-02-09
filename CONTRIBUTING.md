# Contributing to BCEA

Thank you for taking the time to contribute to the development of `BCEA`. You could find the following guidelines useful in making your contributions.

Before you start:

-   It is important to have a valid [GitHub account](https://github.com/signup/free).
-   Trivial changes to comments or documentation do not require creating a new issue.

## Did you find a bug?

-   Make sure the bug was not already reported in the Github [Issues](https://github.com/n8thangreen/BCEA/issues).
-   [Open an issue](https://github.com/n8thangreen/BCEA/issues/new) and clearly describe the issue with as much information as possible. A code sample or an executable test case are recommended.

## Did you plan to write a patch that fixes a bug?

-   [Open an issue](https://github.com/n8thangreen/BCEA/issues/new) and clearly describe the problem and discuss how your solution will affect `BCEA`.
-   Fork the repository on GitHub to work on the patch.
-   Get in touch with the maintainer to refine and prioritize your issue.

## Making changes and Pull requests

-   Start your work on your fork of the repository. If you haven't done this before, try using `usethis::create_from_github("n8thangreen/BCEA", fork = TRUE)`.
-   Install all development dependencies with `devtools::install_dev_deps()`, and then make sure the package passes R CMD check by running `devtools::check()`.
-   Create a Git branch for your pull request (PR). You may want to use `usethis::pr_init("brief-description-of-change")`.
-   Check for unnecessary whitespace with `git diff --check` and format code.
-   Commit messages should be descriptive, mentioning what was changed and why, and also **reference the relevant issue number**.
-   Ensure to add the necessary tests for your changes (`testthat` preferably).
-   Run **all** the tests to assure nothing else was accidentally broken, also keep an eye on the test coverage metric.
-   Commit to git, and then create a PR by running `usethis::pr_push()`, and following the prompts in your browser

## Style guide

The style used throughout follows the [Tidyverse style guide](https://style.tidyverse.org/). This may not be applied 100% of the time but this is the aspiration.


## Copyright issues

-   On submission, it is crucial your PR includes the following statement:

> I own the copyright on the code being contributed, and I hereby grant `BCEA` repo cph unlimited license to use this code in this version or any future version of `BCEA`. I reserve all other rights to the code.

-   It may not be advisable to contribute third party codes to this project. Useful suggestions are nonetheless welcomed.
-   The PRs are thereafter reviewed, with feedback communicated as soon as possible.

# Additional Resources

-   [General GitHub documentation](http://help.github.com/)
-   [About pull requests](https://docs.github.com/en/github/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/about-pull-requests)
