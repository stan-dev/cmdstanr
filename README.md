# CmdStanR <img src="man/figures/logo.png" align="right" width="120" />

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/cmdstanr)](https://CRAN.R-project.org/package=cmdstanr)
[![R-CMD-check](https://github.com/stan-dev/cmdstanr/workflows/R-CMD-check/badge.svg)](https://github.com/stan-dev/cmdstanr/actions?workflow=R-CMD-check)
[![Codecov test coverage](https://codecov.io/gh/stan-dev/cmdstanr/branch/master/graph/badge.svg)](https://codecov.io/gh/stan-dev/cmdstanr?branch=master)
<!-- badges: end -->

### Overview 

CmdStanR is a lightweight interface to [Stan](https://mc-stan.org) for R users
(see [CmdStanPy](https://github.com/stan-dev/cmdstanpy) for Python).

If you are new to CmdStanR we recommend starting with these vignettes:

* [_Getting started with CmdStanR_](https://mc-stan.org/cmdstanr/articles/cmdstanr.html)

* [_How does CmdStanR work?_](https://mc-stan.org/cmdstanr/articles/cmdstanr-internals.html)

### Goals

* A clean interface to Stan services so that CmdStanR can keep up with Stan
releases.

* R code that doesn't interface directly with C++, only calls compiled executables.
      
* Modularity: CmdStanR runs Stan's algorithms and lets downstream modules do the
analysis.

* Flexible [BSD-3 license](https://opensource.org/licenses/BSD-3-Clause).


### Installation

You can install the beta release of the **cmdstanr** R package with 

```r
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
```

or you can install the latest development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("stan-dev/cmdstanr")
```

### Contributing 

There is a lot of work still to be done and contributions are very welcome! 
If you are interested in contributing please comment on an open issue
or open a new one if none are applicable.  

### License

CmdStanR, like CmdStan and the core Stan C++ code, is licensed under new BSD.
See the `LICENSE.md` file.
