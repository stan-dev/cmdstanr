# CmdStanR <img src="man/figures/logo.png" align="right" width="120" />

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/cmdstanr)](https://CRAN.R-project.org/package=cmdstanr)
[![Unit tests](https://github.com/stan-dev/cmdstanr/workflows/Unit%20tests/badge.svg)](https://github.com/stan-dev/cmdstanr/actions?workflow=Unit-tests)
[![Codecov test coverage](https://codecov.io/gh/stan-dev/cmdstanr/branch/master/graph/badge.svg)](https://app.codecov.io/gh/stan-dev/cmdstanr?branch=master)
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

#### Installing the R package

You can install the latest beta release of the **cmdstanr** R package with

```r
# we recommend running this is a fresh R session or restarting your current session
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
```
This does not install the vignettes, which take a long time to build, but they are always available
online at https://mc-stan.org/cmdstanr/articles/.

To instead install the latest development version of the package from GitHub use

```r
# install.packages("remotes")
remotes::install_github("stan-dev/cmdstanr")
```

#### Installing CmdStan

If you don't already have CmdStan installed then, in addition to installing the
R package, it is also necessary to install CmdStan using CmdStanR's
`install_cmdstan()` function. A suitable C++ toolchain is also required.
Instructions are provided in the [_Getting started with
CmdStanR_](https://mc-stan.org/cmdstanr/articles/cmdstanr.html) vignette.


### Contributing

There is a lot of work still to be done and contributions are very welcome!
If you are interested in contributing please comment on an open issue
or open a new one if none are applicable.

### License

CmdStanR, like CmdStan and the core Stan C++ code, is licensed under the
following licenses:

- Code: BSD 3-clause (https://opensource.org/licenses/BSD-3-Clause)
- Documentation: CC-BY 4.0 (https://creativecommons.org/licenses/by/4.0/)
