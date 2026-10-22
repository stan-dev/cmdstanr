# CmdStanR <img src="man/figures/logo.svg" align="right" width="120" />

<!-- badges: start -->
[![Unit tests](https://github.com/stan-dev/cmdstanr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/stan-dev/cmdstanr/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/stan-dev/cmdstanr/branch/master/graph/badge.svg)](https://app.codecov.io/gh/stan-dev/cmdstanr?branch=master)
<!-- badges: end -->

### Overview

CmdStanR is an interface to [Stan](https://mc-stan.org) for R users
(see [CmdStanPy](https://github.com/stan-dev/cmdstanpy) for Python).

If you are new to CmdStanR we recommend starting with these vignettes:

* [_Getting started with CmdStanR_](https://mc-stan.org/cmdstanr/articles/cmdstanr.html)

* [_How does CmdStanR work?_](https://mc-stan.org/cmdstanr/articles/cmdstanr-internals.html)

On the CmdStanR [website](https://mc-stan.org/cmdstanr/articles/index.html) there are 
additional vignettes covering other topics. 

### Goals

* A clean interface to Stan services so that CmdStanR can keep up with Stan
releases.

* R code that calls CmdStan executables rather than linking to Stan's C++.  
Rcpp is only used for optional features like exposing Stan functions to R. 

* Modularity: CmdStanR runs Stan's algorithms and lets downstream modules do the
analysis.

* Flexible [BSD-3 license](https://opensource.org/license/bsd-3-clause).

### Installation

#### Installing the R package

You can install the latest release of the R package with

```r
# we recommend running this in a fresh R session or restarting your current session
install.packages("cmdstanr", repos = c('https://stan-dev.r-universe.dev', getOption("repos")))
```
This does not install the vignettes, but they are always available online at 
https://mc-stan.org/cmdstanr/articles/.

The development version is currently the release candidate for CmdStanR 1.0,
which we'd like people to test. You can install it from GitHub with

```r
# install.packages("remotes")
remotes::install_github("stan-dev/cmdstanr@v1.0")
```

The release notes and documentation for the release candidate are at
https://mc-stan.org/cmdstanr/rc.

#### Installing CmdStan

If you don't already have CmdStan installed then, in addition to installing the
R package, it is also necessary to install CmdStan. CmdStanR's `install_cmdstan()` 
function can do this for you. A suitable C++ toolchain is also required.
Instructions are provided in the [_Getting started with
CmdStanR_](https://mc-stan.org/cmdstanr/articles/cmdstanr.html) vignette.

### Contributing

We welcome contributions from anyone! If you are interested in contributing, 
please comment on an open [issue](https://github.com/stan-dev/cmdstanr/issues),
open a new one, or post on the Stan [forum](https://discourse.mc-stan.org). 
For more details,  please check [CONTRIBUTING.md](.github/CONTRIBUTING.md).

### License

CmdStanR, like CmdStan and the core Stan C++ code, is licensed under the
following licenses:

- Code: BSD 3-clause (https://opensource.org/license/bsd-3-clause/)
- Documentation: CC-BY 4.0 (https://creativecommons.org/licenses/by/4.0/)
