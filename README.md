[<img src="https://raw.githubusercontent.com/stan-dev/logos/master/logo_tm.png" width=100 alt="Stan Logo"/>](https://mc-stan.org)

# CmdStanR 

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/stan-dev/cmdstanr.svg?branch=master)](https://travis-ci.org/stan-dev/cmdstanr)
[![Codecov test coverage](https://codecov.io/gh/stan-dev/cmdstanr/branch/master/graph/badge.svg)](https://codecov.io/gh/stan-dev/cmdstanr?branch=master)
<!-- badges: end -->

CmdStanR is a lightweight interface to [Stan](https://mc-stan.org) for R users
(see [CmdStanPy](https://github.com/stan-dev/cmdstanpy) for Python).

CmdStanR is in early stages of development and currently requires RStan to use
`rstan::read_stan_csv()` for reading CmdStan output into R (RStan is not used
to compile anything). This dependency on RStan will eventually be removed.

### Goals

* A clean interface to Stan services so that CmdStanR can keep up with Stan
releases.

* Minimal dependencies:
  - depend on very few other R packages
  - R code doesn't interface directly with c++, only calls compiled executables 
      
* Modularity: CmdStanR runs Stan's algorithms and lets downstream modules do the
analysis.

* More flexible license: CmdStanR uses the BSD-3 license rather than the GPL-3
license required for RStan.


### Installation

CmdStanR is not released yet, but will eventually be released as the
**cmdstanr** R package. Currently you can install the development version from
GitHub, but expect frequent changes until an official release.

```r
# install.packages("devtools")
devtools::install_github("stan-dev/cmdstanr")
```

### Contributing 

There is a lot of work still to be done and contributions are very welcome! 
If you are interested in contributing please comment on an open issue
or open a new one if none are applicable.  

### License

CmdStanR, like CmdStan and the core Stan C++ code, is licensed under new BSD.
See the [LICENSE.md](LICENSE.md) file.
