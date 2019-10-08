[<img src="https://raw.githubusercontent.com/stan-dev/logos/master/logo_tm.png" width=100 alt="Stan Logo"/>](https://mc-stan.org)

# CmdStanR 

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/stan-dev/cmdstanr.svg?branch=master)](https://travis-ci.org/stan-dev/cmdstanr)
[![Codecov test coverage](https://codecov.io/gh/stan-dev/cmdstanr/branch/master/graph/badge.svg)](https://codecov.io/gh/stan-dev/cmdstanr?branch=master)
<!-- badges: end -->

CmdStanR is a lightweight interface to [Stan](https://mc-stan.org) for R users
(see [CmdStanPy](https://github.com/stan-dev/cmdstanpy) for Python).

CmdStanR is in early stages of development and currently requires RStan to use
`rstan::stan_rdump()` for writing data files and `rstan::read_stan_csv()` for
reading CmdStan output into R (RStan is not used to compile anything). This
dependency on RStan will be eventually be removed.

### Goals

* A clean interface to Stan services so that CmdStanR can keep up with Stan
releases.

* Minimal dependencies:
  - depend on very few other R packages
  - R code doesn't interface directly with c++, only calls compiled executables 
      
* Modularity: CmdStanR runs Stan's algorithm and lets downstream modules do the
analysis.


### Contributing 

There is a lot of work still to be done and contributions are very welcome! 
If you are interested in contributing please comment on an open issue
or open a new one if none are applicable.  
