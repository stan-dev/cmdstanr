# CmdStanR

<!-- badges: start -->
<!-- badges: end -->

CmdStanR is a lightweight interface to [Stan](https://mc-stan.org) for R users
(see [CmdStanPy](https://github.com/stan-dev/cmdstanpy) for Python). It provides
the necessary objects and functions to compile a Stan program and run Stan's
algorithms from R using [CmdStan](https://github.com/stan-dev/cmdstan).

**WARNINGS:**

* Still very early in development
* Does NOT work on Windows yet
* Requires a working version of CmdStan (doesn't help install that yet) 

### Goals

* A clean interface to Stan services so that CmdStanR can keep up with Stan
releases.

* Minimal dependencies:
  - depend on very few other R packages
  - R code doesn't interface directly with c++, only calls compiled executables    
  
* Modularity: CmdStanR runs Stan's algorithm and lets downstream modules do the
analysis.

