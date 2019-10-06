[<img src="https://raw.githubusercontent.com/stan-dev/logos/master/logo_tm.png" width=100 alt="Stan Logo"/>](https://mc-stan.org)

# CmdStanR 

<!-- badges: start -->

<!-- [![Travis build status](https://travis-ci.org/jgabry/cmdstanr.svg?branch=master)](https://travis-ci.org/jgabry/cmdstanr) -->

<!-- badges: end -->

CmdStanR is a lightweight interface to [Stan](https://mc-stan.org) for R users
(see [CmdStanPy](https://github.com/stan-dev/cmdstanpy) for Python).

CmdStanR is in early stages of development and the following warnings currently apply:

**WARNINGS:**

* Still requires RStan for reading CmdStan output (although doesn’t require that you can compile anything via RStan)
* Does NOT work on Windows yet
* Requires an already working installation of CmdStan (doesn't help install that yet) 

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
