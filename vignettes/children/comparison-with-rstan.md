### Different ways of interfacing with Stan's C++

The RStan interface ([**rstan**](https://mc-stan.org/rstan/) package) provides
its core functionality through an in-memory interface to Stan and relies on R
packages such as **Rcpp** to call C++ code from R. CmdStanR's core model
compilation and inference workflow instead runs CmdStan in external processes
and reads the resulting output files. Only optional CmdStanR features, such as
`$expose_functions()` and the additional model methods, use **Rcpp** to call
compiled C++ code directly from R.

### Advantages of RStan

* CRAN provides binary versions of RStan for Windows and macOS. RStan-based
packages can also include precompiled Stan models in their binary packages, which allows users to run the models without a local C++ toolchain. 

  CmdStanR-based packages can use
  [`instantiate`](https://wlandau.github.io/instantiate/) to compile models
  once during package installation. Because CRAN's build machines do not 
  provide CmdStan, packages using this workflow currently need to be installed
  from source with CmdStan and a C++ toolchain available.

* Avoids use of R6 classes, which may result in more familiar syntax for many R users. 


### Advantages of CmdStanR

* CmdStan is installed separately from CmdStanR, so users can often update to a new Stan release by updating CmdStan without waiting for a new CmdStanR release.

* Running CmdStan in external processes isolates inference from the R process,
reducing the risk that a failure during inference terminates the R session.

* Potentially lower memory use in the R session. CmdStan writes results to CSV
files, and CmdStanR loads draws into R only when requested. This can avoid
retaining all output in memory during fitting.

* More permissive license. RStan uses the GPL (>= 3) license while the license
for CmdStanR is BSD 3-clause, which is a bit more permissive and is the same
license used for CmdStan and the Stan C++ source code.
