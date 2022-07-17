### Different ways of interfacing with Stan's C++

The RStan interface ([**rstan**](https://mc-stan.org/rstan/) package) is an
in-memory interface to Stan and relies on R packages like **Rcpp** and
**inline** to call C++ code from R. On the other hand, the CmdStanR interface
does not directly call any C++ code from R, instead relying on the CmdStan
interface behind the scenes for compilation, running algorithms, and writing
results to output files.

### Advantages of RStan

* Advanced features. We are working on making these available outside of RStan
but currently they are only available to R users via RStan:
  - `rstan::log_prob()`
  - `rstan::grad_log_prob()`
  - `rstan::expose_stan_functions()`

* Allows other developers to distribute R packages with
_pre-compiled_ Stan programs (like **rstanarm**) on CRAN.

### Advantages of CmdStanR

* Compatible with latest versions of Stan. Keeping up with Stan releases is
complicated for RStan, often requiring non-trivial changes to the **rstan**
package and new CRAN releases of both **rstan** and **StanHeaders**. With
CmdStanR the latest improvements in Stan will be available from R immediately
after updating CmdStan using `cmdstanr::install_cmdstan()`.

* Fewer installation issues (e.g., no need to mess with Makevars files).

* Running Stan via external processes results in fewer unexpected crashes,
especially in RStudio.

* Less memory overhead.

* More permissive license. RStan uses the GPL-3 license while the license for
CmdStanR is BSD-3, which is a bit more permissive and is the same license used
for CmdStan and the Stan C++ source code.
