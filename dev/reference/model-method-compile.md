# Compile a Stan program

The `$compile()` method of a
[`CmdStanModel`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanModel.md)
object checks the syntax of the Stan program, translates the program to
C++, and creates a compiled executable. To just check the syntax of a
Stan program without compiling it use the
[`$check_syntax()`](https://mc-stan.org/cmdstanr/dev/reference/model-method-check_syntax.md)
method instead.

In most cases the user does not need to explicitly call the `$compile()`
method as compilation will occur when calling
[`cmdstan_model()`](https://mc-stan.org/cmdstanr/dev/reference/cmdstan_model.md).
However it is possible to set `compile=FALSE` in the call to
[`cmdstan_model()`](https://mc-stan.org/cmdstanr/dev/reference/cmdstan_model.md)
and subsequently call the `$compile()` method directly.

After compilation, the paths to the executable and the `.hpp` file
containing the generated C++ code are available via the `$exe_file()`
and `$hpp_file()` methods. The default is to create the executable in
the same directory as the Stan program and to write the generated C++
code in a temporary directory. To save the C++ code to a non-temporary
location use `$save_hpp_file(dir)`.

## Usage

``` r
compile(
  quiet = TRUE,
  dir = NULL,
  pedantic = FALSE,
  include_paths = NULL,
  user_header = NULL,
  cpp_options = list(),
  stanc_options = list(),
  force_recompile = getOption("cmdstanr_force_recompile", default = FALSE),
  compile_model_methods = FALSE,
  compile_standalone = FALSE,
  dry_run = FALSE,
  compile_hessian_method = FALSE,
  threads = FALSE
)
```

## Arguments

- quiet:

  (logical) Should the verbose output from CmdStan during compilation be
  suppressed? The default is `TRUE`, but if you encounter an error we
  recommend trying again with `quiet=FALSE` to see more of the output.

- dir:

  (string) The path to the directory in which to store the CmdStan
  executable (or `.hpp` file if using `$save_hpp_file()`). The default
  is the same location as the Stan program.

- pedantic:

  (logical) Should pedantic mode be turned on? The default is `FALSE`.
  Pedantic mode attempts to warn you about potential issues in your Stan
  program beyond syntax errors. For details see the [*Pedantic mode*
  section](https://mc-stan.org/docs/stan-users-guide/pedantic-mode.html)
  in the Stan Reference Manual. **Note:** to do a pedantic check for a
  model without compiling it or for a model that is already compiled the
  [`$check_syntax()`](https://mc-stan.org/cmdstanr/dev/reference/model-method-check_syntax.md)
  method can be used instead.

- include_paths:

  (character vector) Paths to directories where Stan should look for
  files specified in `#include` directives in the Stan program.

- user_header:

  (string) The path to a C++ file (with a .hpp extension) to compile
  with the Stan model.

- cpp_options:

  (list) Any makefile options to be used when compiling the model
  (`STAN_THREADS`, `STAN_MPI`, `STAN_OPENCL`, etc.). Anything you would
  otherwise write in the `make/local` file. For an example of using
  threading see the Stan case study [Reduce Sum: A Minimal
  Example](https://mc-stan.org/users/documentation/case-studies/reduce_sum_tutorial.html).

- stanc_options:

  (list) Any Stan-to-C++ transpiler options to be used when compiling
  the model. See the **Examples** section below as well as the `stanc`
  chapter of the CmdStan Guide for more details on available options:
  https://mc-stan.org/docs/cmdstan-guide/stanc.html.

- force_recompile:

  (logical) Should the model be recompiled even if was not modified
  since last compiled. The default is `FALSE`. Can also be set via a
  global `cmdstanr_force_recompile` option.

- compile_model_methods:

  (logical) Compile additional model methods
  ([`log_prob()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-log_prob.md),
  [`grad_log_prob()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-grad_log_prob.md),
  [`constrain_variables()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-constrain_variables.md),
  [`unconstrain_variables()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-unconstrain_variables.md)).

- compile_standalone:

  (logical) Should functions in the Stan model be compiled for use in R?
  If `TRUE` the functions will be available via the `functions` field in
  the compiled model object. This can also be done after compilation
  using the
  [`$expose_functions()`](https://mc-stan.org/cmdstanr/dev/reference/model-method-expose_functions.md)
  method.

- dry_run:

  (logical) If `TRUE`, the code will do all checks before compilation,
  but skip the actual C++ compilation. Used to speedup tests.

- compile_hessian_method:

  (logical) Should the (experimental)
  [`hessian()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-hessian.md)
  method be be compiled with the model methods?

- threads:

  Deprecated and will be removed in a future release. Please turn on
  threading via `cpp_options = list(stan_threads = TRUE)` instead.

## Value

The `$compile()` method is called for its side effect of creating the
executable and adding its path to the
[`CmdStanModel`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanModel.md)
object, but it also returns the
[`CmdStanModel`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanModel.md)
object invisibly.

After compilation, the `$exe_file()`, `$hpp_file()`, and
`$save_hpp_file()` methods can be used and return file paths.

## See also

The
[`$check_syntax()`](https://mc-stan.org/cmdstanr/dev/reference/model-method-check_syntax.md)
method to check Stan syntax or enable pedantic model without compiling.

The CmdStanR website
([mc-stan.org/cmdstanr](https://mc-stan.org/cmdstanr/)) for online
documentation and tutorials.

The Stan and CmdStan documentation:

- Stan documentation:
  [mc-stan.org/users/documentation](https://mc-stan.org/users/documentation/)

- CmdStan User’s Guide:
  [mc-stan.org/docs/cmdstan-guide](https://mc-stan.org/docs/cmdstan-guide/)

Other CmdStanModel methods:
[`model-method-check_syntax`](https://mc-stan.org/cmdstanr/dev/reference/model-method-check_syntax.md),
[`model-method-diagnose`](https://mc-stan.org/cmdstanr/dev/reference/model-method-diagnose.md),
[`model-method-expose_functions`](https://mc-stan.org/cmdstanr/dev/reference/model-method-expose_functions.md),
[`model-method-format`](https://mc-stan.org/cmdstanr/dev/reference/model-method-format.md),
[`model-method-generate-quantities`](https://mc-stan.org/cmdstanr/dev/reference/model-method-generate-quantities.md),
[`model-method-laplace`](https://mc-stan.org/cmdstanr/dev/reference/model-method-laplace.md),
[`model-method-optimize`](https://mc-stan.org/cmdstanr/dev/reference/model-method-optimize.md),
[`model-method-pathfinder`](https://mc-stan.org/cmdstanr/dev/reference/model-method-pathfinder.md),
[`model-method-sample`](https://mc-stan.org/cmdstanr/dev/reference/model-method-sample.md),
[`model-method-sample_mpi`](https://mc-stan.org/cmdstanr/dev/reference/model-method-sample_mpi.md),
[`model-method-variables`](https://mc-stan.org/cmdstanr/dev/reference/model-method-variables.md),
[`model-method-variational`](https://mc-stan.org/cmdstanr/dev/reference/model-method-variational.md)

## Examples

``` r
# \dontrun{
file <- file.path(cmdstan_path(), "examples/bernoulli/bernoulli.stan")

# by default compilation happens when cmdstan_model() is called.
# to delay compilation until calling the $compile() method set compile=FALSE
mod <- cmdstan_model(file, compile = FALSE)
mod$compile()
mod$exe_file()
#> [1] "/home/runner/.cmdstan/cmdstan-2.37.0/examples/bernoulli/bernoulli"

# turn on threading support (for using functions that support within-chain parallelization)
mod$compile(force_recompile = TRUE, cpp_options = list(stan_threads = TRUE))
mod$exe_file()
#> [1] "/home/runner/.cmdstan/cmdstan-2.37.0/examples/bernoulli/bernoulli"

# turn on pedantic mode (new in Stan v2.24)
file_pedantic <- write_stan_file("
parameters {
  real sigma;  // pedantic mode will warn about missing <lower=0>
}
model {
  sigma ~ exponential(1);
}
")
mod <- cmdstan_model(file_pedantic, pedantic = TRUE)
#> Warning in '/tmp/Rtmpf5Zbzn/model-1b2f2ce827ce.stan', line 6, column 2: Parameter
#>     sigma is given a exponential distribution, which has strictly positive
#>     support, but sigma was not constrained to be strictly positive.

# }
```
