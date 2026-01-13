# Check syntax of a Stan program

The `$check_syntax()` method of a
[`CmdStanModel`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanModel.md)
object checks the Stan program for syntax errors and returns `TRUE`
(invisibly) if parsing succeeds. If invalid syntax in found an error is
thrown.

## Usage

``` r
check_syntax(
  pedantic = FALSE,
  include_paths = NULL,
  stanc_options = list(),
  quiet = FALSE
)
```

## Arguments

- pedantic:

  (logical) Should pedantic mode be turned on? The default is `FALSE`.
  Pedantic mode attempts to warn you about potential issues in your Stan
  program beyond syntax errors. For details see the [*Pedantic mode*
  chapter](https://mc-stan.org/docs/stan-users-guide/pedantic-mode.html)
  in the Stan Reference Manual.

- include_paths:

  (character vector) Paths to directories where Stan should look for
  files specified in `#include` directives in the Stan program.

- stanc_options:

  (list) Any other Stan-to-C++ transpiler options to be used when
  compiling the model. See the documentation for the
  [`$compile()`](https://mc-stan.org/cmdstanr/dev/reference/model-method-compile.md)
  method for details.

- quiet:

  (logical) Should informational messages be suppressed? The default is
  `FALSE`, which will print a message if the Stan program is valid or
  the compiler error message if there are syntax errors. If `TRUE`, only
  the error message will be printed.

## Value

The `$check_syntax()` method returns `TRUE` (invisibly) if the model is
valid.

## See also

The CmdStanR website
([mc-stan.org/cmdstanr](https://mc-stan.org/cmdstanr/)) for online
documentation and tutorials.

The Stan and CmdStan documentation:

- Stan documentation:
  [mc-stan.org/users/documentation](https://mc-stan.org/users/documentation/)

- CmdStan User’s Guide:
  [mc-stan.org/docs/cmdstan-guide](https://mc-stan.org/docs/cmdstan-guide/)

Other CmdStanModel methods:
[`model-method-compile`](https://mc-stan.org/cmdstanr/dev/reference/model-method-compile.md),
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
file <- write_stan_file("
data {
  int N;
  array[N] int y;
}
parameters {
  // should have <lower=0> but omitting to demonstrate pedantic mode
  real lambda;
}
model {
  y ~ poisson(lambda);
}
")
mod <- cmdstan_model(file, compile = FALSE)

# the program is syntactically correct, however...
mod$check_syntax()
#> Stan program is syntactically correct

# pedantic mode will warn that lambda should be constrained to be positive
# and that lambda has no prior distribution
mod$check_syntax(pedantic = TRUE)
#> Warning in '/tmp/Rtmp17Mk1I/model_febb1e69c7387a0e64cf13583e078104.stan', line 8, column 2 to column 14:
#>     The parameter lambda has no priors. This means either no prior is
#>     provided, or the prior(s) depend on data variables. In the later case,
#>     this may be a false positive.
#> Warning in '/tmp/Rtmp17Mk1I/model_febb1e69c7387a0e64cf13583e078104.stan', line 11, column 14 to column 20:
#>     A poisson distribution is given parameter lambda as a rate parameter
#>     (argument 1), but lambda was not constrained to be strictly positive.
#> Stan program is syntactically correct
# }
```
