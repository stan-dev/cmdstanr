# Check syntax of a Stan program

The `$check_syntax()` method of a
[`CmdStanModel`](https://mc-stan.org/cmdstanr/pr/1287/reference/CmdStanModel.md)
object checks the Stan program for syntax errors and returns `TRUE`
(invisibly) if parsing succeeds. If invalid syntax is found an error is
thrown.

The standalone function `check_syntax_stan_file()` does the same for a
Stan program without creating a model object, and so without compiling
it.

## Usage

``` r
check_syntax(
  pedantic = FALSE,
  include_paths = NULL,
  stanc_options = list(),
  quiet = FALSE
)

check_syntax_stan_file(
  stan_file,
  include_paths = NULL,
  pedantic = FALSE,
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
  in the Stan User's Guide.

- include_paths:

  (character vector) Paths to directories where Stan should look for
  files specified in `#include` directives in the Stan program. The
  method uses the model's own include paths when none are given.
  `check_syntax_stan_file()` uses the program's own directory when none
  are given and the program contains `#include` directives.

- stanc_options:

  (list) Any other Stan-to-C++ transpiler options to be used when
  compiling the model. See the documentation for
  [`cmdstan_model()`](https://mc-stan.org/cmdstanr/pr/1287/reference/cmdstan_model.md)
  for details.

- quiet:

  (logical) Should informational messages be suppressed? The default is
  `FALSE`, which will print a message if the Stan program is valid or
  the compiler error message if there are syntax errors. If `TRUE`, only
  the error message will be printed.

- stan_file:

  (string) The path to a Stan program.

## Value

`TRUE` (invisibly) if the program is valid.

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
[`model-method-build_info`](https://mc-stan.org/cmdstanr/pr/1287/reference/model-method-build_info.md),
[`model-method-cmdstan_defaults`](https://mc-stan.org/cmdstanr/pr/1287/reference/model-method-cmdstan_defaults.md),
[`model-method-diagnose`](https://mc-stan.org/cmdstanr/pr/1287/reference/model-method-diagnose.md),
[`model-method-expose_functions`](https://mc-stan.org/cmdstanr/pr/1287/reference/model-method-expose_functions.md),
[`model-method-format`](https://mc-stan.org/cmdstanr/pr/1287/reference/model-method-format.md),
[`model-method-generate-quantities`](https://mc-stan.org/cmdstanr/pr/1287/reference/model-method-generate-quantities.md),
[`model-method-is_current`](https://mc-stan.org/cmdstanr/pr/1287/reference/model-method-is_current.md),
[`model-method-laplace`](https://mc-stan.org/cmdstanr/pr/1287/reference/model-method-laplace.md),
[`model-method-model-info`](https://mc-stan.org/cmdstanr/pr/1287/reference/model-method-model-info.md),
[`model-method-optimize`](https://mc-stan.org/cmdstanr/pr/1287/reference/model-method-optimize.md),
[`model-method-pathfinder`](https://mc-stan.org/cmdstanr/pr/1287/reference/model-method-pathfinder.md),
[`model-method-sample`](https://mc-stan.org/cmdstanr/pr/1287/reference/model-method-sample.md),
[`model-method-sample_mpi`](https://mc-stan.org/cmdstanr/pr/1287/reference/model-method-sample_mpi.md),
[`model-method-variables`](https://mc-stan.org/cmdstanr/pr/1287/reference/model-method-variables.md),
[`model-method-variational`](https://mc-stan.org/cmdstanr/pr/1287/reference/model-method-variational.md)

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
mod <- cmdstan_model(file)

# the program is syntactically correct, however...
mod$check_syntax()
#> Stan program is syntactically correct

# pedantic mode will warn that lambda should be constrained to be positive
# and that lambda has no prior distribution
mod$check_syntax(pedantic = TRUE)
#> Warning in '/tmp/RtmpCyrpRM/model_287cd4f50e093cb87805d29fd774bdf8.stan', line 8, column 2 to column 14:
#>     The parameter lambda has no priors. This means either no prior is
#>     provided, or the prior(s) depend on data variables. In the later case,
#>     this may be a false positive.
#> Warning in '/tmp/RtmpCyrpRM/model_287cd4f50e093cb87805d29fd774bdf8.stan', line 11, column 14 to column 20:
#>     A poisson distribution is given parameter lambda as a rate parameter
#>     (argument 1), but lambda was not constrained to be strictly positive.
#> Stan program is syntactically correct
# }
```
