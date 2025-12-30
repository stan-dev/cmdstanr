# Run stanc's auto-formatter on the model code.

The `$format()` method of a
[`CmdStanModel`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanModel.md)
object runs stanc's auto-formatter on the model code. Either saves the
formatted model directly back to the file or prints it for inspection.

## Usage

``` r
format(
  overwrite_file = FALSE,
  canonicalize = FALSE,
  backup = TRUE,
  max_line_length = NULL,
  quiet = FALSE
)
```

## Arguments

- overwrite_file:

  (logical) Should the formatted code be written back to the input model
  file. The default is `FALSE`.

- canonicalize:

  (list or logical) Defines whether or not the compiler should
  'canonicalize' the Stan model, removing things like deprecated syntax.
  Default is `FALSE`. If `TRUE`, all canonicalizations are run. You can
  also supply a list of strings which represent options. In that case
  the options are passed to stanc (new in Stan 2.29). See the [User's
  guide
  section](https://mc-stan.org/docs/stan-users-guide/stanc-pretty-printing.html#canonicalizing)
  for available canonicalization options.

- backup:

  (logical) If `TRUE`, create stanfile.bak backups before writing to the
  file. Disable this option if you're sure you have other copies of the
  file or are using a version control system like Git. Defaults to
  `TRUE`. The value is ignored if `overwrite_file = FALSE`.

- max_line_length:

  (integer) The maximum length of a line when formatting. The default is
  `NULL`, which defers to the default line length of stanc.

- quiet:

  (logical) Should informational messages be suppressed? The default is
  `FALSE`.

## Value

The `$format()` method returns `TRUE` (invisibly) if the model is valid.

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
[`model-method-check_syntax`](https://mc-stan.org/cmdstanr/dev/reference/model-method-check_syntax.md),
[`model-method-compile`](https://mc-stan.org/cmdstanr/dev/reference/model-method-compile.md),
[`model-method-diagnose`](https://mc-stan.org/cmdstanr/dev/reference/model-method-diagnose.md),
[`model-method-expose_functions`](https://mc-stan.org/cmdstanr/dev/reference/model-method-expose_functions.md),
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

# Example of removing unnecessary whitespace
file <- write_stan_file("
data {
  int N;
  array[N] int y;
}
parameters {
  real                     lambda;
}
model {
  target +=
 poisson_lpmf(y | lambda);
}
")

# set compile=FALSE then call format to fix old syntax
mod <- cmdstan_model(file, compile = FALSE)
mod$format(canonicalize = list("deprecations"))
#> data {
#>   int N;
#>   array[N] int y;
#> }
#> parameters {
#>   real lambda;
#> }
#> model {
#>   target += poisson_lpmf(y | lambda);
#> }
#> 
#> 

# overwrite the original file instead of just printing it
mod$format(canonicalize = list("deprecations"), overwrite_file = TRUE)
#> Old version of the model stored to /tmp/RtmpNMcPBJ/model_39022cccc3fe5384fab5a52b791fead6.stan.bak-20251230220334.
mod$compile()
# }
```
