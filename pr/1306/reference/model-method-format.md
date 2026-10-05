# Run stanc's auto-formatter on the model code.

The `$format()` method of a
[`CmdStanModel`](https://mc-stan.org/cmdstanr/pr/1306/reference/CmdStanModel.md)
object runs stanc's auto-formatter on the model code. It either saves
the formatted model directly back to the file or prints it for
inspection. The standalone function `format_stan_file()` does the same
for a Stan program without creating a model object.

## Usage

``` r
format(
  overwrite_file = FALSE,
  canonicalize = FALSE,
  backup = TRUE,
  max_line_length = NULL,
  quiet = FALSE
)

format_stan_file(
  stan_file,
  include_paths = NULL,
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
  file? The default is `FALSE`.

- canonicalize:

  (list or logical) Defines whether or not the compiler should
  'canonicalize' the Stan model, removing things like deprecated syntax.
  Default is `FALSE`. If `TRUE`, all canonicalizations are run. You can
  also supply a list of strings which represent options. In that case
  the options are passed to stanc. See the [User's guide
  section](https://mc-stan.org/docs/stan-users-guide/stanc-pretty-printing.html#canonicalizing)
  for available canonicalization options.

- backup:

  (logical) If `TRUE`, create a backup before writing to the file. The
  backup filename is the Stan filename followed by
  `.bak-YYYYMMDDHHMMSS`, where the final digits encode the timestamp.
  Disable this option if you're sure you have other copies of the file
  or are using a version control system like Git. Defaults to `TRUE`.
  The value is ignored if `overwrite_file = FALSE`.

- max_line_length:

  (integer) The maximum length of a line when formatting. The default is
  `NULL`, which defers to the default line length of stanc.

- quiet:

  (logical) Should informational messages be suppressed? The default is
  `FALSE`.

- stan_file:

  (string) The path to a Stan program.

- include_paths:

  (character vector) Paths to directories where Stan should look for
  files specified in `#include` directives in the Stan program. Relative
  paths are resolved against the working directory when the model object
  is created and stored as absolute paths, so subsequent changes to the
  working directory do not affect them. When the program contains
  `#include` directives and no paths are given, the program's own
  directory is used.

## Value

`TRUE` (invisibly) if formatting succeeds.

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
[`model-method-build_info`](https://mc-stan.org/cmdstanr/pr/1306/reference/model-method-build_info.md),
[`model-method-check_syntax`](https://mc-stan.org/cmdstanr/pr/1306/reference/model-method-check_syntax.md),
[`model-method-cmdstan_defaults`](https://mc-stan.org/cmdstanr/pr/1306/reference/model-method-cmdstan_defaults.md),
[`model-method-diagnose`](https://mc-stan.org/cmdstanr/pr/1306/reference/model-method-diagnose.md),
[`model-method-expose_functions`](https://mc-stan.org/cmdstanr/pr/1306/reference/model-method-expose_functions.md),
[`model-method-generate-quantities`](https://mc-stan.org/cmdstanr/pr/1306/reference/model-method-generate-quantities.md),
[`model-method-is_current`](https://mc-stan.org/cmdstanr/pr/1306/reference/model-method-is_current.md),
[`model-method-laplace`](https://mc-stan.org/cmdstanr/pr/1306/reference/model-method-laplace.md),
[`model-method-model-info`](https://mc-stan.org/cmdstanr/pr/1306/reference/model-method-model-info.md),
[`model-method-optimize`](https://mc-stan.org/cmdstanr/pr/1306/reference/model-method-optimize.md),
[`model-method-pathfinder`](https://mc-stan.org/cmdstanr/pr/1306/reference/model-method-pathfinder.md),
[`model-method-sample`](https://mc-stan.org/cmdstanr/pr/1306/reference/model-method-sample.md),
[`model-method-sample_mpi`](https://mc-stan.org/cmdstanr/pr/1306/reference/model-method-sample_mpi.md),
[`model-method-variables`](https://mc-stan.org/cmdstanr/pr/1306/reference/model-method-variables.md),
[`model-method-variational`](https://mc-stan.org/cmdstanr/pr/1306/reference/model-method-variational.md)

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

format_stan_file(file, canonicalize = list("deprecations"))
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

# or through a model object
mod <- cmdstan_model(file)
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

# overwrite the original file instead of just printing it, then create the
# model object again to rebuild the executable from the formatted program
mod$format(canonicalize = list("deprecations"), overwrite_file = TRUE)
#> Old version of the model stored to /tmp/RtmpnItZlq/model_757a40a9bc18f0e4dd1fe7eec4863b8e.stan.bak-20261005192839.
mod <- cmdstan_model(file)
# }
```
