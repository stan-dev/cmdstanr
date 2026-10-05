# Check whether the model can run without a rebuild

The `$is_current()` method of a
[`CmdStanModel`](https://mc-stan.org/cmdstanr/rc-website-test/reference/CmdStanModel.md)
object runs the check that `$sample()` and the other fitting methods run
before they start, and returns the answer instead of raising an error.
It returns `TRUE` when the executable is still the one the object was
created with and, for a model created from a Stan file, nothing the
executable was built from has changed: the Stan file and its includes,
the user header, the build options and the CmdStan installation. It
returns `FALSE` when any of those changed or when the Stan file or the
executable is gone, which is when the fitting methods refuse to run.
Call
[`cmdstan_model()`](https://mc-stan.org/cmdstanr/rc-website-test/reference/cmdstan_model.md)
again to rebuild. It errors rather than answering when the check itself
can't run, which happens when stanc rejects the program or can't find an
included file, when the user header is gone, or when no CmdStan
installation is set.

A package that keeps a `CmdStanModel` inside a saved fit can call it to
decide whether to rebuild before running the model again.

## Usage

``` r
is_current()
```

## Value

`TRUE` or `FALSE`.

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
[`model-method-build_info`](https://mc-stan.org/cmdstanr/rc-website-test/reference/model-method-build_info.md),
[`model-method-check_syntax`](https://mc-stan.org/cmdstanr/rc-website-test/reference/model-method-check_syntax.md),
[`model-method-cmdstan_defaults`](https://mc-stan.org/cmdstanr/rc-website-test/reference/model-method-cmdstan_defaults.md),
[`model-method-diagnose`](https://mc-stan.org/cmdstanr/rc-website-test/reference/model-method-diagnose.md),
[`model-method-expose_functions`](https://mc-stan.org/cmdstanr/rc-website-test/reference/model-method-expose_functions.md),
[`model-method-format`](https://mc-stan.org/cmdstanr/rc-website-test/reference/model-method-format.md),
[`model-method-generate-quantities`](https://mc-stan.org/cmdstanr/rc-website-test/reference/model-method-generate-quantities.md),
[`model-method-laplace`](https://mc-stan.org/cmdstanr/rc-website-test/reference/model-method-laplace.md),
[`model-method-model-info`](https://mc-stan.org/cmdstanr/rc-website-test/reference/model-method-model-info.md),
[`model-method-optimize`](https://mc-stan.org/cmdstanr/rc-website-test/reference/model-method-optimize.md),
[`model-method-pathfinder`](https://mc-stan.org/cmdstanr/rc-website-test/reference/model-method-pathfinder.md),
[`model-method-sample`](https://mc-stan.org/cmdstanr/rc-website-test/reference/model-method-sample.md),
[`model-method-sample_mpi`](https://mc-stan.org/cmdstanr/rc-website-test/reference/model-method-sample_mpi.md),
[`model-method-variables`](https://mc-stan.org/cmdstanr/rc-website-test/reference/model-method-variables.md),
[`model-method-variational`](https://mc-stan.org/cmdstanr/rc-website-test/reference/model-method-variational.md)

## Examples

``` r
# \dontrun{
mod <- cmdstan_model(
  file.path(cmdstan_path(), "examples/bernoulli/bernoulli.stan")
)
mod$is_current()
#> [1] TRUE
# }
```
