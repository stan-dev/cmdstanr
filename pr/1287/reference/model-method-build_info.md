# What is known about how the model's executable was built

The `$build_info()` method of a
[`CmdStanModel`](https://mc-stan.org/cmdstanr/pr/1287/reference/CmdStanModel.md)
object calls
[`stan_build_info()`](https://mc-stan.org/cmdstanr/pr/1287/reference/stan_build_info.md)
on the model's executable. See that page for what the result holds. The
method reports on the executable as it is now, so it also works on a
model whose executable was replaced or whose build record is gone.

This method is different from the `$cpp_options()` method, which answers
a narrower question: the C++ options this model object was created with.
`$build_info()` describes the executable itself, including what it
reports about its own build when run. Take a model created with
`cmdstan_model(exe_file = )` from an executable with no build record:
`$cpp_options()` is empty, since no options were given, but
`$build_info()` still reports whether the executable was built with
threading, OpenCL and so on.

## Usage

``` r
build_info()
```

## Value

See
[`stan_build_info()`](https://mc-stan.org/cmdstanr/pr/1287/reference/stan_build_info.md).

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
[`model-method-check_syntax`](https://mc-stan.org/cmdstanr/pr/1287/reference/model-method-check_syntax.md),
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
mod <- cmdstan_model(
  file.path(cmdstan_path(), "examples/bernoulli/bernoulli.stan"),
  cpp_options = list(stan_threads = TRUE)
)
info <- mod$build_info()
info
#> Build record: available
#> Reported features:
#>   stan_threads: TRUE
#>   stan_mpi: FALSE
#>   stan_opencl: FALSE
#>   stan_no_range_checks: FALSE
#>   stan_version: 2.40.0
#> Configuration:
#>   cpp_options: STAN_THREADS=TRUE
#>   stanc_options: none
#>   stanc_options_from_make: none
#>   include_paths: none
#> Dependencies:
#>   stan_file: /home/runner/.cmdstan/cmdstan-2.40.0/examples/bernoulli/bernoulli.stan
#>   make_local: /home/runner/.cmdstan/cmdstan-2.40.0/make/local
#> CmdStan 2.40.0 at /home/runner/.cmdstan/cmdstan-2.40.0
info$reported_features$stan_threads
#> [1] TRUE
# }
```
