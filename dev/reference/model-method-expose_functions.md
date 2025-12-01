# Expose Stan functions to R

The `$expose_functions()` method of a
[`CmdStanModel`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanModel.md)
object will compile the functions in the Stan program's `functions`
block and expose them for use in R. This can also be specified via the
`compile_standalone` argument to the
[`$compile()`](https://mc-stan.org/cmdstanr/dev/reference/model-method-compile.md)
method.

This method is also available for fitted model objects
([`CmdStanMCMC`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMCMC.md),
[`CmdStanVB`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanVB.md),
etc.). See **Examples**.

Note: there may be many compiler warnings emitted during compilation but
these can be ignored so long as they are warnings and not errors.

## Usage

``` r
expose_functions(global = FALSE, verbose = FALSE)
```

## Arguments

- global:

  (logical) Should the functions be added to the Global Environment? The
  default is `FALSE`, in which case the functions are available via the
  `functions` field of the R6 object.

- verbose:

  (logical) Should detailed information about generated code be printed
  to the console? Defaults to `FALSE`.

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
stan_file <- write_stan_file(
 "
 functions {
   real a_plus_b(real a, real b) {
     return a + b;
   }
 }
 parameters {
   real x;
 }
 model {
   x ~ std_normal();
 }
 "
)
mod <- cmdstan_model(stan_file)
mod$expose_functions()
mod$functions$a_plus_b(1, 2)
#> [1] 3

fit <- mod$sample(refresh = 0)
#> Running MCMC with 4 sequential chains...
#> 
#> Chain 1 finished in 0.0 seconds.
#> Chain 2 finished in 0.0 seconds.
#> Chain 3 finished in 0.0 seconds.
#> Chain 4 finished in 0.0 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 0.0 seconds.
#> Total execution time: 0.5 seconds.
#> 
fit$expose_functions() # already compiled because of above but this would compile them otherwise
#> Functions already compiled, nothing to do!
fit$functions$a_plus_b(1, 2)
#> [1] 3
# }

```
