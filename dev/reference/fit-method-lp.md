# Extract log probability (target)

The `$lp()` method extracts `lp__`, the total log probability (`target`)
accumulated in the model block of the Stan program. For variational
inference the log density of the variational approximation to the
posterior is available via the `$lp_approx()` method. For Laplace
approximation the unnormalized density of the approximation to the
posterior is available via the `$lp_approx()` method.

See the [Increment log density and Distribution
Statements](https://mc-stan.org/docs/reference-manual/statements.html)
sections of the Stan Reference Manual for details on when normalizing
constants are dropped from log probability calculations.

## Usage

``` r
lp()

lp_approx()

lp_approx()
```

## Value

A numeric vector with length equal to the number of (post-warmup) draws
or length equal to `1` for optimization.

## Details

`lp__` is the unnormalized log density on Stan's [unconstrained
space](https://mc-stan.org/docs/2_23/reference-manual/variable-transforms-chapter.html).
This will in general be different than the unnormalized model log
density evaluated at a posterior draw (which is on the constrained
space). `lp__` is intended to diagnose sampling efficiency and evaluate
approximations.

For variational inference `lp_approx__` is the log density of the
variational approximation to `lp__` (also on the unconstrained space).
It is exposed in the variational method for performing the checks
described in Yao et al. (2018) and implemented in the loo package.

For Laplace approximation `lp_approx__` is the unnormalized density of
the Laplace approximation. It can be used to perform the same checks as
in the case of the variational method described in Yao et al. (2018).

## References

Yao, Y., Vehtari, A., Simpson, D., and Gelman, A. (2018). Yes, but did
it work?: Evaluating variational inference. *Proceedings of the 35th
International Conference on Machine Learning*, PMLR 80:5581–5590.

## See also

[`CmdStanMCMC`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMCMC.md),
[`CmdStanMLE`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMLE.md),
[`CmdStanLaplace`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanLaplace.md),
[`CmdStanVB`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanVB.md)

## Examples

``` r
# \dontrun{
fit_mcmc <- cmdstanr_example("logistic")
head(fit_mcmc$lp())
#> [1] -67.13476 -67.31043 -64.79198 -64.96220 -67.98148 -66.84788

fit_mle <- cmdstanr_example("logistic", method = "optimize")
fit_mle$lp()
#> [1] -63.9218

fit_vb <- cmdstanr_example("logistic", method = "variational")
plot(fit_vb$lp(), fit_vb$lp_approx())

# }
```
