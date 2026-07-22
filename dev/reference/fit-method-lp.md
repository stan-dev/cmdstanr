# Extract log probability (target)

The `$lp()` method extracts `lp__`, the target log density evaluated by
Stan, up to an additive constant. For variational inference, Laplace
approximation, and Pathfinder, the log density of the corresponding
approximating distribution is available via `$lp_approx()`.

See the [Increment log density and Distribution
Statements](https://mc-stan.org/docs/reference-manual/statements.html)
sections of the Stan Reference Manual for details on when normalizing
constants are dropped from log probability calculations.

## Usage

``` r
lp()

lp_approx()
```

## Value

A numeric vector with length equal to the number of (post-warmup) draws
or length equal to `1` for optimization.

## Details

The target includes all contributions to the log probability, which can
come from the transformed parameters and model blocks, including certain
user-defined functions. The exact target represented by `lp__` depends
on the inference method:

- For MCMC sampling, variational inference, Pathfinder, and diagnostic
  mode, `lp__` is the log density on Stan's [unconstrained
  space](https://mc-stan.org/docs/reference-manual/transforms.html) and
  includes the Jacobian adjustments for constrained parameters.

- For optimization and Laplace approximation, whether the Jacobian
  adjustments are included depends on the `jacobian` argument.

For MCMC, `lp__` can be used to diagnose sampling efficiency; for
approximation methods, it can be used to evaluate the approximation.

For variational inference `lp_approx__` is the log density of the
variational approximation to `lp__` (also on the unconstrained space).
It is exposed in the variational method for performing the checks
described in Yao et al. (2018) and implemented in the loo package.

For Laplace approximation `lp_approx__` is CmdStan's `log_q__`: the
unnormalized log density of the normal approximation on the
unconstrained space. It can be used to perform the same checks as in the
case of the variational method described in Yao et al. (2018).

For Pathfinder `lp_approx__` is the log density of the approximating
distribution on the unconstrained space.

## References

Yao, Y., Vehtari, A., Simpson, D., and Gelman, A. (2018). Yes, but did
it work?: Evaluating variational inference. *Proceedings of the 35th
International Conference on Machine Learning*, PMLR 80:5581–5590.

## Examples

``` r
# \dontrun{
fit_mcmc <- cmdstanr_example("logistic")
head(fit_mcmc$lp())
#> [1] -67.48149 -70.15671 -65.15136 -66.78601 -66.59203 -67.35672

fit_mle <- cmdstanr_example("logistic", method = "optimize")
fit_mle$lp()
#> [1] -63.9218

fit_vb <- cmdstanr_example("logistic", method = "variational")
plot(fit_vb$lp(), fit_vb$lp_approx())


fit_pathfinder <- cmdstanr_example("logistic", method = "pathfinder")
plot(fit_pathfinder$lp(), fit_pathfinder$lp_approx())

# }
```
