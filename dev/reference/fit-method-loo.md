# Leave-one-out cross-validation (LOO-CV)

The `$loo()` method computes approximate LOO-CV using the loo package.
In order to use this method you must compute and save the pointwise
log-likelihood in your Stan program. See
[`loo::loo.array()`](https://mc-stan.org/loo/reference/loo.html) and the
loo package [vignettes](https://mc-stan.org/loo/articles/) for details.

## Usage

``` r
loo(variables = "log_lik", r_eff = FALSE, moment_match = FALSE, ...)
```

## Arguments

- variables:

  (string) The name of the variable in the Stan program containing the
  pointwise log-likelihood. The default is to look for `"log_lik"`. This
  argument is passed to the
  [`$draws()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-draws.md)
  method.

- r_eff:

  (multiple options) How to handle the `r_eff` argument for `loo()`.
  `r_eff` measures the amount of autocorrelation in MCMC draws, and is
  used to compute more accurate ESS and MCSE estimates for pointwise and
  total ELPDs.

  - `TRUE` will call
    [`loo::relative_eff.array()`](https://mc-stan.org/loo/reference/relative_eff.html)
    to compute the `r_eff` argument to pass to
    [`loo::loo.array()`](https://mc-stan.org/loo/reference/loo.html).

  - `FALSE` (the default) or `NULL` will avoid computing `r_eff`, which
    can be very slow. The reported ESS and MCSE estimates may be
    over-optimistic if the posterior draws are far from independent.

  - If `r_eff` is anything else, that object will be passed as the
    `r_eff` argument to
    [`loo::loo.array()`](https://mc-stan.org/loo/reference/loo.html).

- moment_match:

  (logical) Whether to use a
  [moment-matching](https://mc-stan.org/loo/reference/loo_moment_match.html)
  correction for problematic observations. The default is `FALSE`. Using
  `moment_match=TRUE` will result in compiling the additional methods
  described in
  [fit-method-init_model_methods](https://mc-stan.org/cmdstanr/dev/reference/fit-method-init_model_methods.md).
  This allows CmdStanR to automatically supply the functions for the
  `log_lik_i`, `unconstrain_pars`, `log_prob_upars`, and
  `log_lik_i_upars` arguments to
  [`loo::loo_moment_match()`](https://mc-stan.org/loo/reference/loo_moment_match.html).

- ...:

  Other arguments (e.g., `cores`, `save_psis`, etc.) passed to
  [`loo::loo.array()`](https://mc-stan.org/loo/reference/loo.html) or
  [`loo::loo_moment_match.default()`](https://mc-stan.org/loo/reference/loo_moment_match.html)
  (if `moment_match` = `TRUE` is set).

## Value

The object returned by
[`loo::loo.array()`](https://mc-stan.org/loo/reference/loo.html) or
[`loo::loo_moment_match.default()`](https://mc-stan.org/loo/reference/loo_moment_match.html).

## See also

The loo package website with
[documentation](https://mc-stan.org/loo/reference/index.html) and
[vignettes](https://mc-stan.org/loo/articles/).

## Examples

``` r
# \dontrun{
# the "logistic" example model has "log_lik" in generated quantities
fit <- cmdstanr_example("logistic")
loo_result <- fit$loo(cores = 2)
print(loo_result)
#> 
#> Computed from 4000 by 100 log-likelihood matrix.
#> 
#>          Estimate  SE
#> elpd_loo    -63.6 4.1
#> p_loo         3.9 0.5
#> looic       127.3 8.3
#> ------
#> MCSE of elpd_loo is 0.0.
#> MCSE and ESS estimates assume independent draws (r_eff=1).
#> 
#> All Pareto k estimates are good (k < 0.7).
#> See help('pareto-k-diagnostic') for details.
# }
```
