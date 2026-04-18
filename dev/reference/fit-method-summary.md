# Compute a summary table of estimates and diagnostics

The `$summary()` method runs
[`summarise_draws()`](https://mc-stan.org/posterior/reference/draws_summary.html)
from the posterior package and returns the output. For MCMC, only
post-warmup draws are included in the summary.

There is also a `$print()` method that prints the same summary stats but
removes the extra formatting used for printing tibbles and returns the
fitted model object itself. The `$print()` method may also be faster
than `$summary()` because it is designed to only compute the summary
statistics for the variables that will actually fit in the printed
output whereas `$summary()` will compute them for all of the specified
variables in order to be able to return them to the user. See
**Examples**.

## Usage

``` r
summary(variables = NULL, ...)
```

## Arguments

- variables:

  (character vector) The variables to include.

- ...:

  Optional arguments to pass to
  [`posterior::summarise_draws()`](https://mc-stan.org/posterior/reference/draws_summary.html).

## Value

The `$summary()` method returns the tibble data frame created by
[`posterior::summarise_draws()`](https://mc-stan.org/posterior/reference/draws_summary.html).

The `$print()` method returns the fitted model object itself
(invisibly), which is the standard behavior for print methods in R.

## References

- Vehtari, A., Gelman, A., Simpson, D., Carpenter, B., and Buerkner,
  P.-C. (2021). Rank-normalization, folding, and localization: An
  improved R-hat for assessing convergence of MCMC (with discussion).
  *Bayesian Analysis*, 16(2), 667-718. doi:10.1214/20-BA1221.

- Vehtari, A. (2021). Comparison of MCMC effective sample size
  estimators. https://avehtari.github.io/rhat_ess/ess_comparison.html
  (for ESS diagnostics such as `ess_bulk` and `ess_tail`).

## See also

[`CmdStanMCMC`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMCMC.md),
[`CmdStanMLE`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMLE.md),
[`CmdStanLaplace`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanLaplace.md),
[`CmdStanVB`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanVB.md),
[`CmdStanGQ`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanGQ.md)

## Examples

``` r
# \dontrun{
fit <- cmdstanr_example("logistic")
fit$summary()
#> # A tibble: 105 × 10
#>    variable      mean  median     sd    mad       q5      q95  rhat ess_bulk
#>    <chr>        <dbl>   <dbl>  <dbl>  <dbl>    <dbl>    <dbl> <dbl>    <dbl>
#>  1 lp__       -65.9   -65.6   1.39   1.19   -68.6    -64.3    1.00     2131.
#>  2 alpha        0.382   0.381 0.213  0.215    0.0362   0.741  1.00     4458.
#>  3 beta[1]     -0.668  -0.667 0.246  0.242   -1.08    -0.265  1.000    4150.
#>  4 beta[2]     -0.275  -0.276 0.220  0.217   -0.635    0.0858 1.000    4363.
#>  5 beta[3]      0.676   0.671 0.269  0.273    0.247    1.12   1.00     4119.
#>  6 log_lik[1]  -0.514  -0.508 0.0974 0.0958  -0.684   -0.363  1.00     4437.
#>  7 log_lik[2]  -0.406  -0.388 0.149  0.146   -0.677   -0.200  1.00     4181.
#>  8 log_lik[3]  -0.497  -0.467 0.213  0.205   -0.877   -0.211  1.00     4312.
#>  9 log_lik[4]  -0.451  -0.430 0.151  0.146   -0.729   -0.242  1.000    4283.
#> 10 log_lik[5]  -1.18   -1.16  0.277  0.283   -1.67    -0.764  1.00     4597.
#> # ℹ 95 more rows
#> # ℹ 1 more variable: ess_tail <dbl>
fit$print()
#>    variable   mean median   sd  mad     q5    q95 rhat ess_bulk ess_tail
#>  lp__       -65.90 -65.57 1.39 1.19 -68.56 -64.26 1.00     2130     2797
#>  alpha        0.38   0.38 0.21 0.21   0.04   0.74 1.00     4457     2747
#>  beta[1]     -0.67  -0.67 0.25 0.24  -1.08  -0.26 1.00     4150     2922
#>  beta[2]     -0.28  -0.28 0.22 0.22  -0.64   0.09 1.00     4362     3148
#>  beta[3]      0.68   0.67 0.27 0.27   0.25   1.12 1.00     4119     2775
#>  log_lik[1]  -0.51  -0.51 0.10 0.10  -0.68  -0.36 1.00     4437     3160
#>  log_lik[2]  -0.41  -0.39 0.15 0.15  -0.68  -0.20 1.00     4181     2983
#>  log_lik[3]  -0.50  -0.47 0.21 0.20  -0.88  -0.21 1.00     4311     3181
#>  log_lik[4]  -0.45  -0.43 0.15 0.15  -0.73  -0.24 1.00     4282     3089
#>  log_lik[5]  -1.18  -1.16 0.28 0.28  -1.67  -0.76 1.00     4596     2947
#> 
#>  # showing 10 of 105 rows (change via 'max_rows' argument or 'cmdstanr_max_rows' option)
fit$print(max_rows = 2) # same as print(fit, max_rows = 2)
#>  variable   mean median   sd  mad     q5    q95 rhat ess_bulk ess_tail
#>     lp__  -65.90 -65.57 1.39 1.19 -68.56 -64.26 1.00     2130     2797
#>     alpha   0.38   0.38 0.21 0.21   0.04   0.74 1.00     4457     2747
#> 
#>  # showing 2 of 105 rows (change via 'max_rows' argument or 'cmdstanr_max_rows' option)

# include only certain variables
fit$summary("beta")
#> # A tibble: 3 × 10
#>   variable   mean median    sd   mad     q5     q95  rhat ess_bulk ess_tail
#>   <chr>     <dbl>  <dbl> <dbl> <dbl>  <dbl>   <dbl> <dbl>    <dbl>    <dbl>
#> 1 beta[1]  -0.668 -0.667 0.246 0.242 -1.08  -0.265  1.000    4150.    2923.
#> 2 beta[2]  -0.275 -0.276 0.220 0.217 -0.635  0.0858 1.000    4363.    3148.
#> 3 beta[3]   0.676  0.671 0.269 0.273  0.247  1.12   1.00     4119.    2775.
fit$print(c("alpha", "beta[2]"))
#>  variable  mean median   sd  mad    q5  q95 rhat ess_bulk ess_tail
#>   alpha    0.38   0.38 0.21 0.21  0.04 0.74 1.00     4457     2747
#>   beta[2] -0.28  -0.28 0.22 0.22 -0.64 0.09 1.00     4362     3148

# include all variables but only certain summaries
fit$summary(NULL, c("mean", "sd"))
#> # A tibble: 105 × 3
#>    variable      mean     sd
#>    <chr>        <dbl>  <dbl>
#>  1 lp__       -65.9   1.39  
#>  2 alpha        0.382 0.213 
#>  3 beta[1]     -0.668 0.246 
#>  4 beta[2]     -0.275 0.220 
#>  5 beta[3]      0.676 0.269 
#>  6 log_lik[1]  -0.514 0.0974
#>  7 log_lik[2]  -0.406 0.149 
#>  8 log_lik[3]  -0.497 0.213 
#>  9 log_lik[4]  -0.451 0.151 
#> 10 log_lik[5]  -1.18  0.277 
#> # ℹ 95 more rows

# can use functions created from formulas
# for example, calculate Pr(beta > 0)
fit$summary("beta", prob_gt_0 = ~ mean(. > 0))
#> # A tibble: 3 × 2
#>   variable prob_gt_0
#>   <chr>        <dbl>
#> 1 beta[1]      0.003
#> 2 beta[2]      0.104
#> 3 beta[3]      0.996

# can combine user-specified functions with
# the default summary functions
fit$summary(variables = c("alpha", "beta"),
  posterior::default_summary_measures()[1:4],
  quantiles = ~ quantile2(., probs = c(0.025, 0.975)),
  posterior::default_convergence_measures()
  )
#> # A tibble: 4 × 10
#>   variable   mean median    sd   mad    q2.5  q97.5  rhat ess_bulk ess_tail
#>   <chr>     <dbl>  <dbl> <dbl> <dbl>   <dbl>  <dbl> <dbl>    <dbl>    <dbl>
#> 1 alpha     0.382  0.381 0.213 0.215 -0.0227  0.803 1.00     4458.    2747.
#> 2 beta[1]  -0.668 -0.667 0.246 0.242 -1.16   -0.186 1.000    4150.    2923.
#> 3 beta[2]  -0.275 -0.276 0.220 0.217 -0.706   0.148 1.000    4363.    3148.
#> 4 beta[3]   0.676  0.671 0.269 0.273  0.165   1.20  1.00     4119.    2775.

# the functions need to calculate the appropriate
# value for a matrix input
fit$summary(variables = "alpha", dim)
#> # A tibble: 1 × 3
#>   variable dim.1 dim.2
#>   <chr>    <int> <int>
#> 1 alpha     1000     4

# the usual [stats::var()] is therefore not directly suitable as it
# will produce a covariance matrix unless the data is converted to a vector
fit$print(c("alpha", "beta"), var2 = ~var(as.vector(.x)))
#>  variable var2
#>   alpha   0.05
#>   beta[1] 0.06
#>   beta[2] 0.05
#>   beta[3] 0.07

# }
```
