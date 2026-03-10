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
#>  1 lp__       -66.0   -65.6   1.46   1.21   -68.7    -64.3    1.00     2042.
#>  2 alpha        0.380   0.380 0.221  0.222    0.0159   0.741  1.00     4617.
#>  3 beta[1]     -0.666  -0.659 0.250  0.240   -1.09    -0.266  1.00     4050.
#>  4 beta[2]     -0.267  -0.261 0.221  0.221   -0.641    0.0897 1.00     3743.
#>  5 beta[3]      0.678   0.669 0.269  0.270    0.257    1.13   1.00     4223.
#>  6 log_lik[1]  -0.515  -0.508 0.0997 0.0998  -0.683   -0.360  1.00     4567.
#>  7 log_lik[2]  -0.403  -0.386 0.146  0.138   -0.670   -0.195  1.00     4777.
#>  8 log_lik[3]  -0.492  -0.459 0.212  0.207   -0.882   -0.211  1.000    4012.
#>  9 log_lik[4]  -0.453  -0.432 0.153  0.147   -0.738   -0.238  1.00     3942.
#> 10 log_lik[5]  -1.18   -1.16  0.284  0.277   -1.70    -0.759  1.00     4734.
#> # ℹ 95 more rows
#> # ℹ 1 more variable: ess_tail <dbl>
fit$print()
#>    variable   mean median   sd  mad     q5    q95 rhat ess_bulk ess_tail
#>  lp__       -65.95 -65.60 1.46 1.21 -68.70 -64.28 1.00     2042     2202
#>  alpha        0.38   0.38 0.22 0.22   0.02   0.74 1.00     4617     2513
#>  beta[1]     -0.67  -0.66 0.25 0.24  -1.09  -0.27 1.00     4049     2622
#>  beta[2]     -0.27  -0.26 0.22 0.22  -0.64   0.09 1.00     3743     3115
#>  beta[3]      0.68   0.67 0.27 0.27   0.26   1.13 1.00     4222     3063
#>  log_lik[1]  -0.51  -0.51 0.10 0.10  -0.68  -0.36 1.00     4567     2801
#>  log_lik[2]  -0.40  -0.39 0.15 0.14  -0.67  -0.19 1.00     4777     3294
#>  log_lik[3]  -0.49  -0.46 0.21 0.21  -0.88  -0.21 1.00     4012     2826
#>  log_lik[4]  -0.45  -0.43 0.15 0.15  -0.74  -0.24 1.00     3942     3298
#>  log_lik[5]  -1.18  -1.16 0.28 0.28  -1.70  -0.76 1.00     4733     3146
#> 
#>  # showing 10 of 105 rows (change via 'max_rows' argument or 'cmdstanr_max_rows' option)
fit$print(max_rows = 2) # same as print(fit, max_rows = 2)
#>  variable   mean median   sd  mad     q5    q95 rhat ess_bulk ess_tail
#>     lp__  -65.95 -65.60 1.46 1.21 -68.70 -64.28 1.00     2042     2202
#>     alpha   0.38   0.38 0.22 0.22   0.02   0.74 1.00     4617     2513
#> 
#>  # showing 2 of 105 rows (change via 'max_rows' argument or 'cmdstanr_max_rows' option)

# include only certain variables
fit$summary("beta")
#> # A tibble: 3 × 10
#>   variable   mean median    sd   mad     q5     q95  rhat ess_bulk ess_tail
#>   <chr>     <dbl>  <dbl> <dbl> <dbl>  <dbl>   <dbl> <dbl>    <dbl>    <dbl>
#> 1 beta[1]  -0.666 -0.659 0.250 0.240 -1.09  -0.266   1.00    4050.    2623.
#> 2 beta[2]  -0.267 -0.261 0.221 0.221 -0.641  0.0897  1.00    3743.    3116.
#> 3 beta[3]   0.678  0.669 0.269 0.270  0.257  1.13    1.00    4223.    3063.
fit$print(c("alpha", "beta[2]"))
#>  variable  mean median   sd  mad    q5  q95 rhat ess_bulk ess_tail
#>   alpha    0.38   0.38 0.22 0.22  0.02 0.74 1.00     4617     2513
#>   beta[2] -0.27  -0.26 0.22 0.22 -0.64 0.09 1.00     3743     3115

# include all variables but only certain summaries
fit$summary(NULL, c("mean", "sd"))
#> # A tibble: 105 × 3
#>    variable      mean     sd
#>    <chr>        <dbl>  <dbl>
#>  1 lp__       -66.0   1.46  
#>  2 alpha        0.380 0.221 
#>  3 beta[1]     -0.666 0.250 
#>  4 beta[2]     -0.267 0.221 
#>  5 beta[3]      0.678 0.269 
#>  6 log_lik[1]  -0.515 0.0997
#>  7 log_lik[2]  -0.403 0.146 
#>  8 log_lik[3]  -0.492 0.212 
#>  9 log_lik[4]  -0.453 0.153 
#> 10 log_lik[5]  -1.18  0.284 
#> # ℹ 95 more rows

# can use functions created from formulas
# for example, calculate Pr(beta > 0)
fit$summary("beta", prob_gt_0 = ~ mean(. > 0))
#> # A tibble: 3 × 2
#>   variable prob_gt_0
#>   <chr>        <dbl>
#> 1 beta[1]      0.003
#> 2 beta[2]      0.112
#> 3 beta[3]      0.997

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
#> 1 alpha     0.380  0.380 0.221 0.222 -0.0409  0.813  1.00    4617.    2513.
#> 2 beta[1]  -0.666 -0.659 0.250 0.240 -1.18   -0.186  1.00    4050.    2623.
#> 3 beta[2]  -0.267 -0.261 0.221 0.221 -0.720   0.151  1.00    3743.    3116.
#> 4 beta[3]   0.678  0.669 0.269 0.270  0.176   1.22   1.00    4223.    3063.

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
