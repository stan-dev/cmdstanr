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
variables in order to be able to return them to the user. The `$print()`
method accepts the same `variables` and `...` arguments as `$summary()`.
It also has a `digits` argument for the number of digits to display
after the decimal point (default `2`) and a `max_rows` argument for the
maximum number of rows to print (default
`getOption("cmdstanr_max_rows", 10)`). See **Examples**.

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

## Examples

``` r
# \dontrun{
fit <- cmdstanr_example("logistic")
fit$summary()
#> # A tibble: 105 × 10
#>    variable    mean  median    sd   mad       q5     q95  rhat ess_bulk ess_tail
#>    <chr>      <dbl>   <dbl> <dbl> <dbl>    <dbl>   <dbl> <dbl>    <dbl>    <dbl>
#>  1 lp__     -66.0   -65.6   1.53  1.25  -69.0    -64.3   1.00     2206.    2961.
#>  2 alpha      0.380   0.376 0.226 0.227   0.0232   0.761 1.000    4512.    2754.
#>  3 beta[1]   -0.670  -0.667 0.247 0.236  -1.09    -0.267 1.00     4255.    3113.
#>  4 beta[2]   -0.269  -0.268 0.232 0.232  -0.653    0.105 1.00     4213.    3268.
#>  5 beta[3]    0.676   0.671 0.269 0.269   0.247    1.14  1.000    4274.    2828.
#>  6 log_lik…  -0.515  -0.509 0.104 0.100  -0.693   -0.356 1.00     4531.    2861.
#>  7 log_lik…  -0.404  -0.382 0.152 0.143  -0.682   -0.194 1.00     4871.    3084.
#>  8 log_lik…  -0.497  -0.464 0.228 0.211  -0.933   -0.193 1.00     4274.    2848.
#>  9 log_lik…  -0.454  -0.436 0.157 0.152  -0.740   -0.234 1.00     4189.    2965.
#> 10 log_lik…  -1.18   -1.16  0.285 0.285  -1.66    -0.760 1.000    4595.    3089.
#> # ℹ 95 more rows
fit$print()
#>    variable   mean median   sd  mad     q5    q95 rhat ess_bulk ess_tail
#>  lp__       -66.02 -65.62 1.53 1.25 -68.98 -64.28 1.00     2206     2960
#>  alpha        0.38   0.38 0.23 0.23   0.02   0.76 1.00     4512     2754
#>  beta[1]     -0.67  -0.67 0.25 0.24  -1.09  -0.27 1.00     4255     3113
#>  beta[2]     -0.27  -0.27 0.23 0.23  -0.65   0.11 1.00     4213     3268
#>  beta[3]      0.68   0.67 0.27 0.27   0.25   1.14 1.00     4273     2828
#>  log_lik[1]  -0.52  -0.51 0.10 0.10  -0.69  -0.36 1.00     4530     2860
#>  log_lik[2]  -0.40  -0.38 0.15 0.14  -0.68  -0.19 1.00     4871     3083
#>  log_lik[3]  -0.50  -0.46 0.23 0.21  -0.93  -0.19 1.00     4274     2848
#>  log_lik[4]  -0.45  -0.44 0.16 0.15  -0.74  -0.23 1.00     4188     2964
#>  log_lik[5]  -1.18  -1.16 0.28 0.28  -1.66  -0.76 1.00     4595     3088
#> 
#>  # showing 10 of 105 rows (change via `max_rows` argument or `cmdstanr_max_rows` option)
fit$print(max_rows = 2) # same as print(fit, max_rows = 2)
#>  variable   mean median   sd  mad     q5    q95 rhat ess_bulk ess_tail
#>     lp__  -66.02 -65.62 1.53 1.25 -68.98 -64.28 1.00     2206     2960
#>     alpha   0.38   0.38 0.23 0.23   0.02   0.76 1.00     4512     2754
#> 
#>  # showing 2 of 105 rows (change via `max_rows` argument or `cmdstanr_max_rows` option)

# include only certain variables
fit$summary("beta")
#> # A tibble: 3 × 10
#>   variable   mean median    sd   mad     q5    q95  rhat ess_bulk ess_tail
#>   <chr>     <dbl>  <dbl> <dbl> <dbl>  <dbl>  <dbl> <dbl>    <dbl>    <dbl>
#> 1 beta[1]  -0.670 -0.667 0.247 0.236 -1.09  -0.267 1.00     4255.    3113.
#> 2 beta[2]  -0.269 -0.268 0.232 0.232 -0.653  0.105 1.00     4213.    3268.
#> 3 beta[3]   0.676  0.671 0.269 0.269  0.247  1.14  1.000    4274.    2828.
fit$print(c("alpha", "beta[2]"))
#>  variable  mean median   sd  mad    q5  q95 rhat ess_bulk ess_tail
#>   alpha    0.38   0.38 0.23 0.23  0.02 0.76 1.00     4512     2754
#>   beta[2] -0.27  -0.27 0.23 0.23 -0.65 0.11 1.00     4213     3268

# include all variables but only certain summaries
fit$summary(NULL, c("mean", "sd"))
#> # A tibble: 105 × 3
#>    variable      mean    sd
#>    <chr>        <dbl> <dbl>
#>  1 lp__       -66.0   1.53 
#>  2 alpha        0.380 0.226
#>  3 beta[1]     -0.670 0.247
#>  4 beta[2]     -0.269 0.232
#>  5 beta[3]      0.676 0.269
#>  6 log_lik[1]  -0.515 0.104
#>  7 log_lik[2]  -0.404 0.152
#>  8 log_lik[3]  -0.497 0.228
#>  9 log_lik[4]  -0.454 0.157
#> 10 log_lik[5]  -1.18  0.285
#> # ℹ 95 more rows

# can use functions created from formulas
# for example, calculate Pr(beta > 0)
fit$summary("beta", prob_gt_0 = ~ mean(. > 0))
#> # A tibble: 3 × 2
#>   variable prob_gt_0
#>   <chr>        <dbl>
#> 1 beta[1]    0.00175
#> 2 beta[2]    0.124  
#> 3 beta[3]    0.995  

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
#> 1 alpha     0.380  0.376 0.226 0.227 -0.0479  0.834 1.000    4512.    2754.
#> 2 beta[1]  -0.670 -0.667 0.247 0.236 -1.19   -0.190 1.00     4255.    3113.
#> 3 beta[2]  -0.269 -0.268 0.232 0.232 -0.732   0.163 1.00     4213.    3268.
#> 4 beta[3]   0.676  0.671 0.269 0.269  0.163   1.22  1.000    4274.    2828.

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
