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
#>    variable      mean  median    sd   mad        q5      q95  rhat ess_bulk
#>    <chr>        <dbl>   <dbl> <dbl> <dbl>     <dbl>    <dbl> <dbl>    <dbl>
#>  1 lp__       -66.0   -65.7   1.49  1.26  -68.9     -64.3    1.00     1844.
#>  2 alpha        0.369   0.365 0.228 0.225   0.00283   0.761  1.00     3980.
#>  3 beta[1]     -0.670  -0.667 0.249 0.246  -1.10     -0.271  1.00     4259.
#>  4 beta[2]     -0.274  -0.273 0.228 0.231  -0.650     0.0987 1.00     4272.
#>  5 beta[3]      0.679   0.672 0.272 0.274   0.238     1.13   1.00     4091.
#>  6 log_lik[1]  -0.520  -0.514 0.104 0.104  -0.699    -0.359  1.00     4032.
#>  7 log_lik[2]  -0.400  -0.380 0.150 0.141  -0.677    -0.189  1.00     4524.
#>  8 log_lik[3]  -0.502  -0.469 0.221 0.210  -0.905    -0.208  1.00     4378.
#>  9 log_lik[4]  -0.448  -0.429 0.158 0.156  -0.728    -0.226  1.000    3851.
#> 10 log_lik[5]  -1.17   -1.15  0.281 0.278  -1.68     -0.750  1.00     4503.
#> # ℹ 95 more rows
#> # ℹ 1 more variable: ess_tail <dbl>
fit$print()
#>    variable   mean median   sd  mad     q5    q95 rhat ess_bulk ess_tail
#>  lp__       -66.03 -65.66 1.49 1.26 -68.92 -64.33 1.00     1844     2567
#>  alpha        0.37   0.36 0.23 0.23   0.00   0.76 1.00     3980     2887
#>  beta[1]     -0.67  -0.67 0.25 0.25  -1.10  -0.27 1.00     4258     2938
#>  beta[2]     -0.27  -0.27 0.23 0.23  -0.65   0.10 1.00     4272     3229
#>  beta[3]      0.68   0.67 0.27 0.27   0.24   1.13 1.00     4090     2723
#>  log_lik[1]  -0.52  -0.51 0.10 0.10  -0.70  -0.36 1.00     4032     2296
#>  log_lik[2]  -0.40  -0.38 0.15 0.14  -0.68  -0.19 1.00     4524     2555
#>  log_lik[3]  -0.50  -0.47 0.22 0.21  -0.90  -0.21 1.00     4378     2729
#>  log_lik[4]  -0.45  -0.43 0.16 0.16  -0.73  -0.23 1.00     3851     2707
#>  log_lik[5]  -1.17  -1.15 0.28 0.28  -1.68  -0.75 1.00     4502     2844
#> 
#>  # showing 10 of 105 rows (change via 'max_rows' argument or 'cmdstanr_max_rows' option)
fit$print(max_rows = 2) # same as print(fit, max_rows = 2)
#>  variable   mean median   sd  mad     q5    q95 rhat ess_bulk ess_tail
#>     lp__  -66.03 -65.66 1.49 1.26 -68.92 -64.33 1.00     1844     2567
#>     alpha   0.37   0.36 0.23 0.23   0.00   0.76 1.00     3980     2887
#> 
#>  # showing 2 of 105 rows (change via 'max_rows' argument or 'cmdstanr_max_rows' option)

# include only certain variables
fit$summary("beta")
#> # A tibble: 3 × 10
#>   variable   mean median    sd   mad     q5     q95  rhat ess_bulk ess_tail
#>   <chr>     <dbl>  <dbl> <dbl> <dbl>  <dbl>   <dbl> <dbl>    <dbl>    <dbl>
#> 1 beta[1]  -0.670 -0.667 0.249 0.246 -1.10  -0.271   1.00    4259.    2939.
#> 2 beta[2]  -0.274 -0.273 0.228 0.231 -0.650  0.0987  1.00    4272.    3229.
#> 3 beta[3]   0.679  0.672 0.272 0.274  0.238  1.13    1.00    4091.    2723.
fit$print(c("alpha", "beta[2]"))
#>  variable  mean median   sd  mad    q5  q95 rhat ess_bulk ess_tail
#>   alpha    0.37   0.36 0.23 0.23  0.00 0.76 1.00     3980     2887
#>   beta[2] -0.27  -0.27 0.23 0.23 -0.65 0.10 1.00     4272     3229

# include all variables but only certain summaries
fit$summary(NULL, c("mean", "sd"))
#> # A tibble: 105 × 3
#>    variable      mean    sd
#>    <chr>        <dbl> <dbl>
#>  1 lp__       -66.0   1.49 
#>  2 alpha        0.369 0.228
#>  3 beta[1]     -0.670 0.249
#>  4 beta[2]     -0.274 0.228
#>  5 beta[3]      0.679 0.272
#>  6 log_lik[1]  -0.520 0.104
#>  7 log_lik[2]  -0.400 0.150
#>  8 log_lik[3]  -0.502 0.221
#>  9 log_lik[4]  -0.448 0.158
#> 10 log_lik[5]  -1.17  0.281
#> # ℹ 95 more rows

# can use functions created from formulas
# for example, calculate Pr(beta > 0)
fit$summary("beta", prob_gt_0 = ~ mean(. > 0))
#> # A tibble: 3 × 2
#>   variable prob_gt_0
#>   <chr>        <dbl>
#> 1 beta[1]    0.00125
#> 2 beta[2]    0.114  
#> 3 beta[3]    0.994  

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
#> 1 alpha     0.369  0.365 0.228 0.225 -0.0644  0.841  1.00    3980.    2887.
#> 2 beta[1]  -0.670 -0.667 0.249 0.246 -1.19   -0.194  1.00    4259.    2939.
#> 3 beta[2]  -0.274 -0.273 0.228 0.231 -0.710   0.176  1.00    4272.    3229.
#> 4 beta[3]   0.679  0.672 0.272 0.274  0.147   1.23   1.00    4091.    2723.

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
