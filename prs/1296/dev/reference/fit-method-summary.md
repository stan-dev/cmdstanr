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
#>    variable      mean  median     sd    mad       q5      q95  rhat ess_bulk
#>    <chr>        <dbl>   <dbl>  <dbl>  <dbl>    <dbl>    <dbl> <dbl>    <dbl>
#>  1 lp__       -66.0   -65.6   1.46   1.21   -68.7    -64.3     1.00    2071.
#>  2 alpha        0.380   0.377 0.217  0.215    0.0281   0.741   1.00    3882.
#>  3 beta[1]     -0.673  -0.664 0.248  0.241   -1.09    -0.265   1.00    4072.
#>  4 beta[2]     -0.277  -0.273 0.222  0.218   -0.639    0.0866  1.00    3298.
#>  5 beta[3]      0.683   0.674 0.274  0.273    0.251    1.14    1.00    3921.
#>  6 log_lik[1]  -0.515  -0.510 0.0973 0.0947  -0.685   -0.366   1.00    3846.
#>  7 log_lik[2]  -0.401  -0.384 0.147  0.143   -0.672   -0.194   1.00    4235.
#>  8 log_lik[3]  -0.495  -0.462 0.212  0.194   -0.893   -0.214   1.00    3370.
#>  9 log_lik[4]  -0.448  -0.433 0.152  0.148   -0.727   -0.230   1.00    3161.
#> 10 log_lik[5]  -1.18   -1.16  0.280  0.268   -1.68    -0.763   1.00    3992.
#> # ℹ 95 more rows
#> # ℹ 1 more variable: ess_tail <dbl>
fit$print()
#>    variable   mean median   sd  mad     q5    q95 rhat ess_bulk ess_tail
#>  lp__       -65.95 -65.63 1.46 1.21 -68.68 -64.27 1.00     2070     2599
#>  alpha        0.38   0.38 0.22 0.21   0.03   0.74 1.00     3882     2848
#>  beta[1]     -0.67  -0.66 0.25 0.24  -1.09  -0.27 1.00     4072     2345
#>  beta[2]     -0.28  -0.27 0.22 0.22  -0.64   0.09 1.00     3297     2601
#>  beta[3]      0.68   0.67 0.27 0.27   0.25   1.14 1.00     3921     3125
#>  log_lik[1]  -0.51  -0.51 0.10 0.09  -0.68  -0.37 1.00     3845     2859
#>  log_lik[2]  -0.40  -0.38 0.15 0.14  -0.67  -0.19 1.00     4234     2907
#>  log_lik[3]  -0.50  -0.46 0.21 0.19  -0.89  -0.21 1.00     3369     2850
#>  log_lik[4]  -0.45  -0.43 0.15 0.15  -0.73  -0.23 1.00     3160     2465
#>  log_lik[5]  -1.18  -1.16 0.28 0.27  -1.68  -0.76 1.00     3992     3280
#> 
#>  # showing 10 of 105 rows (change via 'max_rows' argument or 'cmdstanr_max_rows' option)
fit$print(max_rows = 2) # same as print(fit, max_rows = 2)
#>  variable   mean median   sd  mad     q5    q95 rhat ess_bulk ess_tail
#>     lp__  -65.95 -65.63 1.46 1.21 -68.68 -64.27 1.00     2070     2599
#>     alpha   0.38   0.38 0.22 0.21   0.03   0.74 1.00     3882     2848
#> 
#>  # showing 2 of 105 rows (change via 'max_rows' argument or 'cmdstanr_max_rows' option)

# include only certain variables
fit$summary("beta")
#> # A tibble: 3 × 10
#>   variable   mean median    sd   mad     q5     q95  rhat ess_bulk ess_tail
#>   <chr>     <dbl>  <dbl> <dbl> <dbl>  <dbl>   <dbl> <dbl>    <dbl>    <dbl>
#> 1 beta[1]  -0.673 -0.664 0.248 0.241 -1.09  -0.265   1.00    4072.    2346.
#> 2 beta[2]  -0.277 -0.273 0.222 0.218 -0.639  0.0866  1.00    3298.    2601.
#> 3 beta[3]   0.683  0.674 0.274 0.273  0.251  1.14    1.00    3921.    3126.
fit$print(c("alpha", "beta[2]"))
#>  variable  mean median   sd  mad    q5  q95 rhat ess_bulk ess_tail
#>   alpha    0.38   0.38 0.22 0.21  0.03 0.74 1.00     3882     2848
#>   beta[2] -0.28  -0.27 0.22 0.22 -0.64 0.09 1.00     3297     2601

# include all variables but only certain summaries
fit$summary(NULL, c("mean", "sd"))
#> # A tibble: 105 × 3
#>    variable      mean     sd
#>    <chr>        <dbl>  <dbl>
#>  1 lp__       -66.0   1.46  
#>  2 alpha        0.380 0.217 
#>  3 beta[1]     -0.673 0.248 
#>  4 beta[2]     -0.277 0.222 
#>  5 beta[3]      0.683 0.274 
#>  6 log_lik[1]  -0.515 0.0973
#>  7 log_lik[2]  -0.401 0.147 
#>  8 log_lik[3]  -0.495 0.212 
#>  9 log_lik[4]  -0.448 0.152 
#> 10 log_lik[5]  -1.18  0.280 
#> # ℹ 95 more rows

# can use functions created from formulas
# for example, calculate Pr(beta > 0)
fit$summary("beta", prob_gt_0 = ~ mean(. > 0))
#> # A tibble: 3 × 2
#>   variable prob_gt_0
#>   <chr>        <dbl>
#> 1 beta[1]      0.002
#> 2 beta[2]      0.104
#> 3 beta[3]      0.994

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
#> 1 alpha     0.380  0.377 0.217 0.215 -0.0355  0.808  1.00    3882.    2848.
#> 2 beta[1]  -0.673 -0.664 0.248 0.241 -1.18   -0.194  1.00    4072.    2346.
#> 3 beta[2]  -0.277 -0.273 0.222 0.218 -0.709   0.155  1.00    3298.    2601.
#> 4 beta[3]   0.683  0.674 0.274 0.273  0.158   1.25   1.00    3921.    3126.

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
