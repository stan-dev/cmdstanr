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
#>    variable      mean  median     sd   mad       q5      q95  rhat ess_bulk
#>    <chr>        <dbl>   <dbl>  <dbl> <dbl>    <dbl>    <dbl> <dbl>    <dbl>
#>  1 lp__       -66.0   -65.7   1.39   1.24  -68.7    -64.3    1.000    2291.
#>  2 alpha        0.374   0.375 0.220  0.221   0.0112   0.738  1.00     4133.
#>  3 beta[1]     -0.666  -0.664 0.248  0.249  -1.08    -0.257  1.00     4471.
#>  4 beta[2]     -0.269  -0.267 0.226  0.223  -0.638    0.0973 1.00     4317.
#>  5 beta[3]      0.675   0.670 0.269  0.279   0.247    1.12   1.00     3731.
#>  6 log_lik[1]  -0.517  -0.511 0.0992 0.101  -0.691   -0.366  1.00     4316.
#>  7 log_lik[2]  -0.402  -0.384 0.145  0.135  -0.669   -0.198  1.000    4685.
#>  8 log_lik[3]  -0.497  -0.459 0.219  0.203  -0.903   -0.207  1.000    4355.
#>  9 log_lik[4]  -0.451  -0.432 0.151  0.147  -0.719   -0.238  1.00     3915.
#> 10 log_lik[5]  -1.18   -1.16  0.285  0.280  -1.68    -0.750  1.00     4011.
#> # ℹ 95 more rows
#> # ℹ 1 more variable: ess_tail <dbl>
fit$print()
#>    variable   mean median   sd  mad     q5    q95 rhat ess_bulk ess_tail
#>  lp__       -65.97 -65.67 1.39 1.24 -68.70 -64.29 1.00     2291     3113
#>  alpha        0.37   0.37 0.22 0.22   0.01   0.74 1.00     4133     3016
#>  beta[1]     -0.67  -0.66 0.25 0.25  -1.08  -0.26 1.00     4471     2959
#>  beta[2]     -0.27  -0.27 0.23 0.22  -0.64   0.10 1.00     4317     3349
#>  beta[3]      0.67   0.67 0.27 0.28   0.25   1.12 1.00     3731     3333
#>  log_lik[1]  -0.52  -0.51 0.10 0.10  -0.69  -0.37 1.00     4316     3363
#>  log_lik[2]  -0.40  -0.38 0.15 0.14  -0.67  -0.20 1.00     4684     3591
#>  log_lik[3]  -0.50  -0.46 0.22 0.20  -0.90  -0.21 1.00     4355     3326
#>  log_lik[4]  -0.45  -0.43 0.15 0.15  -0.72  -0.24 1.00     3915     3271
#>  log_lik[5]  -1.18  -1.16 0.28 0.28  -1.68  -0.75 1.00     4010     3092
#> 
#>  # showing 10 of 105 rows (change via 'max_rows' argument or 'cmdstanr_max_rows' option)
fit$print(max_rows = 2) # same as print(fit, max_rows = 2)
#>  variable   mean median   sd  mad     q5    q95 rhat ess_bulk ess_tail
#>     lp__  -65.97 -65.67 1.39 1.24 -68.70 -64.29 1.00     2291     3113
#>     alpha   0.37   0.37 0.22 0.22   0.01   0.74 1.00     4133     3016
#> 
#>  # showing 2 of 105 rows (change via 'max_rows' argument or 'cmdstanr_max_rows' option)

# include only certain variables
fit$summary("beta")
#> # A tibble: 3 × 10
#>   variable   mean median    sd   mad     q5     q95  rhat ess_bulk ess_tail
#>   <chr>     <dbl>  <dbl> <dbl> <dbl>  <dbl>   <dbl> <dbl>    <dbl>    <dbl>
#> 1 beta[1]  -0.666 -0.664 0.248 0.249 -1.08  -0.257   1.00    4471.    2959.
#> 2 beta[2]  -0.269 -0.267 0.226 0.223 -0.638  0.0973  1.00    4317.    3350.
#> 3 beta[3]   0.675  0.670 0.269 0.279  0.247  1.12    1.00    3731.    3334.
fit$print(c("alpha", "beta[2]"))
#>  variable  mean median   sd  mad    q5  q95 rhat ess_bulk ess_tail
#>   alpha    0.37   0.37 0.22 0.22  0.01 0.74 1.00     4133     3016
#>   beta[2] -0.27  -0.27 0.23 0.22 -0.64 0.10 1.00     4317     3349

# include all variables but only certain summaries
fit$summary(NULL, c("mean", "sd"))
#> # A tibble: 105 × 3
#>    variable      mean     sd
#>    <chr>        <dbl>  <dbl>
#>  1 lp__       -66.0   1.39  
#>  2 alpha        0.374 0.220 
#>  3 beta[1]     -0.666 0.248 
#>  4 beta[2]     -0.269 0.226 
#>  5 beta[3]      0.675 0.269 
#>  6 log_lik[1]  -0.517 0.0992
#>  7 log_lik[2]  -0.402 0.145 
#>  8 log_lik[3]  -0.497 0.219 
#>  9 log_lik[4]  -0.451 0.151 
#> 10 log_lik[5]  -1.18  0.285 
#> # ℹ 95 more rows

# can use functions created from formulas
# for example, calculate Pr(beta > 0)
fit$summary("beta", prob_gt_0 = ~ mean(. > 0))
#> # A tibble: 3 × 2
#>   variable prob_gt_0
#>   <chr>        <dbl>
#> 1 beta[1]    0.00225
#> 2 beta[2]    0.114  
#> 3 beta[3]    0.997  

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
#> 1 alpha     0.374  0.375 0.220 0.221 -0.0414  0.824  1.00    4133.    3017.
#> 2 beta[1]  -0.666 -0.664 0.248 0.249 -1.17   -0.186  1.00    4471.    2959.
#> 3 beta[2]  -0.269 -0.267 0.226 0.223 -0.713   0.166  1.00    4317.    3350.
#> 4 beta[3]   0.675  0.670 0.269 0.279  0.170   1.21   1.00    3731.    3334.

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
