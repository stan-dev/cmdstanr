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
#>  1 lp__       -65.9   -65.6   1.44   1.25   -68.8    -64.3    1.000    2149.
#>  2 alpha        0.378   0.374 0.213  0.213    0.0368   0.724  0.999    4395.
#>  3 beta[1]     -0.670  -0.664 0.248  0.247   -1.10    -0.272  1.00     4944.
#>  4 beta[2]     -0.275  -0.271 0.227  0.227   -0.652    0.0953 1.000    4390.
#>  5 beta[3]      0.692   0.692 0.267  0.266    0.256    1.14   1.00     4015.
#>  6 log_lik[1]  -0.517  -0.512 0.0986 0.0974  -0.691   -0.368  1.00     4387.
#>  7 log_lik[2]  -0.396  -0.376 0.144  0.137   -0.660   -0.195  1.000    4686.
#>  8 log_lik[3]  -0.496  -0.459 0.220  0.202   -0.902   -0.205  1.00     4398.
#>  9 log_lik[4]  -0.445  -0.425 0.149  0.146   -0.717   -0.232  1.00     3961.
#> 10 log_lik[5]  -1.19   -1.17  0.277  0.279   -1.68    -0.778  1.00     4423.
#> # ℹ 95 more rows
#> # ℹ 1 more variable: ess_tail <dbl>
fit$print()
#>    variable   mean median   sd  mad     q5    q95 rhat ess_bulk ess_tail
#>  lp__       -65.93 -65.60 1.44 1.25 -68.76 -64.28 1.00     2148     2810
#>  alpha        0.38   0.37 0.21 0.21   0.04   0.72 1.00     4395     3478
#>  beta[1]     -0.67  -0.66 0.25 0.25  -1.10  -0.27 1.00     4944     3435
#>  beta[2]     -0.27  -0.27 0.23 0.23  -0.65   0.10 1.00     4389     3420
#>  beta[3]      0.69   0.69 0.27 0.27   0.26   1.14 1.00     4015     3162
#>  log_lik[1]  -0.52  -0.51 0.10 0.10  -0.69  -0.37 1.00     4386     3153
#>  log_lik[2]  -0.40  -0.38 0.14 0.14  -0.66  -0.19 1.00     4686     2960
#>  log_lik[3]  -0.50  -0.46 0.22 0.20  -0.90  -0.21 1.00     4397     3483
#>  log_lik[4]  -0.45  -0.43 0.15 0.15  -0.72  -0.23 1.00     3960     3031
#>  log_lik[5]  -1.19  -1.17 0.28 0.28  -1.68  -0.78 1.00     4423     3501
#> 
#>  # showing 10 of 105 rows (change via 'max_rows' argument or 'cmdstanr_max_rows' option)
fit$print(max_rows = 2) # same as print(fit, max_rows = 2)
#>  variable   mean median   sd  mad     q5    q95 rhat ess_bulk ess_tail
#>     lp__  -65.93 -65.60 1.44 1.25 -68.76 -64.28 1.00     2148     2810
#>     alpha   0.38   0.37 0.21 0.21   0.04   0.72 1.00     4395     3478
#> 
#>  # showing 2 of 105 rows (change via 'max_rows' argument or 'cmdstanr_max_rows' option)

# include only certain variables
fit$summary("beta")
#> # A tibble: 3 × 10
#>   variable   mean median    sd   mad     q5     q95  rhat ess_bulk ess_tail
#>   <chr>     <dbl>  <dbl> <dbl> <dbl>  <dbl>   <dbl> <dbl>    <dbl>    <dbl>
#> 1 beta[1]  -0.670 -0.664 0.248 0.247 -1.10  -0.272  1.00     4944.    3436.
#> 2 beta[2]  -0.275 -0.271 0.227 0.227 -0.652  0.0953 1.000    4390.    3421.
#> 3 beta[3]   0.692  0.692 0.267 0.266  0.256  1.14   1.00     4015.    3163.
fit$print(c("alpha", "beta[2]"))
#>  variable  mean median   sd  mad    q5  q95 rhat ess_bulk ess_tail
#>   alpha    0.38   0.37 0.21 0.21  0.04 0.72 1.00     4395     3478
#>   beta[2] -0.27  -0.27 0.23 0.23 -0.65 0.10 1.00     4389     3420

# include all variables but only certain summaries
fit$summary(NULL, c("mean", "sd"))
#> # A tibble: 105 × 3
#>    variable      mean     sd
#>    <chr>        <dbl>  <dbl>
#>  1 lp__       -65.9   1.44  
#>  2 alpha        0.378 0.213 
#>  3 beta[1]     -0.670 0.248 
#>  4 beta[2]     -0.275 0.227 
#>  5 beta[3]      0.692 0.267 
#>  6 log_lik[1]  -0.517 0.0986
#>  7 log_lik[2]  -0.396 0.144 
#>  8 log_lik[3]  -0.496 0.220 
#>  9 log_lik[4]  -0.445 0.149 
#> 10 log_lik[5]  -1.19  0.277 
#> # ℹ 95 more rows

# can use functions created from formulas
# for example, calculate Pr(beta > 0)
fit$summary("beta", prob_gt_0 = ~ mean(. > 0))
#> # A tibble: 3 × 2
#>   variable prob_gt_0
#>   <chr>        <dbl>
#> 1 beta[1]    0.00225
#> 2 beta[2]    0.109  
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
#> 1 alpha     0.378  0.374 0.213 0.213 -0.0319  0.795 0.999    4395.    3478.
#> 2 beta[1]  -0.670 -0.664 0.248 0.247 -1.18   -0.209 1.00     4944.    3436.
#> 3 beta[2]  -0.275 -0.271 0.227 0.227 -0.717   0.162 1.000    4390.    3421.
#> 4 beta[3]   0.692  0.692 0.267 0.266  0.188   1.23  1.00     4015.    3163.

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
