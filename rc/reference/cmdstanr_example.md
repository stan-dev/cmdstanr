# Fit models for use in examples

Fit models for use in examples

## Usage

``` r
cmdstanr_example(
  example = c("logistic", "schools", "schools_ncp"),
  method = c("sample", "optimize", "laplace", "variational", "pathfinder", "diagnose"),
  ...,
  quiet = TRUE,
  force_recompile = NULL
)

print_example_program(example = c("logistic", "schools", "schools_ncp"))
```

## Arguments

- example:

  (string) The name of the example. The currently available examples are

  - `"logistic"`: logistic regression with intercept and 3 predictors.

  - `"schools"`: the so-called "eight schools" model, a hierarchical
    meta-analysis. Fitting this model will result in warnings about
    divergences.

  - `"schools_ncp"`: non-centered parameterization of the "eight
    schools" model that fixes the problem with divergences.

  To print the Stan code for a given `example` use
  `print_example_program(example)`.

- method:

  (string) The fitting method to use. One of `"sample"`, `"optimize"`,
  `"laplace"`, `"variational"`, `"pathfinder"`, or `"diagnose"`. The
  default is `"sample"` (MCMC).

- ...:

  Arguments passed to the chosen `method`. See the help pages for the
  individual methods for details.

- quiet:

  (logical) If `TRUE` (the default) then fitting the model is wrapped in
  [`utils::capture.output()`](https://rdrr.io/r/utils/capture.output.html).

- force_recompile:

  Passed to
  [`cmdstan_model()`](https://mc-stan.org/cmdstanr/rc/reference/cmdstan_model.md).

## Value

`cmdstanr_example()` returns the fitted model object from the selected
`method`. `print_example_program()` invisibly returns `NULL` after
printing the Stan code.

## See also

[`cmdstan_model()`](https://mc-stan.org/cmdstanr/rc/reference/cmdstan_model.md)
for fitting your own Stan programs and
[`print_stan_file()`](https://mc-stan.org/cmdstanr/rc/reference/print_stan_file.md)
for displaying Stan source files

## Examples

``` r
# \dontrun{
print_example_program("logistic")
#> data {
#>   int<lower=0> N;
#>   int<lower=0> K;
#>   array[N] int<lower=0, upper=1> y;
#>   matrix[N, K] X;
#> }
#> parameters {
#>   real alpha;
#>   vector[K] beta;
#> }
#> model {
#>   target += normal_lpdf(alpha | 0, 1);
#>   target += normal_lpdf(beta | 0, 1);
#>   target += bernoulli_logit_glm_lpmf(y | X, alpha, beta);
#> }
#> generated quantities {
#>   vector[N] log_lik;
#>   for (n in 1 : N) {
#>     log_lik[n] = bernoulli_logit_lpmf(y[n] | alpha + X[n] * beta);
#>   }
#> }
fit_logistic_mcmc <- cmdstanr_example("logistic", chains = 2)
fit_logistic_mcmc$summary()
#> # A tibble: 105 × 10
#>    variable      mean  median     sd    mad       q5      q95  rhat ess_bulk
#>    <chr>        <dbl>   <dbl>  <dbl>  <dbl>    <dbl>    <dbl> <dbl>    <dbl>
#>  1 lp__       -66.0   -65.6   1.43   1.25   -68.7    -64.3    1.00     1064.
#>  2 alpha        0.385   0.387 0.218  0.219    0.0320   0.738  1.00     1811.
#>  3 beta[1]     -0.665  -0.661 0.249  0.244   -1.09    -0.265  1.000    2086.
#>  4 beta[2]     -0.281  -0.281 0.216  0.212   -0.645    0.0598 1.000    2124.
#>  5 beta[3]      0.684   0.676 0.274  0.267    0.230    1.13   1.00     1905.
#>  6 log_lik[1]  -0.515  -0.506 0.0981 0.0967  -0.687   -0.365  1.00     1726.
#>  7 log_lik[2]  -0.405  -0.385 0.152  0.145   -0.679   -0.197  1.00     2431.
#>  8 log_lik[3]  -0.499  -0.467 0.213  0.196   -0.905   -0.215  1.00     1801.
#>  9 log_lik[4]  -0.447  -0.428 0.148  0.143   -0.720   -0.241  1.000    1941.
#> 10 log_lik[5]  -1.19   -1.17  0.285  0.288   -1.67    -0.754  1.00     1847.
#> # ℹ 95 more rows
#> # ℹ 1 more variable: ess_tail <dbl>

fit_logistic_optim <- cmdstanr_example("logistic", method = "optimize")
fit_logistic_optim$summary()
#> # A tibble: 105 × 2
#>    variable   estimate
#>    <chr>         <dbl>
#>  1 lp__        -63.9  
#>  2 alpha         0.364
#>  3 beta[1]      -0.632
#>  4 beta[2]      -0.259
#>  5 beta[3]       0.648
#>  6 log_lik[1]   -0.515
#>  7 log_lik[2]   -0.394
#>  8 log_lik[3]   -0.469
#>  9 log_lik[4]   -0.442
#> 10 log_lik[5]   -1.14 
#> # ℹ 95 more rows

fit_logistic_vb <- cmdstanr_example("logistic", method = "variational")
fit_logistic_vb$summary()
#> # A tibble: 106 × 7
#>    variable       mean  median    sd   mad       q5     q95
#>    <chr>         <dbl>   <dbl> <dbl> <dbl>    <dbl>   <dbl>
#>  1 lp__        -66.5   -66.0   1.84  1.62  -70.0    -64.3  
#>  2 lp_approx__  -2.09   -1.73  1.50  1.36   -4.88    -0.337
#>  3 alpha         0.375   0.369 0.266 0.247  -0.0664   0.818
#>  4 beta[1]      -0.643  -0.636 0.245 0.251  -1.03    -0.252
#>  5 beta[2]      -0.274  -0.271 0.255 0.261  -0.679    0.137
#>  6 beta[3]       0.699   0.684 0.285 0.294   0.253    1.18 
#>  7 log_lik[1]   -0.524  -0.516 0.117 0.112  -0.718   -0.348
#>  8 log_lik[2]   -0.404  -0.385 0.169 0.161  -0.709   -0.172
#>  9 log_lik[3]   -0.512  -0.468 0.251 0.226  -0.974   -0.189
#> 10 log_lik[4]   -0.448  -0.426 0.164 0.166  -0.747   -0.221
#> # ℹ 96 more rows

print_example_program("schools")
#> data {
#>   int<lower=1> J;
#>   vector<lower=0>[J] sigma;
#>   vector[J] y;
#> }
#> parameters {
#>   real mu;
#>   real<lower=0> tau;
#>   vector[J] theta;
#> }
#> model {
#>   target += normal_lpdf(tau | 0, 10);
#>   target += normal_lpdf(mu | 0, 10);
#>   target += normal_lpdf(theta | mu, tau);
#>   target += normal_lpdf(y | theta, sigma);
#> }
fit_schools_mcmc <- cmdstanr_example("schools")
#> Warning: 254 of 4000 (6.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 1 of 4 chains had an E-BFMI less than 0.3.
#> See https://mc-stan.org/misc/warnings for details.
fit_schools_mcmc$summary()
#> # A tibble: 11 × 10
#>    variable   mean median    sd   mad      q5   q95  rhat ess_bulk ess_tail
#>    <chr>     <dbl>  <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl>    <dbl>    <dbl>
#>  1 lp__     -57.8  -58.2   5.69  5.93 -66.7   -48.3  1.10     31.4     22.2
#>  2 mu         6.29   6.01  4.16  3.84  -0.481  13.4  1.01    704.    1220. 
#>  3 tau        5.27   4.33  3.79  3.41   0.598  12.6  1.11     30.4     10.2
#>  4 theta[1]   9.15   8.14  6.97  5.57  -0.610  22.4  1.01    610.    1710. 
#>  5 theta[2]   6.71   6.24  5.59  4.97  -2.51   16.0  1.04    868.    2052. 
#>  6 theta[3]   5.21   5.66  6.69  5.51  -6.60   15.1  1.04   1101.    1681. 
#>  7 theta[4]   6.42   6.14  5.84  5.17  -3.29   15.8  1.01   1144.    1967. 
#>  8 theta[5]   4.53   5.18  5.57  4.88  -5.58   13.1  1.04    970.    1641. 
#>  9 theta[6]   5.28   5.39  5.81  5.04  -4.62   14.3  1.05   1195.    1672. 
#> 10 theta[7]   8.95   8.44  6.16  5.59  -0.127  20.1  1.02    390.    1382. 
#> 11 theta[8]   6.90   6.19  6.98  5.76  -3.76   18.7  1.03   1499.    1921. 

print_example_program("schools_ncp")
#> data {
#>   int<lower=1> J;
#>   vector<lower=0>[J] sigma;
#>   vector[J] y;
#> }
#> parameters {
#>   real mu;
#>   real<lower=0> tau;
#>   vector[J] theta_raw;
#> }
#> transformed parameters {
#>   vector[J] theta = mu + tau * theta_raw;
#> }
#> model {
#>   target += normal_lpdf(tau | 0, 10);
#>   target += normal_lpdf(mu | 0, 10);
#>   target += normal_lpdf(theta_raw | 0, 1);
#>   target += normal_lpdf(y | theta, sigma);
#> }
fit_schools_ncp_mcmc <- cmdstanr_example("schools_ncp")
#> Warning: 2 of 4000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
fit_schools_ncp_mcmc$summary()
#> # A tibble: 19 × 10
#>    variable     mean   median    sd   mad      q5    q95  rhat ess_bulk ess_tail
#>    <chr>       <dbl>    <dbl> <dbl> <dbl>   <dbl>  <dbl> <dbl>    <dbl>    <dbl>
#>  1 lp__     -4.68e+1 -46.5    2.38  2.24  -51.2   -43.4  1.00     1485.    2358.
#>  2 mu        6.60e+0   6.64   4.19  4.24   -0.418  13.3  1.000    2770.    2428.
#>  3 tau       4.84e+0   4.04   3.68  3.43    0.406  11.9  1.00     2273.    1720.
#>  4 theta_r…  3.67e-1   0.363  0.930 0.939  -1.18    1.91 1.000    3436.    2803.
#>  5 theta_r…  2.77e-2   0.0112 0.925 0.927  -1.50    1.57 1.00     3875.    2536.
#>  6 theta_r… -1.45e-1  -0.133  0.945 0.934  -1.73    1.40 1.00     4107.    2766.
#>  7 theta_r… -5.35e-3  -0.0210 0.911 0.925  -1.49    1.48 1.00     3812.    2998.
#>  8 theta_r… -2.91e-1  -0.310  0.907 0.876  -1.75    1.25 1.00     3885.    2750.
#>  9 theta_r… -1.41e-1  -0.160  0.940 0.936  -1.66    1.38 1.00     4524.    2919.
#> 10 theta_r…  3.57e-1   0.357  0.927 0.895  -1.17    1.88 1.00     3718.    2911.
#> 11 theta_r…  6.56e-2   0.0606 0.926 0.919  -1.43    1.61 1.00     3925.    3141.
#> 12 theta[1]  9.12e+0   8.50   6.64  5.80   -0.283  20.8  1.00     3351.    2802.
#> 13 theta[2]  6.88e+0   6.73   5.52  5.25   -1.64   16.1  1.00     4130.    2994.
#> 14 theta[3]  5.58e+0   5.97   6.41  5.61   -5.36   15.3  1.00     3338.    2679.
#> 15 theta[4]  6.47e+0   6.58   5.81  5.22   -3.06   15.5  1.000    4241.    3063.
#> 16 theta[5]  4.79e+0   5.23   5.55  5.18   -4.94   13.2  1.00     3616.    3258.
#> 17 theta[6]  5.63e+0   5.91   5.85  5.46   -4.18   14.8  1.000    3948.    3278.
#> 18 theta[7]  8.92e+0   8.29   5.94  5.26    0.252  19.6  1.00     3912.    3315.
#> 19 theta[8]  7.05e+0   7.04   6.39  5.70   -3.13   17.9  1.000    3713.    2837.

# optimization fails for hierarchical model
cmdstanr_example("schools", "optimize", quiet = FALSE)
#> Initial log joint probability = -56.7189 
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>       99        130.27      0.196627     5.699e+09      0.5454      0.5454      176    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>      179       244.065    0.00944698   3.97464e+15       1e-12       0.001      380  LS failed, Hessian reset  
#> Chain 1 Optimization terminated with error: 
#> Chain 1   Line search failed to achieve a sufficient decrease, no more progress can be made
#> Warning: Fitting finished unexpectedly! Use the $output() method for more information.
#> Finished in  0.1 seconds.
#> Error: Optimization failed. There is no output to read.
# }
```
