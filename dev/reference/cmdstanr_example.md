# Fit models for use in examples

Fit models for use in examples

## Usage

``` r
cmdstanr_example(
  example = c("logistic", "schools", "schools_ncp"),
  method = c("sample", "optimize", "laplace", "variational", "pathfinder", "diagnose"),
  ...,
  quiet = TRUE,
  force_recompile = getOption("cmdstanr_force_recompile", default = FALSE)
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

  (string) Which fitting method should be used? The default is the
  `"sample"` method (MCMC).

- ...:

  Arguments passed to the chosen `method`. See the help pages for the
  individual methods for details.

- quiet:

  (logical) If `TRUE` (the default) then fitting the model is wrapped in
  [`utils::capture.output()`](https://rdrr.io/r/utils/capture.output.html).

- force_recompile:

  Passed to the
  [\$compile()](https://mc-stan.org/cmdstanr/dev/reference/model-method-compile.md)
  method.

## Value

The fitted model object returned by the selected `method`.

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
#>    variable      mean  median    sd    mad         q5      q95  rhat ess_bulk
#>    <chr>        <dbl>   <dbl> <dbl>  <dbl>      <dbl>    <dbl> <dbl>    <dbl>
#>  1 lp__       -66.0   -65.7   1.45  1.28   -68.8      -64.3    0.999    1047.
#>  2 alpha        0.374   0.374 0.223 0.216   -0.000371   0.737  1.000    1996.
#>  3 beta[1]     -0.679  -0.670 0.261 0.259   -1.12      -0.266  1.00     2471.
#>  4 beta[2]     -0.280  -0.279 0.219 0.217   -0.657      0.0677 1.000    2142.
#>  5 beta[3]      0.689   0.686 0.265 0.274    0.257      1.11   1.00     1976.
#>  6 log_lik[1]  -0.518  -0.512 0.101 0.0951  -0.699     -0.360  1.000    2111.
#>  7 log_lik[2]  -0.395  -0.377 0.141 0.131   -0.646     -0.192  1.000    2157.
#>  8 log_lik[3]  -0.501  -0.471 0.219 0.202   -0.910     -0.206  1.00     2228.
#>  9 log_lik[4]  -0.442  -0.427 0.150 0.150   -0.707     -0.231  1.000    1983.
#> 10 log_lik[5]  -1.18   -1.16  0.286 0.283   -1.68      -0.750  1.00     2106.
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
#>  5 beta[3]       0.649
#>  6 log_lik[1]   -0.515
#>  7 log_lik[2]   -0.394
#>  8 log_lik[3]   -0.469
#>  9 log_lik[4]   -0.442
#> 10 log_lik[5]   -1.14 
#> # ℹ 95 more rows

fit_logistic_vb <- cmdstanr_example("logistic", method = "variational")
fit_logistic_vb$summary()
#> # A tibble: 106 × 7
#>    variable       mean  median    sd    mad       q5      q95
#>    <chr>         <dbl>   <dbl> <dbl>  <dbl>    <dbl>    <dbl>
#>  1 lp__        -66.1   -65.7   1.50  1.28   -68.9    -64.3   
#>  2 lp_approx__  -1.98   -1.68  1.35  1.19    -4.59    -0.381 
#>  3 alpha         0.272   0.281 0.205 0.208   -0.0709   0.609 
#>  4 beta[1]      -0.649  -0.647 0.258 0.251   -1.08    -0.224 
#>  5 beta[2]      -0.336  -0.338 0.226 0.228   -0.711    0.0373
#>  6 beta[3]       0.666   0.668 0.237 0.239    0.277    1.05  
#>  7 log_lik[1]   -0.567  -0.558 0.101 0.0982  -0.742   -0.416 
#>  8 log_lik[2]   -0.388  -0.364 0.138 0.132   -0.646   -0.198 
#>  9 log_lik[3]   -0.602  -0.567 0.249 0.233   -1.07    -0.263 
#> 10 log_lik[4]   -0.391  -0.374 0.124 0.121   -0.626   -0.219 
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
#> Warning: 100 of 4000 (2.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 2 of 4 chains had an E-BFMI less than 0.3.
#> See https://mc-stan.org/misc/warnings for details.
fit_schools_mcmc$summary()
#> # A tibble: 11 × 10
#>    variable   mean median    sd   mad       q5   q95  rhat ess_bulk ess_tail
#>    <chr>     <dbl>  <dbl> <dbl> <dbl>    <dbl> <dbl> <dbl>    <dbl>    <dbl>
#>  1 lp__     -57.5  -57.9   5.57  5.90 -66.2    -47.9  1.01     202.     87.7
#>  2 mu         6.76   6.73  4.09  4.04   0.0809  13.4  1.01     606.    673. 
#>  3 tau        5.07   4.17  3.60  3.21   1.03    12.2  1.01     203.     73.1
#>  4 theta[1]   9.37   8.58  6.86  5.88  -0.166   21.5  1.01     837.   1605. 
#>  5 theta[2]   7.19   7.19  5.50  5.08  -1.72    16.3  1.01    1122.   2330. 
#>  6 theta[3]   5.66   6.01  6.49  5.61  -5.61    15.5  1.00     931.   1788. 
#>  7 theta[4]   6.81   6.97  5.66  5.30  -2.93    15.8  1.01     956.   1997. 
#>  8 theta[5]   4.97   5.51  5.52  5.15  -4.65    13.1  1.01     818.    809. 
#>  9 theta[6]   5.83   6.13  5.86  5.33  -4.43    14.6  1.01    1088.   2229. 
#> 10 theta[7]   9.21   8.87  5.76  5.39   0.754   19.1  1.00     755.   1603. 
#> 11 theta[8]   7.34   7.21  6.58  5.64  -3.38    17.9  1.01    1151.   1785. 

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
#> Warning: 1 of 4000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
fit_schools_ncp_mcmc$summary()
#> # A tibble: 19 × 10
#>    variable     mean   median    sd   mad      q5    q95  rhat ess_bulk ess_tail
#>    <chr>       <dbl>    <dbl> <dbl> <dbl>   <dbl>  <dbl> <dbl>    <dbl>    <dbl>
#>  1 lp__     -46.8    -46.6    2.42  2.39  -51.2   -43.4  1.00     1597.    2363.
#>  2 mu         6.51     6.57   4.16  4.02   -0.472  13.4  1.00     3174.    2417.
#>  3 tau        4.78     4.01   3.65  3.46    0.414  11.9  1.00     2041.    2013.
#>  4 theta_r…   0.376    0.383  0.968 0.993  -1.25    1.94 1.00     4078.    2970.
#>  5 theta_r…   0.0539   0.0478 0.889 0.863  -1.43    1.50 1.000    3497.    2765.
#>  6 theta_r…  -0.162   -0.169  0.948 0.939  -1.71    1.44 1.00     4253.    3063.
#>  7 theta_r…   0.0210   0.0283 0.924 0.919  -1.54    1.55 1.00     4003.    2595.
#>  8 theta_r…  -0.279   -0.279  0.927 0.909  -1.80    1.29 1.00     4657.    2974.
#>  9 theta_r…  -0.156   -0.173  0.923 0.923  -1.67    1.38 1.00     4131.    2764.
#> 10 theta_r…   0.356    0.361  0.895 0.861  -1.12    1.79 1.00     3913.    2847.
#> 11 theta_r…   0.0728   0.0711 0.962 0.938  -1.52    1.68 1.00     4201.    2578.
#> 12 theta[1]   9.00     8.38   6.61  5.70   -0.496  21.2  1.00     3941.    2930.
#> 13 theta[2]   6.93     6.90   5.52  5.05   -1.93   16.1  1.00     4037.    3058.
#> 14 theta[3]   5.42     5.83   6.41  5.64   -5.91   15.0  1.00     3598.    2668.
#> 15 theta[4]   6.76     6.70   5.56  5.12   -2.29   15.8  1.00     4455.    3296.
#> 16 theta[5]   4.83     5.19   5.68  5.38   -5.17   13.4  1.00     4228.    3006.
#> 17 theta[6]   5.48     5.82   5.98  5.28   -4.72   14.6  1.00     4230.    3151.
#> 18 theta[7]   8.74     8.39   5.64  5.27    0.391  18.7  1.00     3385.    3292.
#> 19 theta[8]   7.01     6.90   6.63  5.59   -3.68   17.6  1.00     3930.    3203.

# optimization fails for hierarchical model
cmdstanr_example("schools", "optimize", quiet = FALSE)
#> Initial log joint probability = -57.1252 
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>       99       122.724      0.164532   4.55678e+09      0.4431      0.4431      178    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>      183       248.408     0.0553432   4.75335e+16       1e-12       0.001      393  LS failed, Hessian reset  
#> Chain 1 Optimization terminated with error: 
#> Chain 1   Line search failed to achieve a sufficient decrease, no more progress can be made
#> Warning: Fitting finished unexpectedly! Use the $output() method for more information.
#> Finished in  0.1 seconds.
#> Error: Fitting failed. Unable to print.
# }
```
