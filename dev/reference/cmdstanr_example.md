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

  Passed to the
  [\$compile()](https://mc-stan.org/cmdstanr/dev/reference/model-method-compile.md)
  method.

## Value

`cmdstanr_example()` returns the fitted model object from the selected
`method`. `print_example_program()` invisibly returns `NULL` after
printing the Stan code.

## See also

[`cmdstan_model()`](https://mc-stan.org/cmdstanr/dev/reference/cmdstan_model.md)
for fitting your own Stan programs and
[`print_stan_file()`](https://mc-stan.org/cmdstanr/dev/reference/print_stan_file.md)
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
#>  1 lp__       -65.9   -65.6   1.44   1.25   -68.7    -64.3    1.00     1027.
#>  2 alpha        0.381   0.377 0.215  0.205    0.0326   0.737  1.000    1941.
#>  3 beta[1]     -0.667  -0.665 0.250  0.250   -1.08    -0.254  1.00     1988.
#>  4 beta[2]     -0.276  -0.273 0.224  0.219   -0.650    0.0864 1.00     2027.
#>  5 beta[3]      0.680   0.676 0.264  0.259    0.261    1.13   1.00     1918.
#>  6 log_lik[1]  -0.515  -0.508 0.0984 0.0979  -0.686   -0.367  1.00     1971.
#>  7 log_lik[2]  -0.404  -0.384 0.147  0.149   -0.673   -0.199  0.999    2029.
#>  8 log_lik[3]  -0.498  -0.465 0.217  0.193   -0.940   -0.214  1.00     2177.
#>  9 log_lik[4]  -0.449  -0.430 0.151  0.147   -0.706   -0.237  1.00     1849.
#> 10 log_lik[5]  -1.18   -1.16  0.275  0.263   -1.66    -0.765  1.00     2179.
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
#>    variable       mean  median    sd   mad       q5     q95
#>    <chr>         <dbl>   <dbl> <dbl> <dbl>    <dbl>   <dbl>
#>  1 lp__        -66.4   -66.1   1.77  1.59  -69.7    -64.3  
#>  2 lp_approx__  -2.08   -1.78  1.44  1.29   -4.75    -0.363
#>  3 alpha         0.427   0.427 0.241 0.249   0.0180   0.811
#>  4 beta[1]      -0.678  -0.680 0.213 0.223  -1.02    -0.330
#>  5 beta[2]      -0.324  -0.326 0.295 0.300  -0.804    0.170
#>  6 beta[3]       0.670   0.658 0.276 0.268   0.214    1.12 
#>  7 log_lik[1]   -0.501  -0.492 0.107 0.105  -0.696   -0.343
#>  8 log_lik[2]   -0.436  -0.408 0.175 0.156  -0.760   -0.201
#>  9 log_lik[3]   -0.539  -0.489 0.288 0.272  -1.08    -0.173
#> 10 log_lik[4]   -0.453  -0.418 0.173 0.167  -0.777   -0.222
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
#> Warning: 188 of 4000 (5.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 1 of 4 chains had an E-BFMI less than 0.3.
#> See https://mc-stan.org/misc/warnings for details.
fit_schools_mcmc$summary()
#> # A tibble: 11 × 10
#>    variable   mean median    sd   mad      q5   q95  rhat ess_bulk ess_tail
#>    <chr>     <dbl>  <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl>    <dbl>    <dbl>
#>  1 lp__     -58.4  -58.9   5.34  5.36 -66.8   -48.7  1.08     38.3     52.7
#>  2 mu         6.44   6.16  4.15  3.90  -0.287  13.5  1.03    647.    1074. 
#>  3 tau        5.54   4.76  3.69  3.48   1.19   12.8  1.08     39.2     23.2
#>  4 theta[1]   9.44   8.43  7.18  5.99  -0.247  22.6  1.01    734.    1480. 
#>  5 theta[2]   6.82   6.43  5.84  5.25  -2.56   16.6  1.05   1059.    2057. 
#>  6 theta[3]   5.12   5.50  6.86  5.62  -6.63   15.4  1.05   1077.    1511. 
#>  7 theta[4]   6.65   6.13  5.97  5.27  -3.11   16.5  1.03   1015.    1729. 
#>  8 theta[5]   4.39   4.66  5.74  5.27  -5.53   13.5  1.05   1003.    1322. 
#>  9 theta[6]   5.32   5.45  6.02  4.96  -4.87   14.9  1.04   1108.    1717. 
#> 10 theta[7]   9.30   8.65  6.22  5.78   0.263  20.4  1.01    620.    2158. 
#> 11 theta[8]   7.03   6.70  6.88  5.88  -3.92   18.5  1.03    910.    1752. 

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
fit_schools_ncp_mcmc$summary()
#> # A tibble: 19 × 10
#>    variable           mean   median    sd   mad       q5    q95  rhat ess_bulk
#>    <chr>             <dbl>    <dbl> <dbl> <dbl>    <dbl>  <dbl> <dbl>    <dbl>
#>  1 lp__         -46.9      -46.5    2.48  2.38  -51.4    -43.4  1.00     1600.
#>  2 mu             6.45       6.43   4.28  4.09   -0.562   13.4  1.00     3371.
#>  3 tau            4.81       4.00   3.78  3.58    0.339   12.1  1.00     2045.
#>  4 theta_raw[1]   0.351      0.372  0.964 0.967  -1.24     1.90 1.000    4423.
#>  5 theta_raw[2]   0.0374     0.0410 0.894 0.891  -1.44     1.53 1.00     4862.
#>  6 theta_raw[3]  -0.128     -0.130  0.949 0.952  -1.67     1.44 1.00     4809.
#>  7 theta_raw[4]  -0.000343  -0.0195 0.911 0.891  -1.50     1.49 1.000    4356.
#>  8 theta_raw[5]  -0.273     -0.289  0.903 0.886  -1.72     1.24 1.00     4820.
#>  9 theta_raw[6]  -0.161     -0.169  0.932 0.888  -1.70     1.42 1.00     4558.
#> 10 theta_raw[7]   0.347      0.368  0.931 0.943  -1.19     1.82 1.00     3970.
#> 11 theta_raw[8]   0.0836     0.0888 0.932 0.919  -1.44     1.61 1.00     4442.
#> 12 theta[1]       9.01       8.22   6.62  5.66   -0.619   21.2  1.00     3874.
#> 13 theta[2]       6.76       6.71   5.76  5.36   -2.54    16.0  1.00     5346.
#> 14 theta[3]       5.53       5.70   6.65  5.53   -5.48    15.7  1.00     3984.
#> 15 theta[4]       6.49       6.54   5.67  5.23   -2.90    15.5  1.00     4873.
#> 16 theta[5]       4.85       5.20   5.62  5.24   -4.85    13.5  1.00     4569.
#> 17 theta[6]       5.42       5.64   5.95  5.28   -4.78    14.7  1.00     4271.
#> 18 theta[7]       8.78       8.30   5.83  5.48    0.0372  19.1  1.00     4455.
#> 19 theta[8]       7.09       6.87   6.48  5.49   -2.80    18.1  1.00     4149.
#> # ℹ 1 more variable: ess_tail <dbl>

# optimization fails for hierarchical model
cmdstanr_example("schools", "optimize", quiet = FALSE)
#> Initial log joint probability = -60.3846 
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>       99       110.783      0.036349   1.18348e+09       0.253       0.253      173    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>      194       244.211     0.0871827       8.00327       1e-12       0.001      414  LS failed, Hessian reset  
#> Chain 1 Optimization terminated with error: 
#> Chain 1   Line search failed to achieve a sufficient decrease, no more progress can be made
#> Warning: Fitting finished unexpectedly! Use the $output() method for more information.
#> Finished in  0.1 seconds.
#> Error: Fitting failed. Unable to print.
# }
```
