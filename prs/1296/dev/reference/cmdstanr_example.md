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
#>  1 lp__       -66.0   -65.6   1.46   1.27   -68.8    -64.3     1.00     976.
#>  2 alpha        0.388   0.382 0.221  0.215    0.0317   0.760   1.00    2116.
#>  3 beta[1]     -0.672  -0.669 0.249  0.256   -1.09    -0.265   1.00    2303.
#>  4 beta[2]     -0.276  -0.272 0.225  0.226   -0.658    0.0856  1.01    1797.
#>  5 beta[3]      0.681   0.685 0.263  0.255    0.256    1.13    1.00    1954.
#>  6 log_lik[1]  -0.512  -0.508 0.0994 0.0996  -0.681   -0.362   1.00    2052.
#>  7 log_lik[2]  -0.404  -0.384 0.146  0.141   -0.669   -0.199   1.00    2225.
#>  8 log_lik[3]  -0.495  -0.463 0.221  0.207   -0.926   -0.206   1.00    2142.
#>  9 log_lik[4]  -0.451  -0.432 0.153  0.147   -0.730   -0.234   1.00    1832.
#> 10 log_lik[5]  -1.19   -1.17  0.284  0.274   -1.70    -0.752   1.00    2369.
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
#> Warning: 94 of 4000 (2.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 1 of 4 chains had an E-BFMI less than 0.3.
#> See https://mc-stan.org/misc/warnings for details.
fit_schools_mcmc$summary()
#> # A tibble: 11 × 10
#>    variable   mean median    sd   mad      q5   q95  rhat ess_bulk ess_tail
#>    <chr>     <dbl>  <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl>    <dbl>    <dbl>
#>  1 lp__     -58.2  -58.7   5.22  5.15 -66.1   -48.5  1.03     184.     208.
#>  2 mu         6.71   6.77  4.28  4.41  -0.384  13.5  1.01     464.    1445.
#>  3 tau        5.34   4.65  3.44  3.26   1.11   12.0  1.03     171.     155.
#>  4 theta[1]   9.41   9.08  7.00  6.23  -0.847  21.4  1.01    1090.    1672.
#>  5 theta[2]   7.15   7.22  5.75  5.61  -2.24   16.3  1.00    1019.    2181.
#>  6 theta[3]   5.70   5.99  6.69  6.11  -5.77   15.6  1.01     814.    1784.
#>  7 theta[4]   6.84   6.88  6.00  5.79  -3.10   16.3  1.00    1193.    2380.
#>  8 theta[5]   4.94   5.24  5.89  5.74  -5.16   13.8  1.01     701.    1567.
#>  9 theta[6]   5.71   6.03  6.05  5.83  -4.62   14.7  1.01     735.    2034.
#> 10 theta[7]   9.38   9.16  6.03  5.73   0.340  19.7  1.01     957.    1757.
#> 11 theta[8]   7.19   7.34  6.78  6.29  -3.89   18.0  1.01    1408.    2054.

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
#>    variable         mean   median    sd   mad       q5    q95  rhat ess_bulk
#>    <chr>           <dbl>    <dbl> <dbl> <dbl>    <dbl>  <dbl> <dbl>    <dbl>
#>  1 lp__         -46.8    -46.5    2.41  2.26  -51.3    -43.4  1.00     1375.
#>  2 mu             6.37     6.37   4.21  4.11   -0.492   13.1  1.00     3150.
#>  3 tau            4.87     4.12   3.78  3.57    0.401   12.1  1.00     1884.
#>  4 theta_raw[1]   0.347    0.352  0.941 0.918  -1.24     1.90 1.00     3794.
#>  5 theta_raw[2]   0.0649   0.0634 0.895 0.878  -1.40     1.52 1.00     3897.
#>  6 theta_raw[3]  -0.169   -0.175  0.948 0.943  -1.74     1.40 1.00     4346.
#>  7 theta_raw[4]   0.0519   0.0461 0.932 0.905  -1.50     1.60 1.00     3504.
#>  8 theta_raw[5]  -0.285   -0.289  0.900 0.918  -1.77     1.23 1.000    3531.
#>  9 theta_raw[6]  -0.173   -0.172  0.922 0.891  -1.69     1.36 1.000    3752.
#> 10 theta_raw[7]   0.373    0.394  0.910 0.877  -1.14     1.90 1.00     3600.
#> 11 theta_raw[8]   0.0829   0.0896 0.953 0.938  -1.47     1.62 1.00     4183.
#> 12 theta[1]       8.72     8.02   6.60  5.62   -0.888   20.3  1.00     3351.
#> 13 theta[2]       6.85     6.71   5.47  5.00   -1.93    15.8  1.00     4486.
#> 14 theta[3]       5.28     5.69   6.65  5.56   -6.18    15.2  1.00     3772.
#> 15 theta[4]       6.56     6.49   5.58  5.02   -2.41    15.5  1.00     4240.
#> 16 theta[5]       4.61     4.96   5.60  5.04   -5.22    13.1  1.00     4499.
#> 17 theta[6]       5.25     5.54   5.85  5.32   -4.78    14.1  1.000    4237.
#> 18 theta[7]       8.74     8.38   5.89  5.44    0.0755  19.1  1.00     3228.
#> 19 theta[8]       6.94     6.76   6.46  5.58   -3.36    17.8  1.00     3933.
#> # ℹ 1 more variable: ess_tail <dbl>

# optimization fails for hierarchical model
cmdstanr_example("schools", "optimize", quiet = FALSE)
#> Initial log joint probability = -60.3846 
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>       99       112.045     0.0441947   6.77894e+08      0.5067      0.5067      166    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>      187       244.384     0.0416748   9.41626e+15       1e-12       0.001      390  LS failed, Hessian reset  
#> Chain 1 Optimization terminated with error: 
#> Chain 1   Line search failed to achieve a sufficient decrease, no more progress can be made
#> Warning: Fitting finished unexpectedly! Use the $output() method for more information.
#> Finished in  0.1 seconds.
#> Error: Fitting failed. Unable to print.
# }
```
