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
#>    variable      mean  median     sd    mad       q5      q95  rhat ess_bulk
#>    <chr>        <dbl>   <dbl>  <dbl>  <dbl>    <dbl>    <dbl> <dbl>    <dbl>
#>  1 lp__       -65.9   -65.6   1.39   1.19   -68.6    -64.3    1.00     1104.
#>  2 alpha        0.375   0.378 0.213  0.217    0.0272   0.724  1.000    2088.
#>  3 beta[1]     -0.671  -0.667 0.247  0.247   -1.09    -0.273  1.00     2333.
#>  4 beta[2]     -0.262  -0.260 0.218  0.221   -0.611    0.0903 1.00     2227.
#>  5 beta[3]      0.675   0.676 0.262  0.258    0.246    1.11   1.000    2433.
#>  6 log_lik[1]  -0.515  -0.512 0.0952 0.0960  -0.680   -0.370  1.000    2125.
#>  7 log_lik[2]  -0.401  -0.381 0.146  0.145   -0.659   -0.201  0.999    2375.
#>  8 log_lik[3]  -0.486  -0.455 0.206  0.202   -0.855   -0.206  1.00     2135.
#>  9 log_lik[4]  -0.453  -0.439 0.149  0.146   -0.720   -0.244  1.000    2264.
#> 10 log_lik[5]  -1.18   -1.16  0.274  0.259   -1.64    -0.763  1.00     2634.
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
#>    variable       mean  median     sd    mad      q5      q95
#>    <chr>         <dbl>   <dbl>  <dbl>  <dbl>   <dbl>    <dbl>
#>  1 lp__        -66.5   -66.1   1.67   1.60   -69.7   -64.5   
#>  2 lp_approx__  -2.05   -1.69  1.46   1.27    -4.88   -0.354 
#>  3 alpha         0.444   0.440 0.214  0.214    0.104   0.796 
#>  4 beta[1]      -0.807  -0.799 0.263  0.279   -1.24   -0.386 
#>  5 beta[2]      -0.380  -0.386 0.266  0.248   -0.818   0.0822
#>  6 beta[3]       0.770   0.775 0.240  0.236    0.374   1.17  
#>  7 log_lik[1]   -0.489  -0.485 0.0939 0.0931  -0.647  -0.342 
#>  8 log_lik[2]   -0.378  -0.358 0.140  0.126   -0.645  -0.194 
#>  9 log_lik[3]   -0.520  -0.478 0.257  0.233   -1.02   -0.187 
#> 10 log_lik[4]   -0.396  -0.378 0.143  0.133   -0.665  -0.201 
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
#> Warning: 408 of 4000 (10.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 1 of 4 chains had an E-BFMI less than 0.3.
#> See https://mc-stan.org/misc/warnings for details.
fit_schools_mcmc$summary()
#> # A tibble: 11 × 10
#>    variable   mean median    sd   mad       q5   q95  rhat ess_bulk ess_tail
#>    <chr>     <dbl>  <dbl> <dbl> <dbl>    <dbl> <dbl> <dbl>    <dbl>    <dbl>
#>  1 lp__     -56.7  -57.8   6.73  6.53 -66.4    -44.9  1.29     11.6    NA   
#>  2 mu         6.55   7.27  3.96  3.22  -0.404   12.9  1.15    624.   1119.  
#>  3 tau        4.92   4.09  3.69  3.61   0.788   12.1  1.29     11.4     4.55
#>  4 theta[1]   9.26   8.11  6.71  4.64  -0.805   21.5  1.27    917.   1386.  
#>  5 theta[2]   6.71   7.01  5.44  4.24  -2.31    15.8  1.26   1156.   1501.  
#>  6 theta[3]   5.63   6.86  6.54  4.78  -5.99    15.4  1.08    776.   1299.  
#>  7 theta[4]   6.77   7.72  5.66  4.45  -2.83    15.9  1.15   1084.   1625.  
#>  8 theta[5]   5.01   5.99  5.52  4.21  -5.07    12.7  1.04     84.8  1146.  
#>  9 theta[6]   5.53   6.64  5.57  4.28  -4.37    14.2  1.26   1050.   1547.  
#> 10 theta[7]   8.87   7.92  5.75  4.34  -0.0125  19.4  1.27    957.   1126.  
#> 11 theta[8]   6.95   7.68  6.30  4.83  -3.59    17.5  1.22   1533.   1955.  

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
#>    variable         mean   median    sd   mad       q5    q95  rhat ess_bulk
#>    <chr>           <dbl>    <dbl> <dbl> <dbl>    <dbl>  <dbl> <dbl>    <dbl>
#>  1 lp__         -46.9    -46.5    2.52  2.37  -51.5    -43.5  1.00     1155.
#>  2 mu             6.48     6.55   4.11  4.09   -0.361   13.2  1.00     2991.
#>  3 tau            4.68     3.88   3.67  3.38    0.360   11.8  1.00     1923.
#>  4 theta_raw[1]   0.353    0.378  0.977 0.977  -1.30     1.96 1.00     4209.
#>  5 theta_raw[2]   0.0397   0.0464 0.917 0.913  -1.47     1.57 1.00     3814.
#>  6 theta_raw[3]  -0.104   -0.121  0.953 0.919  -1.65     1.48 1.00     4107.
#>  7 theta_raw[4]   0.0215   0.0116 0.931 0.900  -1.51     1.59 1.00     3944.
#>  8 theta_raw[5]  -0.272   -0.298  0.900 0.861  -1.76     1.20 1.00     3600.
#>  9 theta_raw[6]  -0.178   -0.198  0.915 0.891  -1.64     1.36 1.00     4276.
#> 10 theta_raw[7]   0.350    0.364  0.945 0.904  -1.26     1.88 1.000    3551.
#> 11 theta_raw[8]   0.0710   0.0749 0.958 0.947  -1.50     1.67 1.00     4419.
#> 12 theta[1]       8.79     8.11   6.57  5.59   -0.644   20.6  1.00     3730.
#> 13 theta[2]       6.73     6.72   5.64  5.16   -2.45    15.8  1.00     3521.
#> 14 theta[3]       5.63     5.92   6.31  5.38   -5.45    15.2  1.000    3715.
#> 15 theta[4]       6.69     6.67   5.64  5.13   -2.46    16.1  1.00     4109.
#> 16 theta[5]       4.86     5.25   5.63  5.10   -4.99    13.4  1.00     3655.
#> 17 theta[6]       5.40     5.65   5.69  5.14   -4.39    14.2  1.00     3756.
#> 18 theta[7]       8.72     8.27   5.93  5.36    0.0462  19.2  0.999    4037.
#> 19 theta[8]       7.05     6.83   6.30  5.56   -2.83    17.5  1.00     3781.
#> # ℹ 1 more variable: ess_tail <dbl>

# optimization fails for hierarchical model
cmdstanr_example("schools", "optimize", quiet = FALSE)
#> Initial log joint probability = -56.3435 
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>       99       121.413    0.00167739   5.70953e+09      0.1923      0.1923      167    
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
#>      190       245.609     0.0590247        8.0032       1e-12       0.001      420  LS failed, Hessian reset  
#> Chain 1 Optimization terminated with error: 
#> Chain 1   Line search failed to achieve a sufficient decrease, no more progress can be made
#> Warning: Fitting finished unexpectedly! Use the $output() method for more information.
#> Finished in  0.1 seconds.
#> Error: Fitting failed. Unable to print.
# }
```
