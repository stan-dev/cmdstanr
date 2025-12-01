# Return Stan code

Return Stan code

## Usage

``` r
code()
```

## Value

A character vector with one element per line of code.

## See also

[`CmdStanMCMC`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMCMC.md),
[`CmdStanMLE`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMLE.md),
[`CmdStanVB`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanVB.md),
[`CmdStanGQ`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanGQ.md)

## Examples

``` r
# \dontrun{
fit <- cmdstanr_example()
fit$code() # character vector
#>  [1] "data {"                                                            
#>  [2] "  int<lower=0> N;"                                                 
#>  [3] "  int<lower=0> K;"                                                 
#>  [4] "  array[N] int<lower=0, upper=1> y;"                               
#>  [5] "  matrix[N, K] X;"                                                 
#>  [6] "}"                                                                 
#>  [7] "parameters {"                                                      
#>  [8] "  real alpha;"                                                     
#>  [9] "  vector[K] beta;"                                                 
#> [10] "}"                                                                 
#> [11] "model {"                                                           
#> [12] "  target += normal_lpdf(alpha | 0, 1);"                            
#> [13] "  target += normal_lpdf(beta | 0, 1);"                             
#> [14] "  target += bernoulli_logit_glm_lpmf(y | X, alpha, beta);"         
#> [15] "}"                                                                 
#> [16] "generated quantities {"                                            
#> [17] "  vector[N] log_lik;"                                              
#> [18] "  for (n in 1 : N) {"                                              
#> [19] "    log_lik[n] = bernoulli_logit_lpmf(y[n] | alpha + X[n] * beta);"
#> [20] "  }"                                                               
#> [21] "}"                                                                 
cat(fit$code(), sep = "\n") # pretty print
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
# }
```
