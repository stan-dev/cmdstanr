# Create a `draws` object from a CmdStanR fitted model object

Create a `draws` object supported by the posterior package. These
methods are just wrappers around CmdStanR's
[`$draws()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-draws.md)
method provided for convenience.

## Usage

``` r
# S3 method for class 'CmdStanMCMC'
as_draws(x, ...)

# S3 method for class 'CmdStanMLE'
as_draws(x, ...)

# S3 method for class 'CmdStanLaplace'
as_draws(x, ...)

# S3 method for class 'CmdStanVB'
as_draws(x, ...)

# S3 method for class 'CmdStanGQ'
as_draws(x, ...)

# S3 method for class 'CmdStanPathfinder'
as_draws(x, ...)
```

## Arguments

- x:

  A CmdStanR fitted model object.

- ...:

  Optional arguments passed to the
  [`$draws()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-draws.md)
  method (e.g., `variables`, `inc_warmup`, etc.).

## Details

To subset iterations, chains, or draws, use the
[`posterior::subset_draws()`](https://mc-stan.org/posterior/reference/subset_draws.html)
method after creating the `draws` object.

## Examples

``` r
# \dontrun{
fit <- cmdstanr_example()
as_draws(fit)
#> # A draws_array: 1000 iterations, 4 chains, and 105 variables
#> , , variable = lp__
#> 
#>          chain
#> iteration   1   2   3   4
#>         1 -69 -69 -66 -67
#>         2 -65 -64 -70 -67
#>         3 -67 -65 -75 -65
#>         4 -65 -64 -71 -67
#>         5 -64 -66 -69 -67
#> 
#> , , variable = alpha
#> 
#>          chain
#> iteration    1    2    3    4
#>         1 0.96 0.48 0.49 0.23
#>         2 0.47 0.44 0.38 0.50
#>         3 0.33 0.53 0.30 0.20
#>         4 0.25 0.29 0.38 0.56
#>         5 0.50 0.37 0.30 0.69
#> 
#> , , variable = beta[1]
#> 
#>          chain
#> iteration     1     2     3     4
#>         1 -0.60 -0.29 -0.41 -0.72
#>         2 -0.59 -0.66 -0.25 -0.97
#>         3 -1.20 -0.90 -0.41 -0.43
#>         4 -0.61 -0.65 -0.39 -1.07
#>         5 -0.70 -1.08 -0.43 -0.93
#> 
#> , , variable = beta[2]
#> 
#>          chain
#> iteration      1     2      3     4
#>         1  0.078 -0.82  0.085 -0.68
#>         2 -0.530 -0.39 -0.128  0.23
#>         3 -0.086 -0.35  0.397 -0.46
#>         4 -0.489 -0.18  0.296 -0.40
#>         5 -0.213 -0.46  0.193 -0.49
#> 
#> # ... with 995 more iterations, and 101 more variables

# posterior's as_draws_*() methods will also work
posterior::as_draws_rvars(fit)
#> # A draws_rvars: 1000 iterations, 4 chains, and 4 variables
#> $lp__: rvar<1000,4>[1] mean ± sd:
#> [1] -66 ± 1.5 
#> 
#> $alpha: rvar<1000,4>[1] mean ± sd:
#> [1] 0.38 ± 0.22 
#> 
#> $beta: rvar<1000,4>[3] mean ± sd:
#> [1] -0.66 ± 0.25  -0.27 ± 0.23   0.68 ± 0.27 
#> 
#> $log_lik: rvar<1000,4>[100] mean ± sd:
#>   [1] -0.515 ± 0.100  -0.406 ± 0.151  -0.498 ± 0.220  -0.453 ± 0.154 
#>   [5] -1.182 ± 0.279  -0.588 ± 0.187  -0.643 ± 0.124  -0.279 ± 0.137 
#>   [9] -0.690 ± 0.165  -0.737 ± 0.236  -0.280 ± 0.125  -0.502 ± 0.245 
#>  [13] -0.659 ± 0.207  -0.361 ± 0.173  -0.279 ± 0.106  -0.276 ± 0.088 
#>  [17] -1.593 ± 0.294  -0.478 ± 0.109  -0.233 ± 0.077  -0.114 ± 0.079 
#>  [21] -0.212 ± 0.088  -0.566 ± 0.147  -0.330 ± 0.142  -0.137 ± 0.068 
#>  [25] -0.458 ± 0.122  -1.521 ± 0.349  -0.307 ± 0.121  -0.444 ± 0.084 
#>  [29] -0.726 ± 0.227  -0.691 ± 0.192  -0.486 ± 0.160  -0.423 ± 0.106 
#>  [33] -0.413 ± 0.128  -0.065 ± 0.054  -0.588 ± 0.183  -0.325 ± 0.135 
#>  [37] -0.696 ± 0.227  -0.316 ± 0.151  -0.180 ± 0.109  -0.677 ± 0.128 
#>  [41] -1.133 ± 0.256  -0.925 ± 0.197  -0.410 ± 0.265  -1.177 ± 0.188 
#>  [45] -0.359 ± 0.118  -0.585 ± 0.131  -0.308 ± 0.131  -0.324 ± 0.084 
#>  [49] -0.319 ± 0.080  -1.292 ± 0.338  -0.288 ± 0.096  -0.838 ± 0.146 
#>  [53] -0.401 ± 0.129  -0.371 ± 0.142  -0.389 ± 0.138  -0.321 ± 0.196 
#>  [57] -0.654 ± 0.118  -0.945 ± 0.352  -1.351 ± 0.341  -0.981 ± 0.162 
#>  [61] -0.539 ± 0.100  -0.880 ± 0.320  -0.117 ± 0.076  -0.906 ± 0.252 
#>  [65] -2.012 ± 0.598  -0.506 ± 0.133  -0.277 ± 0.083  -1.064 ± 0.239 
#>  [69] -0.435 ± 0.083  -0.636 ± 0.237  -0.613 ± 0.207  -0.466 ± 0.171 
#>  [73] -1.475 ± 0.368  -0.950 ± 0.195  -1.145 ± 0.396  -0.373 ± 0.139 
#>  [77] -0.880 ± 0.138  -0.487 ± 0.174  -0.760 ± 0.192  -0.546 ± 0.200 
#>  [81] -0.166 ± 0.102  -0.227 ± 0.144  -0.343 ± 0.081  -0.275 ± 0.091 
#>  [85] -0.131 ± 0.075  -1.120 ± 0.318  -0.827 ± 0.129  -0.779 ± 0.240 
#>  [89] -1.270 ± 0.318  -0.264 ± 0.137  -0.390 ± 0.130  -1.496 ± 0.341 
#>  [93] -0.742 ± 0.221  -0.318 ± 0.087  -0.388 ± 0.112  -1.572 ± 0.286 
#>  [97] -0.430 ± 0.103  -1.047 ± 0.370  -0.697 ± 0.142  -0.390 ± 0.098 
#> 
posterior::as_draws_list(fit)
#> # A draws_list: 1000 iterations, 4 chains, and 105 variables
#> 
#> [chain = 1]
#> $lp__
#>  [1] -69 -65 -67 -65 -64 -64 -64 -66 -67 -66
#> 
#> $alpha
#>  [1] 0.96 0.47 0.33 0.25 0.50 0.35 0.40 0.12 0.72 0.71
#> 
#> $`beta[1]`
#>  [1] -0.60 -0.59 -1.20 -0.61 -0.70 -0.57 -0.70 -0.97 -0.32 -0.90
#> 
#> $`beta[2]`
#>  [1]  0.078 -0.530 -0.086 -0.489 -0.213 -0.225 -0.249 -0.272 -0.172 -0.308
#> 
#> 
#> [chain = 2]
#> $lp__
#>  [1] -69 -64 -65 -64 -66 -66 -69 -66 -66 -67
#> 
#> $alpha
#>  [1]  0.4805  0.4444  0.5275  0.2888  0.3654  0.0852  0.9077 -0.0033  0.7021
#> [10]  0.3836
#> 
#> $`beta[1]`
#>  [1] -0.29 -0.66 -0.90 -0.65 -1.08 -0.83 -0.41 -0.62 -0.71 -0.63
#> 
#> $`beta[2]`
#>  [1] -0.824 -0.389 -0.352 -0.182 -0.461 -0.035 -0.654 -0.087 -0.477  0.291
#> 
#> # ... with 990 more iterations, and 2 more chains, and 101 more variables
# }
```
