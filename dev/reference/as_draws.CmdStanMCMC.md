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
#>         1 -66 -67 -72 -66
#>         2 -66 -64 -67 -67
#>         3 -68 -66 -68 -67
#>         4 -67 -70 -64 -68
#>         5 -71 -65 -67 -68
#> 
#> , , variable = alpha
#> 
#>          chain
#> iteration    1     2     3    4
#>         1 0.27  0.79 0.074 0.25
#>         2 0.16  0.38 0.083 0.15
#>         3 0.93  0.11 0.538 0.31
#>         4 0.34 -0.24 0.323 0.28
#>         5 0.62  0.59 0.212 0.41
#> 
#> , , variable = beta[1]
#> 
#>          chain
#> iteration     1     2     3     4
#>         1 -0.86 -0.38 -0.65 -0.96
#>         2 -0.67 -0.60 -0.33 -0.33
#>         3 -0.72 -0.49 -0.49 -0.50
#>         4 -0.93 -0.27 -0.50 -0.98
#>         5 -0.50 -0.72 -0.18 -0.78
#> 
#> , , variable = beta[2]
#> 
#>          chain
#> iteration      1      2      3      4
#>         1  0.044 -0.061  0.348 -0.447
#>         2 -0.523 -0.331  0.074 -0.094
#>         3 -0.218 -0.253  0.220  0.169
#>         4 -0.327 -0.124 -0.084 -0.259
#>         5 -0.567 -0.360 -0.191 -0.308
#> 
#> # ... with 995 more iterations, and 101 more variables

# posterior's as_draws_*() methods will also work
posterior::as_draws_rvars(fit)
#> # A draws_rvars: 1000 iterations, 4 chains, and 4 variables
#> $lp__: rvar<1000,4>[1] mean ± sd:
#> [1] -66 ± 1.5 
#> 
#> $alpha: rvar<1000,4>[1] mean ± sd:
#> [1] 0.37 ± 0.22 
#> 
#> $beta: rvar<1000,4>[3] mean ± sd:
#> [1] -0.67 ± 0.25  -0.27 ± 0.23   0.68 ± 0.27 
#> 
#> $log_lik: rvar<1000,4>[100] mean ± sd:
#>   [1] -0.519 ± 0.099  -0.400 ± 0.148  -0.500 ± 0.219  -0.447 ± 0.151 
#>   [5] -1.177 ± 0.282  -0.595 ± 0.191  -0.637 ± 0.125  -0.280 ± 0.135 
#>   [9] -0.697 ± 0.170  -0.742 ± 0.233  -0.282 ± 0.124  -0.494 ± 0.238 
#>  [13] -0.652 ± 0.208  -0.362 ± 0.171  -0.281 ± 0.107  -0.277 ± 0.087 
#>  [17] -1.587 ± 0.288  -0.482 ± 0.109  -0.234 ± 0.076  -0.113 ± 0.078 
#>  [21] -0.213 ± 0.087  -0.572 ± 0.150  -0.333 ± 0.141  -0.137 ± 0.066 
#>  [25] -0.452 ± 0.122  -1.516 ± 0.344  -0.308 ± 0.121  -0.448 ± 0.084 
#>  [29] -0.721 ± 0.229  -0.699 ± 0.195  -0.491 ± 0.162  -0.427 ± 0.108 
#>  [33] -0.407 ± 0.127  -0.064 ± 0.050  -0.583 ± 0.185  -0.327 ± 0.133 
#>  [37] -0.702 ± 0.228  -0.310 ± 0.148  -0.180 ± 0.108  -0.684 ± 0.131 
#>  [41] -1.125 ± 0.252  -0.936 ± 0.202  -0.412 ± 0.262  -1.171 ± 0.186 
#>  [45] -0.360 ± 0.117  -0.579 ± 0.129  -0.303 ± 0.127  -0.326 ± 0.083 
#>  [49] -0.321 ± 0.080  -1.286 ± 0.333  -0.290 ± 0.094  -0.831 ± 0.143 
#>  [53] -0.404 ± 0.130  -0.372 ± 0.141  -0.383 ± 0.135  -0.322 ± 0.191 
#>  [57] -0.660 ± 0.119  -0.955 ± 0.359  -1.366 ± 0.346  -0.975 ± 0.160 
#>  [61] -0.543 ± 0.099  -0.873 ± 0.311  -0.117 ± 0.072  -0.897 ± 0.249 
#>  [65] -2.006 ± 0.587  -0.511 ± 0.137  -0.278 ± 0.082  -1.056 ± 0.235 
#>  [69] -0.438 ± 0.084  -0.643 ± 0.238  -0.606 ± 0.209  -0.460 ± 0.171 
#>  [73] -1.490 ± 0.367  -0.945 ± 0.197  -1.139 ± 0.388  -0.374 ± 0.138 
#>  [77] -0.873 ± 0.139  -0.490 ± 0.172  -0.767 ± 0.191  -0.539 ± 0.197 
#>  [81] -0.162 ± 0.100  -0.222 ± 0.138  -0.345 ± 0.081  -0.277 ± 0.091 
#>  [85] -0.131 ± 0.074  -1.132 ± 0.323  -0.820 ± 0.128  -0.772 ± 0.240 
#>  [89] -1.284 ± 0.320  -0.260 ± 0.135  -0.384 ± 0.130  -1.492 ± 0.343 
#>  [93] -0.736 ± 0.219  -0.319 ± 0.087  -0.391 ± 0.112  -1.568 ± 0.282 
#>  [97] -0.433 ± 0.102  -1.057 ± 0.378  -0.690 ± 0.141  -0.393 ± 0.097 
#> 
posterior::as_draws_list(fit)
#> # A draws_list: 1000 iterations, 4 chains, and 105 variables
#> 
#> [chain = 1]
#> $lp__
#>  [1] -66 -66 -68 -67 -71 -67 -67 -65 -65 -67
#> 
#> $alpha
#>  [1]  0.272  0.156  0.930  0.336  0.617  0.180  0.036  0.480  0.579 -0.027
#> 
#> $`beta[1]`
#>  [1] -0.86 -0.67 -0.72 -0.93 -0.50 -0.30 -0.62 -0.61 -0.42 -0.98
#> 
#> $`beta[2]`
#>  [1]  0.044 -0.523 -0.218 -0.327 -0.567 -0.537 -0.594 -0.537 -0.346 -0.186
#> 
#> 
#> [chain = 2]
#> $lp__
#>  [1] -67 -64 -66 -70 -65 -66 -65 -64 -66 -65
#> 
#> $alpha
#>  [1]  0.79  0.38  0.11 -0.24  0.59  0.16  0.40  0.48  0.17  0.56
#> 
#> $`beta[1]`
#>  [1] -0.38 -0.60 -0.49 -0.27 -0.72 -0.96 -0.36 -0.60 -0.50 -0.81
#> 
#> $`beta[2]`
#>  [1] -0.061 -0.331 -0.253 -0.124 -0.360 -0.115 -0.286 -0.271  0.014 -0.391
#> 
#> # ... with 990 more iterations, and 2 more chains, and 101 more variables
# }
```
