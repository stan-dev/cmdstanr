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

## Value

A `posterior::draws_*` object. The default format depends on the fitted
model class and can be changed using arguments passed through `...`.

## Details

To subset iterations, chains, or draws, use the
[`posterior::subset_draws()`](https://mc-stan.org/posterior/reference/subset_draws.html)
method after creating the `draws` object.

## See also

[`$draws()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-draws.md)
and
[`posterior::as_draws()`](https://mc-stan.org/posterior/reference/draws.html)

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
#>         1 -66 -67 -66 -66
#>         2 -67 -67 -66 -65
#>         3 -66 -68 -64 -65
#>         4 -67 -68 -65 -65
#>         5 -69 -67 -64 -66
#> 
#> , , variable = alpha
#> 
#>          chain
#> iteration    1    2    3    4
#>         1 0.78 0.21 0.21 0.55
#>         2 0.78 0.20 0.52 0.31
#>         3 0.57 0.56 0.30 0.53
#>         4 0.11 0.24 0.38 0.63
#>         5 0.46 0.54 0.26 0.24
#> 
#> , , variable = beta[1]
#> 
#>          chain
#> iteration     1     2     3     4
#>         1 -0.76 -0.80 -0.83 -0.71
#>         2 -0.46 -0.67 -0.47 -0.41
#>         3 -1.07 -1.26 -0.76 -0.54
#>         4 -0.31 -0.11 -0.33 -0.49
#>         5 -0.31 -1.21 -0.81 -0.71
#> 
#> , , variable = beta[2]
#> 
#>          chain
#> iteration       1      2     3      4
#>         1 -0.1912 -0.710 -0.26 -0.614
#>         2 -0.0880 -0.524  0.16 -0.181
#>         3 -0.4462 -0.528 -0.20 -0.097
#>         4  0.0062 -0.015 -0.27 -0.232
#>         5 -0.2247 -0.510 -0.15 -0.549
#> 
#> # ... with 995 more iterations, and 101 more variables

# posterior's as_draws_*() methods will also work
posterior::as_draws_rvars(fit)
#> # A draws_rvars: 1000 iterations, 4 chains, and 4 variables
#> $lp__: rvar<1000,4>[1] mean ± sd:
#> [1] -66 ± 1.4 
#> 
#> $alpha: rvar<1000,4>[1] mean ± sd:
#> [1] 0.38 ± 0.22 
#> 
#> $beta: rvar<1000,4>[3] mean ± sd:
#> [1] -0.66 ± 0.25  -0.28 ± 0.23   0.68 ± 0.26 
#> 
#> $log_lik: rvar<1000,4>[100] mean ± sd:
#>   [1] -0.518 ± 0.098  -0.404 ± 0.149  -0.503 ± 0.219  -0.447 ± 0.151 
#>   [5] -1.176 ± 0.275  -0.592 ± 0.192  -0.640 ± 0.127  -0.277 ± 0.136 
#>   [9] -0.693 ± 0.171  -0.744 ± 0.235  -0.280 ± 0.125  -0.496 ± 0.235 
#>  [13] -0.657 ± 0.210  -0.363 ± 0.170  -0.280 ± 0.105  -0.276 ± 0.087 
#>  [17] -1.591 ± 0.282  -0.479 ± 0.109  -0.232 ± 0.075  -0.113 ± 0.075 
#>  [21] -0.212 ± 0.087  -0.568 ± 0.151  -0.328 ± 0.140  -0.136 ± 0.065 
#>  [25] -0.455 ± 0.123  -1.519 ± 0.331  -0.307 ± 0.122  -0.446 ± 0.084 
#>  [29] -0.718 ± 0.223  -0.692 ± 0.195  -0.488 ± 0.163  -0.425 ± 0.108 
#>  [33] -0.411 ± 0.129  -0.063 ± 0.050  -0.583 ± 0.183  -0.323 ± 0.132 
#>  [37] -0.704 ± 0.228  -0.310 ± 0.147  -0.180 ± 0.105  -0.680 ± 0.132 
#>  [41] -1.132 ± 0.257  -0.930 ± 0.203  -0.406 ± 0.256  -1.172 ± 0.181 
#>  [45] -0.360 ± 0.114  -0.583 ± 0.131  -0.305 ± 0.128  -0.324 ± 0.082 
#>  [49] -0.320 ± 0.079  -1.288 ± 0.321  -0.288 ± 0.092  -0.832 ± 0.141 
#>  [53] -0.402 ± 0.131  -0.372 ± 0.138  -0.387 ± 0.137  -0.316 ± 0.190 
#>  [57] -0.659 ± 0.119  -0.956 ± 0.356  -1.358 ± 0.347  -0.976 ± 0.155 
#>  [61] -0.541 ± 0.098  -0.872 ± 0.306  -0.116 ± 0.073  -0.906 ± 0.249 
#>  [65] -2.013 ± 0.591  -0.510 ± 0.136  -0.276 ± 0.081  -1.064 ± 0.238 
#>  [69] -0.437 ± 0.084  -0.634 ± 0.235  -0.609 ± 0.208  -0.464 ± 0.174 
#>  [73] -1.486 ± 0.371  -0.943 ± 0.191  -1.137 ± 0.378  -0.375 ± 0.136 
#>  [77] -0.877 ± 0.139  -0.487 ± 0.169  -0.767 ± 0.192  -0.544 ± 0.195 
#>  [81] -0.163 ± 0.100  -0.225 ± 0.139  -0.344 ± 0.079  -0.276 ± 0.089 
#>  [85] -0.130 ± 0.072  -1.123 ± 0.320  -0.823 ± 0.128  -0.776 ± 0.242 
#>  [89] -1.276 ± 0.321  -0.260 ± 0.134  -0.387 ± 0.132  -1.490 ± 0.336 
#>  [93] -0.734 ± 0.214  -0.319 ± 0.085  -0.388 ± 0.113  -1.568 ± 0.274 
#>  [97] -0.430 ± 0.101  -1.058 ± 0.375  -0.695 ± 0.144  -0.391 ± 0.094 
#> 
posterior::as_draws_list(fit)
#> # A draws_list: 1000 iterations, 4 chains, and 105 variables
#> 
#> [chain = 1]
#> $lp__
#>  [1] -66 -67 -66 -67 -69 -65 -64 -68 -65 -67
#> 
#> $alpha
#>  [1] 0.78 0.78 0.57 0.11 0.46 0.21 0.44 0.75 0.11 0.14
#> 
#> $`beta[1]`
#>  [1] -0.76 -0.46 -1.07 -0.31 -0.31 -0.69 -0.52 -0.47 -0.72 -0.73
#> 
#> $`beta[2]`
#>  [1] -0.1912 -0.0880 -0.4462  0.0062 -0.2247 -0.1402 -0.3922  0.1708 -0.1849
#> [10]  0.2095
#> 
#> 
#> [chain = 2]
#> $lp__
#>  [1] -67 -67 -68 -68 -67 -65 -64 -65 -65 -66
#> 
#> $alpha
#>  [1] 0.21 0.20 0.56 0.24 0.54 0.35 0.41 0.34 0.35 0.22
#> 
#> $`beta[1]`
#>  [1] -0.80 -0.67 -1.26 -0.11 -1.21 -0.54 -0.69 -0.61 -0.73 -0.39
#> 
#> $`beta[2]`
#>  [1] -0.710 -0.524 -0.528 -0.015 -0.510 -0.355 -0.120 -0.439 -0.066  0.075
#> 
#> # ... with 990 more iterations, and 2 more chains, and 101 more variables
# }
```
