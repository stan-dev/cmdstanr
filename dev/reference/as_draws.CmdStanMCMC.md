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
#>         1 -65 -64 -66 -66
#>         2 -65 -64 -65 -66
#>         3 -67 -66 -65 -67
#>         4 -68 -65 -69 -65
#>         5 -66 -65 -68 -65
#> 
#> , , variable = alpha
#> 
#>          chain
#> iteration    1    2    3    4
#>         1 0.51 0.29 0.31 0.68
#>         2 0.49 0.43 0.21 0.72
#>         3 0.73 0.52 0.54 0.86
#>         4 0.53 0.40 0.83 0.31
#>         5 0.51 0.14 0.12 0.47
#> 
#> , , variable = beta[1]
#> 
#>          chain
#> iteration     1     2     3     4
#>         1 -0.67 -0.76 -0.28 -0.43
#>         2 -0.41 -0.62 -0.37 -0.49
#>         3 -0.36 -0.26 -0.60 -0.56
#>         4 -1.08 -0.50 -0.68 -0.39
#>         5 -0.63 -0.58 -0.49 -0.87
#> 
#> , , variable = beta[2]
#> 
#>          chain
#> iteration     1     2     3     4
#>         1 -0.58 -0.12 -0.54 -0.16
#>         2 -0.44 -0.27 -0.22 -0.28
#>         3 -0.42 -0.37 -0.48 -0.15
#>         4  0.19 -0.32 -0.56 -0.29
#>         5 -0.37 -0.21 -0.19 -0.24
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
#> [1] -0.67 ± 0.26  -0.28 ± 0.23   0.68 ± 0.28 
#> 
#> $log_lik: rvar<1000,4>[100] mean ± sd:
#>   [1] -0.516 ± 0.101  -0.406 ± 0.153  -0.502 ± 0.226  -0.450 ± 0.154 
#>   [5] -1.180 ± 0.292  -0.593 ± 0.195  -0.640 ± 0.126  -0.277 ± 0.137 
#>   [9] -0.694 ± 0.170  -0.742 ± 0.240  -0.282 ± 0.129  -0.502 ± 0.246 
#>  [13] -0.656 ± 0.211  -0.363 ± 0.178  -0.281 ± 0.111  -0.276 ± 0.089 
#>  [17] -1.596 ± 0.295  -0.478 ± 0.109  -0.232 ± 0.077  -0.115 ± 0.082 
#>  [21] -0.213 ± 0.090  -0.569 ± 0.151  -0.329 ± 0.141  -0.136 ± 0.068 
#>  [25] -0.455 ± 0.123  -1.526 ± 0.346  -0.308 ± 0.125  -0.445 ± 0.085 
#>  [29] -0.722 ± 0.232  -0.693 ± 0.196  -0.489 ± 0.166  -0.425 ± 0.111 
#>  [33] -0.411 ± 0.130  -0.064 ± 0.052  -0.584 ± 0.185  -0.323 ± 0.134 
#>  [37] -0.702 ± 0.233  -0.312 ± 0.149  -0.182 ± 0.114  -0.680 ± 0.130 
#>  [41] -1.132 ± 0.261  -0.930 ± 0.202  -0.408 ± 0.269  -1.176 ± 0.192 
#>  [45] -0.359 ± 0.121  -0.583 ± 0.132  -0.306 ± 0.131  -0.323 ± 0.085 
#>  [49] -0.319 ± 0.082  -1.296 ± 0.336  -0.287 ± 0.096  -0.836 ± 0.145 
#>  [53] -0.403 ± 0.133  -0.371 ± 0.145  -0.388 ± 0.140  -0.318 ± 0.195 
#>  [57] -0.657 ± 0.120  -0.956 ± 0.361  -1.359 ± 0.352  -0.980 ± 0.162 
#>  [61] -0.540 ± 0.100  -0.881 ± 0.321  -0.117 ± 0.074  -0.907 ± 0.254 
#>  [65] -2.014 ± 0.616  -0.510 ± 0.140  -0.276 ± 0.083  -1.064 ± 0.241 
#>  [69] -0.436 ± 0.086  -0.636 ± 0.241  -0.608 ± 0.209  -0.463 ± 0.175 
#>  [73] -1.485 ± 0.374  -0.948 ± 0.202  -1.148 ± 0.398  -0.374 ± 0.144 
#>  [77] -0.878 ± 0.140  -0.486 ± 0.177  -0.765 ± 0.195  -0.547 ± 0.203 
#>  [81] -0.164 ± 0.101  -0.227 ± 0.144  -0.344 ± 0.084  -0.277 ± 0.095 
#>  [85] -0.131 ± 0.078  -1.124 ± 0.330  -0.825 ± 0.129  -0.776 ± 0.245 
#>  [89] -1.277 ± 0.329  -0.260 ± 0.135  -0.386 ± 0.132  -1.495 ± 0.359 
#>  [93] -0.739 ± 0.224  -0.318 ± 0.091  -0.388 ± 0.114  -1.574 ± 0.293 
#>  [97] -0.429 ± 0.102  -1.058 ± 0.379  -0.695 ± 0.144  -0.390 ± 0.099 
#> 
posterior::as_draws_list(fit)
#> # A draws_list: 1000 iterations, 4 chains, and 105 variables
#> 
#> [chain = 1]
#> $lp__
#>  [1] -65 -65 -67 -68 -66 -64 -68 -66 -66 -66
#> 
#> $alpha
#>  [1] 0.510 0.490 0.731 0.531 0.507 0.314 0.206 0.541 0.012 0.753
#> 
#> $`beta[1]`
#>  [1] -0.67 -0.41 -0.36 -1.08 -0.63 -0.56 -0.75 -0.84 -0.59 -0.73
#> 
#> $`beta[2]`
#>  [1] -0.58 -0.44 -0.42  0.19 -0.37 -0.29 -0.87  0.17 -0.41 -0.21
#> 
#> 
#> [chain = 2]
#> $lp__
#>  [1] -64 -64 -66 -65 -65 -65 -67 -65 -65 -66
#> 
#> $alpha
#>  [1] 0.294 0.432 0.519 0.397 0.137 0.493 0.614 0.452 0.091 0.567
#> 
#> $`beta[1]`
#>  [1] -0.76 -0.62 -0.26 -0.50 -0.58 -0.61 -0.21 -0.95 -0.77 -0.26
#> 
#> $`beta[2]`
#>  [1] -0.116 -0.272 -0.373 -0.322 -0.209 -0.197 -0.398 -0.098 -0.375 -0.324
#> 
#> # ... with 990 more iterations, and 2 more chains, and 101 more variables
# }
```
