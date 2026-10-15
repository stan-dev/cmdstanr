# Create a `draws` object from a CmdStanR fitted model object

Create a `draws` object supported by the posterior package. These
methods are just wrappers around CmdStanR's
[`$draws()`](https://mc-stan.org/cmdstanr/rc/reference/fit-method-draws.md)
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
  [`$draws()`](https://mc-stan.org/cmdstanr/rc/reference/fit-method-draws.md)
  method (e.g., `variables`, `inc_warmup`, etc.).

## Value

A `posterior::draws_*` object. The default format depends on the fitted
model class and can be changed using arguments passed through `...`.

## Details

To subset iterations, chains, or draws, use the
[`posterior::subset_draws()`](https://mc-stan.org/posterior/reference/subset_draws.html)
method after creating the `draws` object.

## See also

[`$draws()`](https://mc-stan.org/cmdstanr/rc/reference/fit-method-draws.md)
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
#>         1 -65 -65 -65 -66
#>         2 -65 -65 -65 -67
#>         3 -65 -67 -64 -66
#>         4 -67 -67 -66 -66
#>         5 -67 -67 -66 -66
#> 
#> , , variable = alpha
#> 
#>          chain
#> iteration      1    2    3    4
#>         1  0.539 0.35 0.49 0.22
#>         2  0.138 0.35 0.21 0.52
#>         3  0.595 0.50 0.36 0.70
#>         4 -0.049 0.60 0.56 0.46
#>         5  0.892 0.10 0.28 0.76
#> 
#> , , variable = beta[1]
#> 
#>          chain
#> iteration     1     2     3     4
#>         1 -0.73 -0.59 -0.72 -1.06
#>         2 -0.57 -0.99 -0.97 -1.12
#>         3 -0.78 -0.14 -0.59 -0.76
#>         4 -0.49 -0.16 -1.11 -1.00
#>         5 -0.76 -1.11 -0.75 -0.78
#> 
#> , , variable = beta[2]
#> 
#>          chain
#> iteration      1      2     3      4
#>         1 -0.171  0.027 -0.46 -0.108
#>         2 -0.266 -0.213 -0.27 -0.030
#>         3 -0.231 -0.176 -0.45 -0.025
#>         4 -0.031 -0.324 -0.28 -0.244
#>         5 -0.067 -0.174 -0.24 -0.360
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
#> [1] -0.66 ± 0.25  -0.28 ± 0.23   0.68 ± 0.27 
#> 
#> $log_lik: rvar<1000,4>[100] mean ± sd:
#>   [1] -0.517 ± 0.099  -0.404 ± 0.152  -0.504 ± 0.222  -0.445 ± 0.149 
#>   [5] -1.182 ± 0.281  -0.587 ± 0.191  -0.641 ± 0.123  -0.274 ± 0.134 
#>   [9] -0.689 ± 0.168  -0.745 ± 0.233  -0.277 ± 0.124  -0.496 ± 0.244 
#>  [13] -0.660 ± 0.209  -0.366 ± 0.178  -0.279 ± 0.108  -0.273 ± 0.087 
#>  [17] -1.596 ± 0.291  -0.477 ± 0.108  -0.232 ± 0.076  -0.115 ± 0.083 
#>  [21] -0.210 ± 0.087  -0.565 ± 0.149  -0.327 ± 0.140  -0.136 ± 0.068 
#>  [25] -0.454 ± 0.120  -1.522 ± 0.349  -0.304 ± 0.120  -0.444 ± 0.084 
#>  [29] -0.719 ± 0.223  -0.690 ± 0.195  -0.483 ± 0.162  -0.423 ± 0.107 
#>  [33] -0.411 ± 0.129  -0.063 ± 0.051  -0.585 ± 0.181  -0.322 ± 0.134 
#>  [37] -0.703 ± 0.224  -0.308 ± 0.145  -0.182 ± 0.114  -0.678 ± 0.129 
#>  [41] -1.138 ± 0.256  -0.929 ± 0.200  -0.413 ± 0.268  -1.175 ± 0.186 
#>  [45] -0.361 ± 0.120  -0.583 ± 0.131  -0.303 ± 0.128  -0.323 ± 0.084 
#>  [49] -0.318 ± 0.080  -1.289 ± 0.339  -0.288 ± 0.096  -0.833 ± 0.143 
#>  [53] -0.398 ± 0.129  -0.374 ± 0.145  -0.387 ± 0.138  -0.317 ± 0.195 
#>  [57] -0.657 ± 0.116  -0.954 ± 0.348  -1.358 ± 0.347  -0.977 ± 0.159 
#>  [61] -0.541 ± 0.098  -0.872 ± 0.320  -0.115 ± 0.072  -0.909 ± 0.253 
#>  [65] -2.029 ± 0.600  -0.508 ± 0.135  -0.275 ± 0.082  -1.068 ± 0.238 
#>  [69] -0.435 ± 0.083  -0.635 ± 0.241  -0.612 ± 0.207  -0.466 ± 0.173 
#>  [73] -1.487 ± 0.365  -0.945 ± 0.192  -1.137 ± 0.397  -0.376 ± 0.142 
#>  [77] -0.879 ± 0.136  -0.491 ± 0.176  -0.767 ± 0.187  -0.545 ± 0.202 
#>  [81] -0.162 ± 0.099  -0.226 ± 0.143  -0.343 ± 0.082  -0.276 ± 0.093 
#>  [85] -0.131 ± 0.078  -1.125 ± 0.327  -0.824 ± 0.128  -0.781 ± 0.242 
#>  [89] -1.278 ± 0.323  -0.258 ± 0.132  -0.387 ± 0.130  -1.499 ± 0.345 
#>  [93] -0.733 ± 0.215  -0.318 ± 0.089  -0.385 ± 0.111  -1.574 ± 0.285 
#>  [97] -0.430 ± 0.102  -1.056 ± 0.367  -0.697 ± 0.143  -0.392 ± 0.098 
#> 
posterior::as_draws_list(fit)
#> # A draws_list: 1000 iterations, 4 chains, and 105 variables
#> 
#> [chain = 1]
#> $lp__
#>  [1] -65 -65 -65 -67 -67 -65 -64 -65 -65 -68
#> 
#> $alpha
#>  [1]  0.539  0.138  0.595 -0.049  0.892  0.030  0.334  0.336  0.660 -0.070
#> 
#> $`beta[1]`
#>  [1] -0.73 -0.57 -0.78 -0.49 -0.76 -0.65 -0.56 -0.92 -0.58 -0.93
#> 
#> $`beta[2]`
#>  [1] -0.171 -0.266 -0.231 -0.031 -0.067 -0.255 -0.211 -0.252 -0.256 -0.314
#> 
#> 
#> [chain = 2]
#> $lp__
#>  [1] -65 -65 -67 -67 -67 -66 -65 -64 -64 -64
#> 
#> $alpha
#>  [1] 0.35 0.35 0.50 0.60 0.10 0.12 0.25 0.44 0.49 0.45
#> 
#> $`beta[1]`
#>  [1] -0.59 -0.99 -0.14 -0.16 -1.11 -0.61 -0.41 -0.61 -0.57 -0.64
#> 
#> $`beta[2]`
#>  [1]  0.027 -0.213 -0.176 -0.324 -0.174 -0.118 -0.291 -0.131 -0.353 -0.357
#> 
#> # ... with 990 more iterations, and 2 more chains, and 101 more variables
# }
```
