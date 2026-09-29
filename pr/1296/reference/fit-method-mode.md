# Extract the mode used for a Laplace approximation

The `$mode()` method returns the mode used to center the Laplace
approximation. This method is only available for
[`CmdStanLaplace`](https://mc-stan.org/cmdstanr/reference/CmdStanLaplace.md)
objects returned by
[`$laplace()`](https://mc-stan.org/cmdstanr/reference/model-method-laplace.md),
not objects reconstructed using
[`as_cmdstan_fit()`](https://mc-stan.org/cmdstanr/reference/read_cmdstan_csv.md).

## Usage

``` r
mode()
```

## Value

A [`CmdStanMLE`](https://mc-stan.org/cmdstanr/reference/CmdStanMLE.md)
object.

## See also

[`CmdStanLaplace`](https://mc-stan.org/cmdstanr/reference/CmdStanLaplace.md),
[`$laplace()`](https://mc-stan.org/cmdstanr/reference/model-method-laplace.md)
