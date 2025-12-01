# CmdStanDiagnose objects

A `CmdStanDiagnose` object is the object returned by the
[`$diagnose()`](https://mc-stan.org/cmdstanr/dev/reference/model-method-diagnose.md)
method of a
[`CmdStanModel`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanModel.md)
object.

## Methods

`CmdStanDiagnose` objects have the following associated methods:

|                                                                                                      |                                                                |
|------------------------------------------------------------------------------------------------------|----------------------------------------------------------------|
| **Method**                                                                                           | **Description**                                                |
| [`$gradients()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-gradients.md)                 | Return gradients from diagnostic mode.                         |
| [`$lp()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-lp.md)                               | Return the total log probability density (`target`).           |
| [`$init()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-init.md)                           | Return user-specified initial values.                          |
| [`$metadata()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-metadata.md)                   | Return a list of metadata gathered from the CmdStan CSV files. |
| [`$save_output_files()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-save_output_files.md) | Save output CSV files to a specified location.                 |
| [`$save_data_file()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-save_output_files.md)    | Save JSON data file to a specified location.                   |

## See also

The CmdStanR website
([mc-stan.org/cmdstanr](https://mc-stan.org/cmdstanr/)) for online
documentation and tutorials.

The Stan and CmdStan documentation:

- Stan documentation:
  [mc-stan.org/users/documentation](https://mc-stan.org/users/documentation/)

- CmdStan User’s Guide:
  [mc-stan.org/docs/cmdstan-guide](https://mc-stan.org/docs/cmdstan-guide/)

Other fitted model objects:
[`CmdStanGQ`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanGQ.md),
[`CmdStanLaplace`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanLaplace.md),
[`CmdStanMCMC`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMCMC.md),
[`CmdStanMLE`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMLE.md),
[`CmdStanPathfinder`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanPathfinder.md),
[`CmdStanVB`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanVB.md)

## Examples

``` r
# \dontrun{
test <- cmdstanr_example("logistic", method = "diagnose")

# retrieve the gradients
test$gradients()
#>   param_idx     value     model finite_diff        error
#> 1         0  1.710110 -18.30620   -18.30620 -1.81553e-08
#> 2         1 -0.468770  -5.97695    -5.97695  1.10331e-08
#> 3         2 -1.800050  20.42290    20.42290  1.19712e-08
#> 4         3 -0.508314  16.91120    16.91120  3.84095e-10
# }
```
