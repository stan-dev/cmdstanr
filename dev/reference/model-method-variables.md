# Input and output variables of a Stan program

The `$variables()` method of a
[`CmdStanModel`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanModel.md)
object returns a list, each element representing a Stan model block:
`data`, `parameters`, `transformed_parameters` and
`generated_quantities`.

Each element contains a list of variables, with each variables
represented as a list with infromation on its scalar type (`real` or
`int`) and number of dimensions.

`transformed data` is not included, as variables in that block are not
part of the model's input or output.

## Usage

``` r
variables()
```

## Value

The `$variables()` returns a list with information on input and output
variables for each of the Stan model blocks.

## See also

Other CmdStanModel methods:
[`model-method-check_syntax`](https://mc-stan.org/cmdstanr/dev/reference/model-method-check_syntax.md),
[`model-method-compile`](https://mc-stan.org/cmdstanr/dev/reference/model-method-compile.md),
[`model-method-diagnose`](https://mc-stan.org/cmdstanr/dev/reference/model-method-diagnose.md),
[`model-method-expose_functions`](https://mc-stan.org/cmdstanr/dev/reference/model-method-expose_functions.md),
[`model-method-format`](https://mc-stan.org/cmdstanr/dev/reference/model-method-format.md),
[`model-method-generate-quantities`](https://mc-stan.org/cmdstanr/dev/reference/model-method-generate-quantities.md),
[`model-method-laplace`](https://mc-stan.org/cmdstanr/dev/reference/model-method-laplace.md),
[`model-method-optimize`](https://mc-stan.org/cmdstanr/dev/reference/model-method-optimize.md),
[`model-method-pathfinder`](https://mc-stan.org/cmdstanr/dev/reference/model-method-pathfinder.md),
[`model-method-sample`](https://mc-stan.org/cmdstanr/dev/reference/model-method-sample.md),
[`model-method-sample_mpi`](https://mc-stan.org/cmdstanr/dev/reference/model-method-sample_mpi.md),
[`model-method-variational`](https://mc-stan.org/cmdstanr/dev/reference/model-method-variational.md)

## Examples

``` r
# \dontrun{
file <- file.path(cmdstan_path(), "examples/bernoulli/bernoulli.stan")

# create a `CmdStanModel` object, compiling the model is not required
mod <- cmdstan_model(file, compile = FALSE)

mod$variables()
#> $parameters
#> $parameters$theta
#> $parameters$theta$type
#> [1] "real"
#> 
#> $parameters$theta$dimensions
#> [1] 0
#> 
#> 
#> 
#> $included_files
#> list()
#> 
#> $data
#> $data$N
#> $data$N$type
#> [1] "int"
#> 
#> $data$N$dimensions
#> [1] 0
#> 
#> 
#> $data$y
#> $data$y$type
#> [1] "int"
#> 
#> $data$y$dimensions
#> [1] 1
#> 
#> 
#> 
#> $transformed_parameters
#> named list()
#> 
#> $generated_quantities
#> named list()
#> 

# }
```
