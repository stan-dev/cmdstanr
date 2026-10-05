# Input and output variables of a Stan program

The `$variables()` method of a
[`CmdStanModel`](https://mc-stan.org/cmdstanr/pr/1306/reference/CmdStanModel.md)
object returns a list, each element representing a Stan model block:
`data`, `parameters`, `transformed_parameters` and
`generated_quantities`. The information describes the program the
executable was built from, captured when the model object was created.

The standalone function `variables_stan_file()` returns the same list
for a Stan program as it is now, without creating a model object or
compiling.

Each element in the returned object contains a list of variables, with
each variable represented as a list with information on its scalar type
(`real` or `int`) and number of dimensions.

The number of dimensions reported is the number of indexing dimensions
in the declared Stan variable, equivalently the number of indices needed
to access one scalar element. This means a scalar has 0 dimensions, a
vector or one-dimensional array has 1, and a matrix or two-dimensional
array has 2. Array dimensions are added to any vector or matrix
dimensions, so `array[J] matrix[N, K]` has 3 dimensions. See
**Examples**.

`transformed data` is not included, as variables in that block are not
part of the model's input or output.

## Usage

``` r
variables()

variables_stan_file(stan_file, include_paths = NULL)
```

## Arguments

- stan_file:

  (string) The path to a Stan program.

- include_paths:

  (character vector) Paths to directories where Stan should look for
  files specified in `#include` directives in the Stan program. Relative
  paths are resolved against the working directory when the model object
  is created and stored as absolute paths, so subsequent changes to the
  working directory do not affect them. When the program contains
  `#include` directives and no paths are given, the program's own
  directory is used.

## Value

A list with information on input and output variables for each of the
Stan model blocks.

## See also

[`write_stan_json()`](https://mc-stan.org/cmdstanr/pr/1306/reference/write_stan_json.md)
for writing data for CmdStan.

Other CmdStanModel methods:
[`model-method-build_info`](https://mc-stan.org/cmdstanr/pr/1306/reference/model-method-build_info.md),
[`model-method-check_syntax`](https://mc-stan.org/cmdstanr/pr/1306/reference/model-method-check_syntax.md),
[`model-method-cmdstan_defaults`](https://mc-stan.org/cmdstanr/pr/1306/reference/model-method-cmdstan_defaults.md),
[`model-method-diagnose`](https://mc-stan.org/cmdstanr/pr/1306/reference/model-method-diagnose.md),
[`model-method-expose_functions`](https://mc-stan.org/cmdstanr/pr/1306/reference/model-method-expose_functions.md),
[`model-method-format`](https://mc-stan.org/cmdstanr/pr/1306/reference/model-method-format.md),
[`model-method-generate-quantities`](https://mc-stan.org/cmdstanr/pr/1306/reference/model-method-generate-quantities.md),
[`model-method-is_current`](https://mc-stan.org/cmdstanr/pr/1306/reference/model-method-is_current.md),
[`model-method-laplace`](https://mc-stan.org/cmdstanr/pr/1306/reference/model-method-laplace.md),
[`model-method-model-info`](https://mc-stan.org/cmdstanr/pr/1306/reference/model-method-model-info.md),
[`model-method-optimize`](https://mc-stan.org/cmdstanr/pr/1306/reference/model-method-optimize.md),
[`model-method-pathfinder`](https://mc-stan.org/cmdstanr/pr/1306/reference/model-method-pathfinder.md),
[`model-method-sample`](https://mc-stan.org/cmdstanr/pr/1306/reference/model-method-sample.md),
[`model-method-sample_mpi`](https://mc-stan.org/cmdstanr/pr/1306/reference/model-method-sample_mpi.md),
[`model-method-variational`](https://mc-stan.org/cmdstanr/pr/1306/reference/model-method-variational.md)

## Examples

``` r
# \dontrun{
stan_file <- write_stan_file("
data {
  int N;
  array[2, 3] int y;
}
parameters {
  real alpha;
  vector[N] beta;
  array[2] matrix[3, 4] theta;
}
")

mod <- cmdstan_model(stan_file)

vars <- mod$variables()
str(vars)
#> List of 5
#>  $ parameters            :List of 3
#>   ..$ alpha:List of 2
#>   .. ..$ type      : chr "real"
#>   .. ..$ dimensions: int 0
#>   ..$ beta :List of 2
#>   .. ..$ type      : chr "real"
#>   .. ..$ dimensions: int 1
#>   ..$ theta:List of 2
#>   .. ..$ type      : chr "real"
#>   .. ..$ dimensions: int 3
#>  $ included_files        : list()
#>  $ data                  :List of 2
#>   ..$ N:List of 2
#>   .. ..$ type      : chr "int"
#>   .. ..$ dimensions: int 0
#>   ..$ y:List of 2
#>   .. ..$ type      : chr "int"
#>   .. ..$ dimensions: int 2
#>  $ transformed_parameters: Named list()
#>  $ generated_quantities  : Named list()
# }
```
