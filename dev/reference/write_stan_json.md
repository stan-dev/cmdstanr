# Write data to a JSON file readable by CmdStan

Write data to a JSON file readable by CmdStan

## Usage

``` r
write_stan_json(data, file, always_decimal = FALSE)
```

## Arguments

- data:

  (list) A named list of R objects.

- file:

  (string) The path to where the data file should be written.

- always_decimal:

  (logical) Force generate non-integers with decimal points to better
  distinguish between integers and floating point values. If `TRUE` all
  R objects in `data` intended for integers must be of integer type.

## Value

`NULL`, invisibly.

## Details

`write_stan_json()` performs several conversions before writing the JSON
file:

- `logical` -\> `integer` (`TRUE` -\> `1`, `FALSE` -\> `0`)

- `factor` -\> `integer` (the index of each value's level)

- `data.frame` -\> `matrix` (via
  [`data.matrix()`](https://rdrr.io/r/base/data.matrix.html)); every
  column must be numeric, integer, logical, or factor

- `list` -\> `array`

- `table` -\> `vector`, `matrix`, or `array` (depending on dimensions of
  table)

### Factor conversion

Factors are written as their level indices, i.e., the position of each
value in `levels(x)` rather than the value itself. The default levels
are the sorted unique values, e.g., `factor(c(10, 9, 8))` has levels
`8`, `9`, `10` and is written as `[3, 2, 1]`. An unused level shifts the
indices of the levels after it. The fitting methods of a model compiled
from a Stan file will error if a factor is supplied for a variable that
is not declared as `int`, but if `write_stan_json()` is called directly
by the user it has no declarations to check and so it always does the
conversion.

### List to array conversion

The `list` to `array` conversion is intended to make it easier to
prepare the data for certain Stan declarations involving arrays:

- `array[K] vector[J] v ` can be constructed in R as a list with `K`
  elements where each element is a vector of length `J`

- `array[K] matrix[I,J] m ` can be constructed in R as a list with `K`
  elements where each element is an `IxJ` matrix

- `array[K,I,J] int n ` can be constructed in R as a list with `K`
  elements where each element is an `IxJ` matrix of integers

These can also be passed in from R as arrays instead of lists but the
list option is provided for convenience. A list always contributes
exactly one leading dimension, so `array[K,L] vector[J] v ` can be
supplied either as a list of `K` matrices each with dimensions `LxJ` or
as a single R array with dimensions `KxLxJ`. Nested lists are not
supported: every element of the list must be a vector, matrix, or array.

### Scalar vs. length-1 vector

Because R does not distinguish between a scalar and a vector of length
1, a length-1 vector like `c(42)` is written to JSON as a scalar (`42`)
rather than an array (`[42]`). If a Stan variable is declared as a
vector or array that may have length 1, wrap the value in
[`array()`](https://rdrr.io/r/base/array.html) to force array output.
Because [`array()`](https://rdrr.io/r/base/array.html) uses the length
of its input as the default dimension, this works regardless of length:

- `write_stan_json(list(x = array(42)), file)` writes `"x": [42]`

- `write_stan_json(list(x = array(c(42, 43))), file)` writes
  `"x": [42, 43]`

This is only necessary when calling `write_stan_json()` directly. When
passing a data list to the fitting methods of a model compiled from a
Stan file (e.g., `$sample()`), CmdStanR uses the model's variable
declarations to make this correction automatically.

## See also

[`$variables()`](https://mc-stan.org/cmdstanr/dev/reference/model-method-variables.md)
for inspecting the input and output variables of a Stan program.

## Examples

``` r
x <- matrix(rnorm(10), 5, 2)
y <- rpois(nrow(x), lambda = 10)
z <- c(TRUE, FALSE)
data <- list(N = nrow(x), K = ncol(x), x = x, y = y, z = z)

# write data to json file
file <- tempfile(fileext = ".json")
write_stan_json(data, file)

# check the contents of the file
cat(readLines(file), sep = "\n")
#> {
#>   "N": 5,
#>   "K": 2,
#>   "x": [
#>     [0.675270015431045, 1.0439388958914],
#>     [-0.595348959958748, -0.302706284526524],
#>     [0.110090411151627, 1.41728126064142],
#>     [0.372003230867959, -0.867101064629448],
#>     [-0.609858300092441, -2.21814114794348]
#>   ],
#>   "y": [9, 11, 13, 10, 13],
#>   "z": [1, 0]
#> }


# demonstrating list to array conversion
# suppose x is declared as `array[2] vector[3] x`
# we can use a list of length 2 where each element is a vector of length 3
data <- list(x = list(1:3, 4:6))
file <- tempfile(fileext = ".json")
write_stan_json(data, file)
cat(readLines(file), sep = "\n")
#> {
#>   "x": [
#>     [1, 2, 3],
#>     [4, 5, 6]
#>   ]
#> }
```
