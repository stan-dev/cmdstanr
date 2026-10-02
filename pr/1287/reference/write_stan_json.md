# Write data to a JSON file readable by CmdStan

Write data to a JSON file readable by CmdStan

## Usage

``` r
write_stan_json(data, file, always_decimal = FALSE, variables = NULL)
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

- variables:

  (list) Optionally, the Stan declarations of the variables in `data`,
  so that they are written the way the Stan program expects. Use
  `mod$variables()$data` (see
  [`$variables()`](https://mc-stan.org/cmdstanr/pr/1287/reference/model-method-variables.md))
  or `variables_stan_file(stan_file)$data`. Given the declarations, an
  unnamed list is written as a tuple when the variable is a tuple (see
  **Tuples** below), a length-1 value is written as an array when the
  variable is declared with a dimension (see **Scalar vs. length-1
  vector** below), and a factor is only accepted for an `int` variable.
  The fitting methods of a model compiled from a Stan file pass the
  declarations themselves. Without them every unnamed list is converted
  to an array, a length-1 value is written as a scalar, and every factor
  is converted to its level indices.

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

- `complex` -\> the pair `[re, im]`; a complex vector or matrix -\> an
  array of pairs

- `table` -\> `vector`, `matrix`, or `array` (depending on dimensions of
  table)

### Factor conversion

Factors are written as their level indices, i.e., the position of each
value in `levels(x)` rather than the value itself. The default levels
are the sorted unique values, e.g., `factor(c(10, 9, 8))` has levels
`8`, `9`, `10` and is written as `[3, 2, 1]`. An unused level shifts the
indices of the levels after it. With `variables`, which the fitting
methods of a model compiled from a Stan file always pass, a factor for a
variable not declared as `int` is an error. Without them
`write_stan_json()` has no declarations to check and always does the
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

### Tuples

A tuple is an unnamed list with one element per tuple element, so
`tuple(int, vector[2]) t` is `list(3, c(1.5, 2.5))`, and a nested tuple
is a nested list. An array of tuples is a list of such lists:
`array[2] tuple(real, real) ts` is `list(list(1, 2), list(3, 4))`. For
an array with more than one dimension give the list a `dim` attribute,
so `array[2, 3] tuple(real, real)` is `array(cells, dim = c(2, 3))` with
`cells` a list of the six tuples in the order
[`array()`](https://rdrr.io/r/base/array.html) fills them, the first
index changing fastest. Since an unnamed list is otherwise converted to
an array, a tuple is only written as one when `variables` declares it as
a tuple. The fitting methods pass the declarations for you.

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

This is only necessary when calling `write_stan_json()` directly without
`variables`. With them, and in the fitting methods of a model compiled
from a Stan file (e.g., `$sample()`), CmdStanR makes this correction
from the declarations.

## See also

[`$variables()`](https://mc-stan.org/cmdstanr/pr/1287/reference/model-method-variables.md)
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
#>     [0.710346515685883, 0.372003230867959],
#>     [-0.248198556052917, -0.609858300092441],
#>     [0.675270015431045, 1.0439388958914],
#>     [-0.595348959958748, -0.302706284526524],
#>     [0.110090411151627, 1.41728126064142]
#>   ],
#>   "y": [15, 14, 10, 9, 11],
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


# complex numbers are written as [re, im] pairs
data <- list(z = 1 + 2i, zv = c(1 + 2i, 3 + 4i))
write_stan_json(data, file)
cat(readLines(file), sep = "\n")
#> {
#>   "z": [1, 2],
#>   "zv": [
#>     [1, 2],
#>     [3, 4]
#>   ]
#> }


# tuples need the declarations from the Stan program, see 'variables'
# \dontrun{
stan_file <- write_stan_file("
data {
  tuple(int, vector[2]) t;
  array[2] tuple(real, real) ts;
}
")
data <- list(t = list(3, c(1.5, 2.5)), ts = list(list(1, 2), list(3, 4)))
write_stan_json(data, file, variables = variables_stan_file(stan_file)$data)
cat(readLines(file), sep = "\n")
#> {
#>   "t": {
#>     "1": 3,
#>     "2": [1.5, 2.5]
#>   },
#>   "ts": [
#>     {
#>       "1": 1,
#>       "2": 2
#>     },
#>     {
#>       "1": 3,
#>       "2": 4
#>     }
#>   ]
#> }
# }
```
