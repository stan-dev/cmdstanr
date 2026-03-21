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

## Details

`write_stan_json()` performs several conversions before writing the JSON
file:

- `logical` -\> `integer` (`TRUE` -\> `1`, `FALSE` -\> `0`)

- `data.frame` -\> `matrix` (via
  [`data.matrix()`](https://rdrr.io/r/base/data.matrix.html))

- `list` -\> `array`

- `table` -\> `vector`, `matrix`, or `array` (depending on dimensions of
  table)

The `list` to `array` conversion is intended to make it easier to
prepare the data for certain Stan declarations involving arrays:

- `array[K] vector[J] v ` can be constructed in R as a list with `K`
  elements where each element a vector of length `J`

- `array[K] matrix[I,J] m ` can be constructed in R as a list with `K`
  elements where each element an `IxJ` matrix

These can also be passed in from R as arrays instead of lists but the
list option is provided for convenience. Unfortunately for arrays with
more than one dimension (e.g. `array[K,L] vector[J] v `) it is not
possible to use an R list and an array must be used instead. For this
example the array in R should have dimensions `KxLxJ`.

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
#>     [-1.6865047424422, -1.43127077688999],
#>     [-0.90281494922007, 1.38291086057034],
#>     [1.31763369782387, 0.00312594041218188],
#>     [1.10018974454478, -0.0778868243875754],
#>     [1.20376783938691, 0.441428225928461]
#>   ],
#>   "y": [10, 7, 17, 11, 6],
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
