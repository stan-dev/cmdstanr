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

- `vector[J] v[K]` (or equivalently `array[K] vector[J] v ` as of Stan
  2.27) can be constructed in R as a list with `K` elements where each
  element a vector of length `J`

- `matrix[I,J] v[K]` (or equivalently `array[K] matrix[I,J] m ` as of
  Stan 2.27 ) can be constructed in R as a list with `K` elements where
  each element an `IxJ` matrix

These can also be passed in from R as arrays instead of lists but the
list option is provided for convenience. Unfortunately for arrays with
more than one dimension, e.g., `vector[J] v[K,L]` (or equivalently
`array[K,L] vector[J] v ` as of Stan 2.27) it is not possible to use an
R list and an array must be used instead. For this example the array in
R should have dimensions `KxLxJ`.

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
#>     [1.60440732826108, 2.11227728781614],
#>     [-1.51502452882071, -0.356124415722617],
#>     [-1.41602391449351, -1.06446420865157],
#>     [0.876777326556075, 1.07711653845397],
#>     [0.624132412621394, 1.18157556654405]
#>   ],
#>   "y": [10, 8, 11, 16, 15],
#>   "z": [1, 0]
#> }


# demonstrating list to array conversion
# suppose x is declared as `vector[3] x[2]` (or equivalently `array[2] vector[3] x`)
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
