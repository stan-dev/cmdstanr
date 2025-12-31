# Write Stan code to a file

Convenience function for writing Stan code to a (possibly
[temporary](https://rdrr.io/r/base/tempfile.html)) file with a `.stan`
extension. By default, the file name is chosen deterministically based
on a [hash](https://rlang.r-lib.org/reference/hash.html) of the Stan
code, and the file is not overwritten if it already has correct
contents. This means that calling this function multiple times with the
same Stan code will reuse the compiled model. This also however means
that the function is potentially not thread-safe. Using
`hash_salt = Sys.getpid()` should ensure thread-safety in the rare cases
when it is needed.

## Usage

``` r
write_stan_file(
  code,
  dir = getOption("cmdstanr_write_stan_file_dir", tempdir()),
  basename = NULL,
  force_overwrite = FALSE,
  hash_salt = ""
)
```

## Arguments

- code:

  (character vector) The Stan code to write to the file. This can be a
  character vector of length one (a string) containing the entire Stan
  program or a character vector with each element containing one line of
  the Stan program.

- dir:

  (string) An optional path to the directory where the file will be
  written. If omitted, a global option `cmdstanr_write_stan_file_dir` is
  used. If the global options is not set, [temporary
  directory](https://rdrr.io/r/base/tempfile.html) is used.

- basename:

  (string) If `dir` is specified, optionally the basename to use for the
  file created. If not specified a file name is generated from
  [hashing](https://rlang.r-lib.org/reference/hash.html) the code.

- force_overwrite:

  (logical) If set to `TRUE` the file will always be overwritten and
  thus the resulting model will always be recompiled.

- hash_salt:

  (string) Text to add to the model code prior to hashing to determine
  the file name if `basename` is not set.

## Value

The path to the file.

## Examples

``` r
# stan program as a single string
stan_program <- "
data {
  int<lower=0> N;
  array[N] int<lower=0,upper=1> y;
}
parameters {
  real<lower=0,upper=1> theta;
}
model {
  y ~ bernoulli(theta);
}
"

f <- write_stan_file(stan_program)
print(f)
#> [1] "/tmp/Rtmpaxzs2V/model_7f12fc190dd23b0e462f7d73040dd97e.stan"

lines <- readLines(f)
print(lines)
#>  [1] ""                                   "data {"                            
#>  [3] "  int<lower=0> N;"                  "  array[N] int<lower=0,upper=1> y;"
#>  [5] "}"                                  "parameters {"                      
#>  [7] "  real<lower=0,upper=1> theta;"     "}"                                 
#>  [9] "model {"                            "  y ~ bernoulli(theta);"           
#> [11] "}"                                  ""                                  
cat(lines, sep = "\n")
#> 
#> data {
#>   int<lower=0> N;
#>   array[N] int<lower=0,upper=1> y;
#> }
#> parameters {
#>   real<lower=0,upper=1> theta;
#> }
#> model {
#>   y ~ bernoulli(theta);
#> }
#> 

# stan program as character vector of lines
f2 <- write_stan_file(lines)
identical(readLines(f), readLines(f2))
#> [1] TRUE
```
