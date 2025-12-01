# Write Stan code to a temporary file

This function is deprecated. Please use
[`write_stan_file()`](https://mc-stan.org/cmdstanr/dev/reference/write_stan_file.md)
instead.

## Usage

``` r
write_stan_tempfile(code, dir = tempdir())
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
