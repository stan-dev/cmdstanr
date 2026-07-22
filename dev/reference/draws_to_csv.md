# Write posterior draws objects to CSV files suitable for running standalone generated quantities with CmdStan.

Write posterior draws objects to CSV files suitable for running
standalone generated quantities with CmdStan.

## Usage

``` r
draws_to_csv(
  draws,
  sampler_diagnostics = NULL,
  dir = tempdir(),
  basename = "fittedParams"
)
```

## Arguments

- draws:

  A `posterior::draws_*` object.

- sampler_diagnostics:

  Either `NULL` or a `posterior::draws_*` object of sampler diagnostics.

- dir:

  (string) An optional path to the directory where the CSV files will be
  written. If not set, [temporary
  directory](https://rdrr.io/r/base/tempfile.html) is used.

- basename:

  (string) The base name for the output CSV files. The default is
  `"fittedParams"`. A timestamp, chain ID, and six-character random
  hexadecimal suffix are appended to the base name.

## Value

Paths to CSV files (one per chain).

## Details

`draws_to_csv()` generates a CSV suitable for running standalone
generated quantities with CmdStan. The CSV file contains a single
comment `# num_samples = <n>`, where `<n>` is the number of iterations
in the supplied draws object.

The comment is followed by the column names. The first column is the
`lp__` value, followed by sampler diagnostics and finally other
variables of the draws object. If the draws object does not contain the
`lp__` or sampler diagnostics variables, columns with zeros are created
in order to conform with the requirements of the standalone generated
quantities method of CmdStan.

The column names line is finally followed by the values of the draws in
the same order as the column names.

## See also

[`$generate_quantities()`](https://mc-stan.org/cmdstanr/dev/reference/model-method-generate-quantities.md)
for using the generated CSV files

## Examples

``` r
# \dontrun{
draws <- posterior::example_draws()

draws_csv_files <- draws_to_csv(draws)
print(draws_csv_files)
#> [1] "/tmp/RtmpR10Vum/fittedParams-202607220353-1-193848.csv"
#> [2] "/tmp/RtmpR10Vum/fittedParams-202607220353-2-193848.csv"
#> [3] "/tmp/RtmpR10Vum/fittedParams-202607220353-3-193848.csv"
#> [4] "/tmp/RtmpR10Vum/fittedParams-202607220353-4-193848.csv"

# draws_csv_files <- draws_to_csv(draws,
#                                 sampler_diagnostics = sampler_diagnostics,
#                                 dir = "~/my_folder",
#                                 basename = "my-samples")
# }
```
