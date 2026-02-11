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

  (string) If `dir` is specified, \`basename“ is used for naming the
  output CSV files. If not specified, the file names are randomly
  generated.

## Value

Paths to CSV files (one per chain).

## Details

`draws_to_csv()` generates a CSV suitable for running standalone
generated quantities with CmdStan. The CSV file contains a single
comment `#num_samples`, which equals the number of iterations in the
supplied draws object.

The comment is followed by the column names. The first column is the
`lp__` value, followed by sampler diagnostics and finnaly other
variables of the draws object. \#' If the draws object does not contain
the `lp__` or sampler diagnostics variables, columns with zeros are
created in order to conform with the requirements of the standalone
generated quantities method of CmdStan.

The column names line is finally followed by the values of the draws in
the same order as the column names.

## Examples

``` r
# \dontrun{
draws <- posterior::example_draws()

draws_csv_files <- draws_to_csv(draws)
print(draws_csv_files)
#> [1] "/tmp/RtmpqiZtfq/fittedParams-202602110010-1-21b0d2.csv"
#> [2] "/tmp/RtmpqiZtfq/fittedParams-202602110010-2-21b0d2.csv"
#> [3] "/tmp/RtmpqiZtfq/fittedParams-202602110010-3-21b0d2.csv"
#> [4] "/tmp/RtmpqiZtfq/fittedParams-202602110010-4-21b0d2.csv"

# draws_csv_files <- draws_to_csv(draws,
#                                 sampler_diagnostic = sampler_diagnostics,
#                                 dir = "~/my_folder",
#                                 basename = "my-samples")
# }
```
