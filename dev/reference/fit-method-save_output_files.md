# Save output and data files

Fitted model objects returned directly by a
[`CmdStanModel`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanModel.md)
method have methods for saving (moving to a specified location) files
created by CmdStanR, including CmdStan output CSV files and input data
files. These methods move the files from their current location
(possibly the temporary directory) to a user-specified location. **The
paths stored in the fitted model object will also be updated to point to
the new file locations.**

The versions without the `save_` prefix (e.g., `$output_files()`) return
the current file paths without moving any files.

Objects created by
[`as_cmdstan_fit()`](https://mc-stan.org/cmdstanr/dev/reference/read_cmdstan_csv.md)
support `$output_files()` but not the other methods documented on this
page because the original CmdStan run is unavailable. See
**Reconstructed fitted model objects** in the
[`as_cmdstan_fit()`](https://mc-stan.org/cmdstanr/dev/reference/read_cmdstan_csv.md)
documentation for details.

## Usage

``` r
save_output_files(dir = ".", basename = NULL, timestamp = TRUE, random = TRUE)

save_latent_dynamics_files(
  dir = ".",
  basename = NULL,
  timestamp = TRUE,
  random = TRUE
)

save_profile_files(dir = ".", basename = NULL, timestamp = TRUE, random = TRUE)

save_data_file(dir = ".", basename = NULL, timestamp = TRUE, random = TRUE)

save_config_files(dir = ".", basename = NULL, timestamp = TRUE, random = TRUE)

save_metric_files(dir = ".", basename = NULL, timestamp = TRUE, random = TRUE)

output_files(include_failed = FALSE)

profile_files(include_failed = FALSE)

latent_dynamics_files(include_failed = FALSE)

data_file()

config_files(include_failed = FALSE)

metric_files(include_failed = FALSE)
```

## Arguments

- dir:

  (string) Path to directory where the files should be saved.

- basename:

  (string) Base filename to use. If `NULL` (the default), the model name
  is used. See **Details**.

- timestamp:

  (logical) Should a timestamp be added to the file name(s)? Defaults to
  `TRUE`. See **Details**.

- random:

  (logical) Should a six-character random hexadecimal suffix be added to
  the file name(s)? Defaults to `TRUE`. See **Details**.

- include_failed:

  (logical) Should CmdStan runs that failed also be included? The
  default is `FALSE`.

## Value

The `$save_*` methods print a message with the new file paths and
(invisibly) return a character vector of the new paths. If any file
cannot be copied then the method errors and no original files are
removed. The methods also have the side effect of setting the internal
paths in the fitted model object to the new paths.

The methods *without* the `save_` prefix return character vectors of
file paths without moving any files.

## Details

For `$save_output_files()` the files moved to `dir` will have names of
the form `basename-timestamp-id-random.csv`, where

- `basename` is the user's provided `basename` argument or, if `NULL`,
  the model name;

- `timestamp` is of the form `format(Sys.time(), "%Y%m%d%H%M")`;

- `id` is the MCMC chain id (or `1` for non MCMC);

- `random` contains six random hexadecimal characters.

`$save_latent_dynamics_files()` uses the pattern
`basename-diagnostic-timestamp-id-random.csv`. The
`$latent_dynamics_files()` and `$save_latent_dynamics_files()` methods
apply only to
[`CmdStanMCMC`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMCMC.md)
and
[`CmdStanVB`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanVB.md)
objects created with `save_latent_dynamics = TRUE`.

`$save_profile_files()` uses the pattern
`basename-profile-timestamp-id-random.csv`.

`$save_metric_files()` uses the pattern
`basename-metric-timestamp-id-random.json`. Make sure to set
`save_metric = TRUE` when fitting the model.

`$save_config_files()` uses the pattern
`basename-config-timestamp-id-random.json`. Make sure to set
`save_cmdstan_config = TRUE` when fitting the model.

`$save_data_file()` uses the pattern `basename-timestamp-random.ext`,
where `.ext` is the original data file extension. No `id` is included
because even with multiple MCMC chains the data file is the same.

## Examples

``` r
# \dontrun{
fit <- cmdstanr_example()
fit$output_files()
#> [1] "/tmp/Rtmp3nC1us/logistic-202607222113-1-749b85.csv"
#> [2] "/tmp/Rtmp3nC1us/logistic-202607222113-2-749b85.csv"
#> [3] "/tmp/Rtmp3nC1us/logistic-202607222113-3-749b85.csv"
#> [4] "/tmp/Rtmp3nC1us/logistic-202607222113-4-749b85.csv"
fit$data_file()
#> [1] "/home/runner/work/_temp/Library/cmdstanr/logistic.data.json"

# just using tempdir for the example
my_dir <- tempdir()
fit$save_output_files(dir = my_dir, basename = "banana")
#> Moved 4 files and set internal paths to new locations:
#> - /tmp/Rtmp3nC1us/banana-202607222113-1-54693c.csv
#> - /tmp/Rtmp3nC1us/banana-202607222113-2-54693c.csv
#> - /tmp/Rtmp3nC1us/banana-202607222113-3-54693c.csv
#> - /tmp/Rtmp3nC1us/banana-202607222113-4-54693c.csv
fit$save_output_files(dir = my_dir, basename = "tomato", timestamp = FALSE)
#> Moved 4 files and set internal paths to new locations:
#> - /tmp/Rtmp3nC1us/tomato-1-1f3e3b.csv
#> - /tmp/Rtmp3nC1us/tomato-2-1f3e3b.csv
#> - /tmp/Rtmp3nC1us/tomato-3-1f3e3b.csv
#> - /tmp/Rtmp3nC1us/tomato-4-1f3e3b.csv
fit$save_output_files(dir = my_dir, basename = "lettuce", timestamp = FALSE, random = FALSE)
#> Moved 4 files and set internal paths to new locations:
#> - /tmp/Rtmp3nC1us/lettuce-1.csv
#> - /tmp/Rtmp3nC1us/lettuce-2.csv
#> - /tmp/Rtmp3nC1us/lettuce-3.csv
#> - /tmp/Rtmp3nC1us/lettuce-4.csv
# }
```
