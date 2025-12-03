# Save output and data files

All fitted model objects have methods for saving (moving to a specified
location) the files created by CmdStanR to hold CmdStan output csv files
and input data files. These methods move the files from their current
location (possibly the temporary directory) to a user-specified
location. **The paths stored in the fitted model object will also be
updated to point to the new file locations.**

The versions without the `save_` prefix (e.g., `$output_files()`) return
the current file paths without moving any files.

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

  (string) Base filename to use. See **Details**.

- timestamp:

  (logical) Should a timestamp be added to the file name(s)? Defaults to
  `TRUE`. See **Details**.

- random:

  (logical) Should random alphanumeric characters be added to the end of
  the file name(s)? Defaults to `TRUE`. See **Details**.

- include_failed:

  (logical) Should CmdStan runs that failed also be included? The
  default is `FALSE.`

## Value

The `$save_*` methods print a message with the new file paths and
(invisibly) return a character vector of the new paths (or `NA` for any
that couldn't be copied). They also have the side effect of setting the
internal paths in the fitted model object to the new paths.

The methods *without* the `save_` prefix return character vectors of
file paths without moving any files.

## Details

For `$save_output_files()` the files moved to `dir` will have names of
the form `basename-timestamp-id-random`, where

- `basename` is the user's provided `basename` argument;

- `timestamp` is of the form `format(Sys.time(), "%Y%m%d%H%M")`;

- `id` is the MCMC chain id (or `1` for non MCMC);

- `random` contains six random alphanumeric characters;

For `$save_latent_dynamics_files()` everything is the same as for
`$save_output_files()` except `"-diagnostic-"` is included in the new
file name after `basename`.

For `$save_profile_files()` everything is the same as for
`$save_output_files()` except `"-profile-"` is included in the new file
name after `basename`.

For `$save_metric_files()` everything is the same as for
`$save_output_files()` except `"-metric-"` is included in the new file
name after `basename`.

For `$save_config_files()` everything is the same as for
`$save_output_files()` except `"-config-"` is included in the new file
name after `basename`.

For `$save_data_file()` no `id` is included in the file name because
even with multiple MCMC chains the data file is the same.

## See also

[`CmdStanMCMC`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMCMC.md),
[`CmdStanMLE`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMLE.md),
[`CmdStanVB`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanVB.md),
[`CmdStanGQ`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanGQ.md)

## Examples

``` r
# \dontrun{
fit <- cmdstanr_example()
fit$output_files()
#> [1] "/tmp/RtmpCX9wPP/logistic-202512031820-1-3099e9.csv"
#> [2] "/tmp/RtmpCX9wPP/logistic-202512031820-2-3099e9.csv"
#> [3] "/tmp/RtmpCX9wPP/logistic-202512031820-3-3099e9.csv"
#> [4] "/tmp/RtmpCX9wPP/logistic-202512031820-4-3099e9.csv"
fit$data_file()
#> [1] "/home/runner/work/_temp/Library/cmdstanr/logistic.data.json"

# just using tempdir for the example
my_dir <- tempdir()
fit$save_output_files(dir = my_dir, basename = "banana")
#> Moved 4 files and set internal paths to new locations:
#> - /tmp/RtmpCX9wPP/banana-202512031820-1-0f8944.csv
#> - /tmp/RtmpCX9wPP/banana-202512031820-2-0f8944.csv
#> - /tmp/RtmpCX9wPP/banana-202512031820-3-0f8944.csv
#> - /tmp/RtmpCX9wPP/banana-202512031820-4-0f8944.csv
fit$save_output_files(dir = my_dir, basename = "tomato", timestamp = FALSE)
#> Moved 4 files and set internal paths to new locations:
#> - /tmp/RtmpCX9wPP/tomato-1-7a320c.csv
#> - /tmp/RtmpCX9wPP/tomato-2-7a320c.csv
#> - /tmp/RtmpCX9wPP/tomato-3-7a320c.csv
#> - /tmp/RtmpCX9wPP/tomato-4-7a320c.csv
fit$save_output_files(dir = my_dir, basename = "lettuce", timestamp = FALSE, random = FALSE)
#> Moved 4 files and set internal paths to new locations:
#> - /tmp/RtmpCX9wPP/lettuce-1.csv
#> - /tmp/RtmpCX9wPP/lettuce-2.csv
#> - /tmp/RtmpCX9wPP/lettuce-3.csv
#> - /tmp/RtmpCX9wPP/lettuce-4.csv
# }
```
