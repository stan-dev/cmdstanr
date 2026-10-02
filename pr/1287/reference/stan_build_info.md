# What is known about how a CmdStan executable was built

When CmdStanR builds a model it writes a build record next to the
executable containing the options the build was asked for, the files it
read, the CmdStan installation it used, and what the executable reports
about itself. `stan_build_info()` reads that record back into R. When
there is no usable record it says why, and reports only the information
we can obtain by querying the executable itself (using CmdStan's
`<exe> info`).

The
[`$build_info()`](https://mc-stan.org/cmdstanr/pr/1287/reference/model-method-build_info.md)
method of a
[`CmdStanModel`](https://mc-stan.org/cmdstanr/pr/1287/reference/CmdStanModel.md)
object runs `stan_build_info()` internally for the model's executable.

## Usage

``` r
stan_build_info(exe_file)

# S3 method for class 'stan_build_info'
print(x, ...)
```

## Arguments

- exe_file:

  (string) Path to the executable.

- x:

  (`stan_build_info`) The object to print.

- ...:

  Not used.

## Value

A list of class `"stan_build_info"`. Two fields are always there:

- `record`: A list containing `status`, either `"available"` or
  `"unavailable"`, and `reason`, which is `NULL` when the record is
  available and otherwise one of `"missing"` (no record beside the
  executable), `"unreadable"` (a record that could not be read),
  `"executable_mismatch"` (the record describes a different executable,
  so the one at this path has changed since the record was written) or
  `"unsupported_format"` (written by a CmdStanR that stores records
  differently, in which case the result also has a `format_version`
  field).

- `reported_features`: A list containing what the executable reports
  about its own build. `stan_threads`, `stan_mpi`, `stan_opencl` and
  `stan_no_range_checks` are each `TRUE`, `FALSE`, or `NA` when the
  executable did not report the feature, and `stan_version` is the Stan
  version the executable reports being compiled with, or `NA` when it
  did not report one. These come from the record when it is available
  and otherwise from querying the executable.

When the build record is available, there are four more fields:

- `configuration`: A list of the options the model was created with.

  - `cpp_options`: the list of options in their make spelling, as
    `$cpp_options()` reports them. For example,
    `list(stan_threads = TRUE)` comes back as
    `list(STAN_THREADS = "TRUE")`.

  - `stanc_options`: the flags as given to stanc, in order. For example,
    `list(O1 = TRUE)` and `list("O1")` both come back as `list("--O1")`.

  - `stanc_options_from_make`: the flags `make/local` added to the stanc
    call through `STANCFLAGS`. A flag that `stanc_options` also sets is
    not repeated here since `stanc_options` takes precedence.

  - `include_paths`: a character vector of the directories searched for
    included files. When none were provided but the Stan program has
    includes this is set to the program's own directory.

- `dependencies`: A list describing the files the build read. Contains
  sublists `stan_file`, `included_files`, `user_header` and
  `make_local`. `user_header` and `make_local` are `NULL` when the build
  had none. `included_files` holds one entry per file. Each entry has
  two fields: `built_from`, the path the file had when the build ran,
  and `exists`, whether that path exists now. A path that no longer
  exists is not necessarily a problem. For example, an R package may
  build its models at install time in a temporary directory that is gone
  by the time the model is used.

- `cmdstan`: A list containing the `path` and `version` of the CmdStan
  installation that built the executable, and whether that path still
  `exists`. This version and `reported_features$stan_version` will
  typically agree except when using a release candidate
  (`cmdstan$version` will have a release-candidate suffix whereas
  `reported_features$stan_version` comes from the Stan library headers
  the executable was compiled against and will not).

- `untracked_dependencies`: A list of files the build depended on that
  CmdStanR cannot follow, so a change to them does not automatically
  trigger a rebuild. An empty list means nothing of the kind was found.
  Each file is reported as a sublist with two fields: `kind`, which is
  `"make_local_include"` when `make/local` includes another makefile or
  `"user_header_include"` when the user header includes other headers,
  and `detected_in`, the file the include was found in.

The result leaves out some of what the record holds: the file hashes the
rebuild check compares, the stanc flags CmdStanR added itself, the model
name given to stanc, and the TBB directory.

Absent items and empty items have different interpretations. A field
missing from the result means there was no usable record to read it
from. An empty list is a recorded empty value, such as no untracked
dependencies.

## See also

[`cmdstan_model()`](https://mc-stan.org/cmdstanr/pr/1287/reference/cmdstan_model.md),
[model-method-build_info](https://mc-stan.org/cmdstanr/pr/1287/reference/model-method-build_info.md)

## Examples

``` r
# \dontrun{
exe <- compile_stan_file(
  file.path(cmdstan_path(), "examples/bernoulli/bernoulli.stan"),
  cpp_options = list(stan_threads = TRUE),
  stanc_options = list("O1")
)
info <- stan_build_info(exe)
info
#> Build record: available
#> Reported features:
#>   stan_threads: TRUE
#>   stan_mpi: FALSE
#>   stan_opencl: FALSE
#>   stan_no_range_checks: FALSE
#>   stan_version: 2.40.0
#> Configuration:
#>   cpp_options: STAN_THREADS=TRUE
#>   stanc_options: --O1
#>   stanc_options_from_make: none
#>   include_paths: none
#> Dependencies:
#>   stan_file: /home/runner/.cmdstan/cmdstan-2.40.0/examples/bernoulli/bernoulli.stan
#>   make_local: /home/runner/.cmdstan/cmdstan-2.40.0/make/local
#> CmdStan 2.40.0 at /home/runner/.cmdstan/cmdstan-2.40.0
info$configuration
#> $cpp_options
#> $cpp_options$STAN_THREADS
#> [1] "TRUE"
#> 
#> 
#> $stanc_options
#> $stanc_options[[1]]
#> [1] "--O1"
#> 
#> 
#> $stanc_options_from_make
#> list()
#> 
#> $include_paths
#> character(0)
#> 
info$reported_features$stan_threads
#> [1] TRUE
info$dependencies$stan_file
#> $built_from
#> [1] "/home/runner/.cmdstan/cmdstan-2.40.0/examples/bernoulli/bernoulli.stan"
#> 
#> $exists
#> [1] TRUE
#> 
# }
```
