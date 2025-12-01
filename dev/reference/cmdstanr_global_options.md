# CmdStanR global options

These options can be set via
[`options()`](https://rdrr.io/r/base/options.html) for an entire R
session.

## Details

- `cmdstanr_draws_format`: Which format provided by the posterior
  package should be used when returning the posterior or approximate
  posterior draws? The default depends on the model fitting method. See
  [draws](https://mc-stan.org/cmdstanr/dev/reference/fit-method-draws.md)
  for more details.

- `cmdstanr_force_recompile`: Should the default be to recompile models
  even if there were no Stan code changes since last compiled? See
  [compile](https://mc-stan.org/cmdstanr/dev/reference/model-method-compile.md)
  for more details. The default is `FALSE`.

- `cmdstanr_max_rows`: The maximum number of rows of output to print
  when using the
  [`$print()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-summary.md)
  method. The default is 10.

- `cmdstanr_print_line_numbers`: Should line numbers be included when
  printing a Stan program? The default is `FALSE`.

- `cmdstanr_no_ver_check`: Should the check for a more recent version of
  CmdStan be disabled? The default is `FALSE`.

- `cmdstanr_output_dir`: The directory where CmdStan should write its
  output CSV files when fitting models. The default is a temporary
  directory. Files in a temporary directory are removed as part of R
  garbage collection, while files in an explicitly defined directory are
  not automatically deleted.

- `cmdstanr_verbose`: Should more information be printed when compiling
  or running models, including showing how CmdStan was called
  internally? The default is `FALSE`.

- `cmdstanr_warn_inits`: Should a warning be thrown if initial values
  are only provided for a subset of parameters? The default is `TRUE`.

- `cmdstanr_write_stan_file_dir`: The directory where
  [`write_stan_file()`](https://mc-stan.org/cmdstanr/dev/reference/write_stan_file.md)
  should write Stan files. The default is a temporary directory. Files
  in a temporary directory are removed as part of R garbage collection,
  while files in an explicitly defined directory are not automatically
  deleted.

- `mc.cores`: The number of cores to use for various parallelization
  tasks (e.g. running MCMC chains, installing CmdStan). The default
  depends on the use case and is documented with the methods that make
  use of `mc.cores`.
