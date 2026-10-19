# Changelog

## CmdStanR 1.0.0

This is a major release with enough noteworthy items that we’ve broken
the release notes into sections. The **Building models** section covers
the compilation redesign. In particular,
[`cmdstan_model()`](https://mc-stan.org/cmdstanr/rc/reference/cmdstan_model.md)
now compiles the model or reuses an up-to-date executable, before it
returns, and the separate `$compile()` method is gone (code that calls
`$compile()` or passes `compile = FALSE` will need to change). **Data
and initial values** covers improvements to how data and initial values
are handled, **Other new features** and **Bug fixes** cover what their
titles suggest, and **Removed and deprecated** lists everything else
that could break existing code beyond the compilation redesign.

Where a change can break existing code, the entry says what to use
instead, so you (or your coding agent) can update it.

A massive thank you to everyone who contributed code and reviews to this
release and to everyone who filed the bug reports and feature requests
that went into it.

### Building models

- [`cmdstan_model()`](https://mc-stan.org/cmdstanr/rc/reference/cmdstan_model.md)
  now compiles the model, or reuses an up-to-date executable, before it
  returns. The `compile` argument and the `$compile()` method are gone.
  To compile a Stan file without creating a model object, use the new
  [`compile_stan_file()`](https://mc-stan.org/cmdstanr/rc/reference/cmdstan_model.md),
  which returns the path to the executable. The new
  [`check_syntax_stan_file()`](https://mc-stan.org/cmdstanr/rc/reference/model-method-check_syntax.md),
  [`format_stan_file()`](https://mc-stan.org/cmdstanr/rc/reference/model-method-format.md)
  and
  [`variables_stan_file()`](https://mc-stan.org/cmdstanr/rc/reference/model-method-variables.md)
  also work without a model object.
  ([\#1256](https://github.com/stan-dev/cmdstanr/issues/1256))
- [`cmdstan_model()`](https://mc-stan.org/cmdstanr/rc/reference/cmdstan_model.md)
  now recompiles when the Stan program or its `#include`s, the user
  header, `cpp_options`, `stanc_options`, `make/local` or the CmdStan
  installation has changed, and in an interactive session (or with
  `quiet = FALSE`) it says which. For changes it can’t detect, such as a
  new C++ compiler, use `force_recompile = TRUE`. The vignette “How does
  CmdStanR work?” lists them.
  ([\#1255](https://github.com/stan-dev/cmdstanr/issues/1255),
  [\#1237](https://github.com/stan-dev/cmdstanr/issues/1237),
  [\#1019](https://github.com/stan-dev/cmdstanr/issues/1019),
  [\#1308](https://github.com/stan-dev/cmdstanr/issues/1308))
- Compiling now writes a hidden file next to the executable that records
  how it was built, for example `.bernoulli.cmdstanr.json` next to
  `bernoulli`. If your `.gitignore` ignores the executable, add
  `.*.cmdstanr.json` too. If the file is missing,
  [`cmdstan_model()`](https://mc-stan.org/cmdstanr/rc/reference/cmdstan_model.md)
  recompiles.
  ([\#1238](https://github.com/stan-dev/cmdstanr/issues/1238))
- The new
  [`stan_build_info()`](https://mc-stan.org/cmdstanr/rc/reference/stan_build_info.md)
  and `$build_info()` show how an executable was built: the options and
  files used, the CmdStan installation, and what the executable itself
  reports. ([\#1258](https://github.com/stan-dev/cmdstanr/issues/1258))
- If a build uses files CmdStanR can’t check for changes, such as a
  makefile included from `make/local` or a header included from the user
  header, compiling prints a note that changes to them need
  `force_recompile = TRUE`.
  ([\#1257](https://github.com/stan-dev/cmdstanr/issues/1257))
- `pedantic = TRUE` now shows the pedantic warnings even when the
  executable is up to date and isn’t rebuilt.
  ([\#1258](https://github.com/stan-dev/cmdstanr/issues/1258))
- Running a model whose executable is out of date, for example because
  the Stan file was edited after
  [`cmdstan_model()`](https://mc-stan.org/cmdstanr/rc/reference/cmdstan_model.md),
  is now an error. Previously the old executable ran. This means the
  Stan file has to exist whenever the model runs. To run an executable
  without its Stan file, create the model with
  `cmdstan_model(exe_file = )`.
  ([\#1255](https://github.com/stan-dev/cmdstanr/issues/1255))
- The new `$is_current()` method returns `FALSE` when the executable is
  out of date, so you can check before running a model, such as one
  stored in a saved fit.
  ([\#1258](https://github.com/stan-dev/cmdstanr/issues/1258))
- A failed compilation now leaves the previous executable in place and
  usable. Previously the model could end up with no executable, or with
  one that didn’t match its model methods.
  ([\#1235](https://github.com/stan-dev/cmdstanr/issues/1235))
- The `compile_standalone` and `compile_model_methods` arguments are
  gone. Use `$expose_functions()` and `fit$init_model_methods()`
  instead. ([\#1256](https://github.com/stan-dev/cmdstanr/issues/1256),
  [\#1245](https://github.com/stan-dev/cmdstanr/issues/1245))
- `cmdstan_model(exe_file = )` without a `stan_file` now gives an error
  if you also pass `cpp_options`, `stanc_options`, `include_paths`,
  `user_header`, `force_recompile` or `pedantic`, since there is nothing
  to compile. The `cmdstanr_force_recompile` option is ignored for these
  models. ([\#1258](https://github.com/stan-dev/cmdstanr/issues/1258))
- [`cmdstan_model()`](https://mc-stan.org/cmdstanr/rc/reference/cmdstan_model.md)
  no longer accepts both `stan_file` and `exe_file`, and `$exe_file()`
  no longer accepts a path. The executable is named after the Stan file,
  and the `dir` argument of
  [`cmdstan_model()`](https://mc-stan.org/cmdstanr/rc/reference/cmdstan_model.md)
  sets where it goes.
  ([\#1253](https://github.com/stan-dev/cmdstanr/issues/1253),
  [\#1258](https://github.com/stan-dev/cmdstanr/issues/1258))
- `$cmdstan_version()` now reports the version of CmdStan that compiled
  the executable, not the version at
  [`cmdstan_path()`](https://mc-stan.org/cmdstanr/rc/reference/set_cmdstan_path.md).
  ([\#1249](https://github.com/stan-dev/cmdstanr/issues/1249))
- `$cpp_options()` now returns the options the model was created with,
  using make variable names and values, so `list(stan_threads = TRUE)`
  comes back as `list(STAN_THREADS = "true")`. Use
  [`stan_build_info()`](https://mc-stan.org/cmdstanr/rc/reference/stan_build_info.md)
  to see what the executable reports about its own build.
  ([\#1019](https://github.com/stan-dev/cmdstanr/issues/1019),
  [\#1258](https://github.com/stan-dev/cmdstanr/issues/1258))
- Every `cpp_options` entry must now be named, as in
  `list(STAN_THREADS = TRUE)`. For `+=` and the other makefile
  operators, use
  [`cmdstan_make_local()`](https://mc-stan.org/cmdstanr/rc/reference/install_cmdstan.md).
  ([\#1250](https://github.com/stan-dev/cmdstanr/issues/1250))
- Setting an option to `FALSE` in `cpp_options`, as in
  `list(stan_threads = FALSE)`, now turns it off, even if `make/local`
  turns it on. Previously `FALSE` turned the option on.
  ([\#1251](https://github.com/stan-dev/cmdstanr/issues/1251))
- A model compiled with threading now runs when `threads_per_chain`
  isn’t set, and asking for more than one thread from a model compiled
  without threading is now an error.
  ([\#1258](https://github.com/stan-dev/cmdstanr/issues/1258))
- Each build setting now goes in one argument, and putting it somewhere
  else gives an error that names the right one.
  ([\#1258](https://github.com/stan-dev/cmdstanr/issues/1258))
  - Include paths go in `include_paths`, not in `stanc_options`,
    `cpp_options` or the `STANCFLAGS` in `make/local`.
  - The user header goes in `user_header`, not in `cpp_options`. The new
    `$user_header()` method returns its path, and `$cpp_options()` no
    longer includes it.
  - Other stanc flags go in `stanc_options`, not in `cpp_options` as
    `STANCFLAGS`.
  - `stanc_options` no longer accepts flags that have their own
    argument: `include-paths` (use `include_paths`), `warn-pedantic`
    (`pedantic`), `allow-undefined` (`user_header`), `use-opencl`
    (`cpp_options = list(stan_opencl = TRUE)`) and `name` (set from the
    file name).
- A flag in `stanc_options` now overrides the same flag in the
  `STANCFLAGS` of `make/local`.
  ([\#1258](https://github.com/stan-dev/cmdstanr/issues/1258))
- `stanc_options` entries with values, such as
  `list(canonicalize = "deprecations")` or
  `list("max-line-length" = 78)`, now work, including values that
  contain spaces or quotes.
  ([\#1227](https://github.com/stan-dev/cmdstanr/issues/1227),
  [\#1233](https://github.com/stan-dev/cmdstanr/issues/1233),
  [\#1263](https://github.com/stan-dev/cmdstanr/issues/1263))
- A stanc error now stops compilation right away and shows the stanc
  message. ([\#1227](https://github.com/stan-dev/cmdstanr/issues/1227))
- Relative include paths are now resolved when the model is created, so
  changing the working directory afterwards no longer breaks `#include`.
  `$include_paths()` returns the absolute paths, and the error for an
  include path that doesn’t exist shows its absolute path.
  ([\#1227](https://github.com/stan-dev/cmdstanr/issues/1227),
  [\#1229](https://github.com/stan-dev/cmdstanr/issues/1229))
- `#include` directories with spaces in their paths now work.
  ([\#820](https://github.com/stan-dev/cmdstanr/issues/820),
  [\#1230](https://github.com/stan-dev/cmdstanr/issues/1230))
- Error messages from a running model now name the model’s Stan file
  instead of a temporary copy of it.
  ([\#1258](https://github.com/stan-dev/cmdstanr/issues/1258))
- `$code()` and `$variables()` now always describe the program the model
  was created from. After `$format(overwrite_file = TRUE)`, call
  [`cmdstan_model()`](https://mc-stan.org/cmdstanr/rc/reference/cmdstan_model.md)
  again to pick up the rewritten file.
  ([\#1258](https://github.com/stan-dev/cmdstanr/issues/1258))
- `$expose_functions()` and `fit$init_model_methods()` now work on a
  model or fit loaded with
  [`readRDS()`](https://rdrr.io/r/base/readRDS.html). Previously calling
  one of the functions or methods failed with a null symbol address
  error. ([\#1157](https://github.com/stan-dev/cmdstanr/issues/1157),
  [\#1158](https://github.com/stan-dev/cmdstanr/issues/1158))
- Compiling, checking syntax and reading a program’s variables no longer
  leave copies of the program, the generated C++ or the executable in
  R’s temporary directory.
  ([\#1258](https://github.com/stan-dev/cmdstanr/issues/1258))

### Data and initial values

- Lists of matrices or vectors, and data frames, can now be passed for
  `int` variables, not just `real` ones.
  ([\#817](https://github.com/stan-dev/cmdstanr/issues/817))
- A data frame column that isn’t numeric, integer, logical or factor is
  now an error. Previously a character column was silently converted to
  integer codes in alphabetical order. If that’s what you want, convert
  the column yourself, e.g. with `as.integer(factor(x))`.
  ([\#1225](https://github.com/stan-dev/cmdstanr/issues/1225))
- Lists of logical vectors or matrices are now converted to integers,
  like other logical data, instead of giving an error.
  ([\#1225](https://github.com/stan-dev/cmdstanr/issues/1225))
- Supplying a factor for a variable not declared as `int` is now an
  error. ([\#1225](https://github.com/stan-dev/cmdstanr/issues/1225))
- Factors are now accepted for length-1 `int` arrays
  (e.g. `array[1] int x`).
  ([\#1225](https://github.com/stan-dev/cmdstanr/issues/1225))
- Initial values taken from a Pathfinder fit now use the right weights.
  Draws CmdStan already resampled with PSIS are no longer reweighted,
  distinct draws with equal log weights are no longer merged, and a draw
  that was resampled several times keeps that weight.
  ([\#1206](https://github.com/stan-dev/cmdstanr/issues/1206),
  [\#1207](https://github.com/stan-dev/cmdstanr/issues/1207))
- `$pathfinder()` now gives each path its own initial values when `init`
  supplies one set per path. Previously every path used the first set.
  ([\#1206](https://github.com/stan-dev/cmdstanr/issues/1206))
- Initial values taken from a fit now work for a model created with only
  `exe_file`. Previously this failed with “‘init’ contains empty lists”.
  ([\#1171](https://github.com/stan-dev/cmdstanr/issues/1171))
- A function passed as `init` is no longer called an extra time to check
  its output.
  ([\#1195](https://github.com/stan-dev/cmdstanr/issues/1195))
- Tuples and complex numbers can now be passed as data and initial
  values. Pass a tuple as an unnamed list of its elements and a complex
  number as an R complex value. `init = fit` now includes tuple
  parameters.
  [`write_stan_json()`](https://mc-stan.org/cmdstanr/rc/reference/write_stan_json.md)
  gains a `variables` argument, which it needs to tell a tuple from a
  list of arrays.
  ([\#925](https://github.com/stan-dev/cmdstanr/issues/925))

### Other new features

- `$sample()` can now show a progress bar (using the progressr package)
  with `show_progress_bar = TRUE` or
  `options(cmdstanr_progress_bar = TRUE)`. The option also turns it on
  when another package such as brms calls CmdStanR for you. The feature
  is still experimental and may change in the future.
  ([\#1138](https://github.com/stan-dev/cmdstanr/issues/1138))
- Reading MCMC and generated quantities draws from CmdStan CSV files now
  uses less memory.
  ([\#1041](https://github.com/stan-dev/cmdstanr/issues/1041),
  [\#1307](https://github.com/stan-dev/cmdstanr/issues/1307))
- [`as_cmdstan_fit()`](https://mc-stan.org/cmdstanr/rc/reference/read_cmdstan_csv.md)
  now reads draws from the CSV files when they’re first requested (like
  fits from `$sample()` do), so looking at a few variables of a large
  fit takes much less memory.
  ([\#1294](https://github.com/stan-dev/cmdstanr/issues/1294))
- [`read_cmdstan_csv()`](https://mc-stan.org/cmdstanr/rc/reference/read_cmdstan_csv.md)
  and
  [`as_cmdstan_fit()`](https://mc-stan.org/cmdstanr/rc/reference/read_cmdstan_csv.md)
  can now read gzip-compressed CmdStan CSV files ending in `.csv.gz` and
  bzip2-compressed files ending in `.csv.bz2`. CmdStanR now requires
  data.table 1.18.0 or later.
  ([\#1027](https://github.com/stan-dev/cmdstanr/issues/1027),
  [\#1217](https://github.com/stan-dev/cmdstanr/issues/1217))
- The fitting methods, `$save_output_files()` and
  `$save_latent_dynamics_files()` gain a `compress` argument for writing
  gzip- or bzip2-compressed CSV files. The `cmdstanr_compress` option
  sets the default for the fitting methods.
  ([\#1276](https://github.com/stan-dev/cmdstanr/issues/1276),
  [\#1311](https://github.com/stan-dev/cmdstanr/issues/1311))
- The new
  [`print_stan_file()`](https://mc-stan.org/cmdstanr/rc/reference/print_stan_file.md)
  prints a Stan file, with syntax highlighting when used in a Quarto or
  R Markdown document.
  ([\#1166](https://github.com/stan-dev/cmdstanr/issues/1166))
- The new `$cmdstan_defaults()` method returns CmdStan’s default
  argument values for a method (`"sample"` by default), using CmdStanR’s
  argument names.
  ([\#1167](https://github.com/stan-dev/cmdstanr/issues/1167))
- The new `$materialize()` method reads a fit’s draws, sampler
  diagnostics, initial values and profiles from the CSV files into R in
  one call. ([\#1181](https://github.com/stan-dev/cmdstanr/issues/1181))
- The new `$command()` method returns the command line CmdStanR ran for
  each CmdStan run.
  ([\#900](https://github.com/stan-dev/cmdstanr/issues/900))
- `$save_object()` gains `format = "qs2"`, which uses the qs2 package
  and is faster and uses less memory than
  [`saveRDS()`](https://rdrr.io/r/base/readRDS.html).
  ([\#1125](https://github.com/stan-dev/cmdstanr/issues/1125))
- [`as_cmdstan_fit()`](https://mc-stan.org/cmdstanr/rc/reference/read_cmdstan_csv.md)
  gains a `variables` argument to read only some of the variables from
  the CSV files.
  ([\#1121](https://github.com/stan-dev/cmdstanr/issues/1121))
- `$generate_quantities()` gains the `show_messages` and
  `show_exceptions` arguments that the other methods have.
  ([\#1142](https://github.com/stan-dev/cmdstanr/issues/1142))
- `$loo()` now defaults to `r_eff = FALSE`, skipping the relative
  efficiency computation, which can be very slow. Set `r_eff = TRUE` for
  the previous behavior.
  ([\#1091](https://github.com/stan-dev/cmdstanr/issues/1091))
- `$log_prob()`, `$grad_log_prob()`, and other model methods are now
  faster after initialization.
  ([\#1274](https://github.com/stan-dev/cmdstanr/issues/1274))
- `$init_model_methods()` takes less time to set up, and later fits of
  the same model reuse the compiled methods instead of compiling them
  again. ([\#1309](https://github.com/stan-dev/cmdstanr/issues/1309))
- `fit$init_model_methods()` and `$expose_functions()` gain a `quiet`
  argument that suppresses the messages printed while the methods or
  functions compile.
  ([\#914](https://github.com/stan-dev/cmdstanr/issues/914))
- [`install_cmdstan()`](https://mc-stan.org/cmdstanr/rc/reference/install_cmdstan.md)
  can now copy the `make/local` flags of the current installation into
  the new one, so the new CmdStan is built the same way. Set
  `copy_make_local = TRUE` or `FALSE`, or leave it unset and an
  interactive session shows the old `make/local` and asks.
  ([\#1267](https://github.com/stan-dev/cmdstanr/issues/1267))
- [`cmdstan_make_local()`](https://mc-stan.org/cmdstanr/rc/reference/install_cmdstan.md)
  no longer adds a flag that’s already in `make/local`.
  ([\#1266](https://github.com/stan-dev/cmdstanr/issues/1266))
- [`install_cmdstan()`](https://mc-stan.org/cmdstanr/rc/reference/install_cmdstan.md)
  now retries a download without the token if GitHub rejects the one in
  `GITHUB_PAT`, and warns that the token may be invalid.
  ([\#909](https://github.com/stan-dev/cmdstanr/issues/909))
- The `timeout` argument of
  [`install_cmdstan()`](https://mc-stan.org/cmdstanr/rc/reference/install_cmdstan.md)
  now also applies to the download of CmdStan. Previously the download
  used R’s `timeout` option (60 seconds by default), which could be too
  short on a slow connection.
- Chain IDs in generated file names are now zero-padded to at least two
  digits, for example `01` instead of `1`.
  ([\#1244](https://github.com/stan-dev/cmdstanr/issues/1244))
- On Windows,
  [`check_cmdstan_toolchain()`](https://mc-stan.org/cmdstanr/rc/reference/install_cmdstan.md)
  now finds Rtools through `R_TOOLS_SOFT` and falls back to `PATH`, so
  it works with more R distributions and with future Rtools releases.
  ([\#1211](https://github.com/stan-dev/cmdstanr/issues/1211))
- `$expose_functions()` now gives an error naming the conflicting names
  when a function or argument name is a reserved word in C++.
  ([\#1154](https://github.com/stan-dev/cmdstanr/issues/1154))
- The `$expose_functions()` documentation now explains how to keep
  compiled functions and model methods across R sessions with the
  `rcpp.cache.dir` option.
  ([\#870](https://github.com/stan-dev/cmdstanr/issues/870))
- The new `cmdstanr_save_metric` and `cmdstanr_save_config` options set
  the defaults of the `save_metric` and `save_cmdstan_config` arguments.
  ([\#1159](https://github.com/stan-dev/cmdstanr/issues/1159))
- Set the new `cmdstanr_spinner` option to `FALSE` to turn off the
  spinner shown while compiling, checking syntax or installing CmdStan.
  The spinner is also no longer shown when knitting.
  ([\#486](https://github.com/stan-dev/cmdstanr/issues/486))
- `$generate_quantities()` now also accepts `CmdStanMLE`,
  `CmdStanLaplace`, and `CmdStanPathfinder` objects as `fitted_params`.
  ([\#1203](https://github.com/stan-dev/cmdstanr/issues/1203))
- `$generate_quantities()` now reports per-process execution times with
  CmdStan 2.39 or newer, and
  [`read_cmdstan_csv()`](https://mc-stan.org/cmdstanr/rc/reference/read_cmdstan_csv.md)
  returns these times from standalone generated quantities CSV files.
  ([\#1168](https://github.com/stan-dev/cmdstanr/issues/1168))

### Bug fixes

- `$expose_functions()` can now expose a function that returns an array
  of tuples.
  ([\#1329](https://github.com/stan-dev/cmdstanr/issues/1329))
- If a fit’s CSV files were in a temporary directory that no longer
  exists (for example a cached Quarto chunk, or a fit saved with
  [`saveRDS()`](https://rdrr.io/r/base/readRDS.html) and loaded in a new
  session), the error now says so and how to avoid it.
  ([\#1012](https://github.com/stan-dev/cmdstanr/issues/1012))
- [`check_cmdstan_toolchain()`](https://mc-stan.org/cmdstanr/rc/reference/install_cmdstan.md)
  now waits longer for WSL to respond and says so when it doesn’t,
  instead of reporting that no WSL distribution is installed.
  ([\#1297](https://github.com/stan-dev/cmdstanr/issues/1297))
- `$pathfinder(save_single_paths = TRUE)` now works. Previously the
  argument was ignored.
- `$save_metric_files()` now gives a clear error when no metric files
  were written, and saved metric files are no longer deleted when the
  fit is garbage-collected.
  ([\#1021](https://github.com/stan-dev/cmdstanr/issues/1021))
- [`cmdstan_model()`](https://mc-stan.org/cmdstanr/rc/reference/cmdstan_model.md)
  no longer fails when `MAKEFLAGS` turns on directory printing.
  ([\#1163](https://github.com/stan-dev/cmdstanr/issues/1163))
- Quoted values in the `STANCFLAGS` of `make/local` now reach stanc as
  one argument instead of being split on spaces.
  ([\#1232](https://github.com/stan-dev/cmdstanr/issues/1232))
- `$laplace()` with `output_basename` and no `mode` no longer writes
  over the CSV file from the optimization it runs first. That file is
  now named `<output_basename>-mode-01.csv`.
- `$laplace()` now passes `show_messages` and `show_exceptions` to the
  optimization it runs when `mode` isn’t supplied.
- [`read_cmdstan_csv()`](https://mc-stan.org/cmdstanr/rc/reference/read_cmdstan_csv.md)
  now expands `~` in file paths.
  ([\#1098](https://github.com/stan-dev/cmdstanr/issues/1098))
- Loading CmdStanR no longer causes
  [`parallel::mclapply()`](https://rdrr.io/r/parallel/mclapply.html) to
  leave zombie processes behind.
  ([\#1105](https://github.com/stan-dev/cmdstanr/issues/1105))
- Data with a `NULL` element now gives an error naming the variable.
  ([\#1129](https://github.com/stan-dev/cmdstanr/issues/1129))
- `$metadata()` now reports the number of chains in `num_chains`.
  Previously it was always 1.
  ([\#1187](https://github.com/stan-dev/cmdstanr/issues/1187))
- `$lp_approx()` and `$mle()` now return numeric vectors whatever the
  `cmdstanr_draws_format` option is set to.
  ([\#1190](https://github.com/stan-dev/cmdstanr/issues/1190))
- Stan files whose names contain several spaces or quotes now compile.
  ([\#1200](https://github.com/stan-dev/cmdstanr/issues/1200))
- `$draws()` on a Pathfinder fit now orders the diagnostic columns the
  same way as the other methods.
  ([\#1205](https://github.com/stan-dev/cmdstanr/issues/1205))
- An executable that can’t be run, for example one that lost its execute
  permission or was built for another platform, now gives an error that
  names the executable and says how to rebuild it.
  ([\#1246](https://github.com/stan-dev/cmdstanr/issues/1246))
- A model with no parameters no longer ends every `$sample()` call with
  a warning about a NaN E-BFMI.
- [`install_cmdstan()`](https://mc-stan.org/cmdstanr/rc/reference/install_cmdstan.md)
  now always downloads with R’s libcurl method, so the retry without a
  rejected `GITHUB_PAT` also works when
  `options(download.file.method = "curl")` is set.
- On WSL, a failed copy of the executable or the output CSV files now
  gives an error right away.
- On Windows, a model now runs with the TBB from the CmdStan
  installation that built it, even after
  [`set_cmdstan_path()`](https://mc-stan.org/cmdstanr/rc/reference/set_cmdstan_path.md)
  switches to another installation.
  ([\#1261](https://github.com/stan-dev/cmdstanr/issues/1261))
- Fits with tuple or complex variables now work fully. `$metadata()`
  reports their sizes, a tuple’s name selects all of its columns in
  `$draws()` and the other methods with a `variables` argument, and the
  model methods accept and return them.
  ([\#925](https://github.com/stan-dev/cmdstanr/issues/925))
- With CmdStan in WSL, R can now read the output, diagnostic, profile,
  config and metric files when `output_dir` is set, and CmdStan can find
  data and init files when `TMPDIR` points at the `//wsl$` share.
  ([\#1110](https://github.com/stan-dev/cmdstanr/issues/1110),
  [\#1113](https://github.com/stan-dev/cmdstanr/issues/1113))
- CmdStan processes are now stopped when the R process that started them
  dies, for example a future worker interrupted from the parent session.
  Previously they kept running.
  ([\#1086](https://github.com/stan-dev/cmdstanr/issues/1086))
- `$init_model_methods()` and `$expose_functions()` now work in the same
  R session as rstan or brms, whichever loads first, when RcppParallel
  6.2.0 or later is installed.
  ([\#1177](https://github.com/stan-dev/cmdstanr/issues/1177),
  [\#1270](https://github.com/stan-dev/cmdstanr/issues/1270))
- A function exposed with `$expose_functions()` that returns a tuple no
  longer occasionally crashes R or returns a list with wrong elements.
  ([\#1001](https://github.com/stan-dev/cmdstanr/issues/1001))
- `$expose_functions(global = TRUE)` now also puts the functions in the
  `$functions` field of the model or fit, so a later
  `$expose_functions()` call works.
  ([\#1317](https://github.com/stan-dev/cmdstanr/issues/1317))
- `cmdstan_make_local(cpp_options = list(stan_threads = FALSE))`, and
  the same argument of
  [`install_cmdstan()`](https://mc-stan.org/cmdstanr/rc/reference/install_cmdstan.md),
  now turn threading off. Previously they turned it on.
  ([\#605](https://github.com/stan-dev/cmdstanr/issues/605))

### Removed and deprecated

- CmdStanR now requires R 4.0.0 or later.
  ([\#1144](https://github.com/stan-dev/cmdstanr/issues/1144))
- CmdStan versions older than 2.37.0 are no longer supported. To use an
  older CmdStan, install an older CmdStanR release from GitHub.
  ([\#1144](https://github.com/stan-dev/cmdstanr/issues/1144))
- The `CMDSTANR_NO_VER_CHECK` option and environment variable are
  deprecated. Use the lowercase `cmdstanr_no_ver_check` instead.
- The `num_threads` argument of `$pathfinder()` is deprecated. Use
  `threads` instead.
- The `save_latent_dynamics` argument is now only available in
  `$sample()`, `$sample_mpi()` and `$variational()`, the methods for
  which CmdStan writes these files.
- `fit$variable_skeleton()` is removed. `fit$constrain_variables()` now
  returns the values already structured, so
  [`utils::relist()`](https://rdrr.io/r/utils/relist.html) and the
  skeleton are no longer needed.
  ([\#925](https://github.com/stan-dev/cmdstanr/issues/925))
- The `CMDSTANR_USE_MSYS_TOOLCHAIN` environment variable is deprecated
  and ignored (with a warning).
  ([\#1144](https://github.com/stan-dev/cmdstanr/issues/1144))
- Removed deprecated items (replacements in parentheses).
  ([\#1061](https://github.com/stan-dev/cmdstanr/issues/1061))
  - `read_sample_csv()`
    ([`read_cmdstan_csv()`](https://mc-stan.org/cmdstanr/rc/reference/read_cmdstan_csv.md))
  - `write_stan_tempfile()`
    ([`write_stan_file()`](https://mc-stan.org/cmdstanr/rc/reference/write_stan_file.md))
  - `jacobian_adjustment` argument to `fit$log_prob()` and similar
    methods (`jacobian` argument)
  - `output_samples` argument to `model$variational()` (`draws`
    argument)
  - `hessian` argument to `fit$init_model_methods()` (the Hessian method
    is now always compiled)
  - `threads` (`cpp_options = list(stan_threads = TRUE)`) and
    `compile_hessian_method` (always compiled), formerly arguments to
    `$compile()`
  - several arguments to `model$sample()`:
    - `cores` and `num_cores` (`parallel_chains`)
    - `num_chains` (`chains`)
    - `num_warmup` (`iter_warmup`)
    - `num_samples` (`iter_sampling`)
    - `validate_csv` (`diagnostics`)
    - `save_extra_diagnostics` (`save_latent_dynamics`)
    - `max_depth` (`max_treedepth`)
    - `stepsize` (`step_size`)

## CmdStanR 0.9.0

### General Improvements/Changes

- Added compatibility for RTools45
  ([\#1066](https://github.com/stan-dev/cmdstanr/issues/1066))
- CmdStanR will now use RTools with no additional toolchain updates
  needed on Windows (CmdStan 2.35+ only;
  [\#1065](https://github.com/stan-dev/cmdstanr/issues/1065),
  [\#1054](https://github.com/stan-dev/cmdstanr/issues/1054))
- Improve error messages when calling
  [`sampler_diagnostics()`](https://mc-stan.org/cmdstanr/rc/reference/fit-method-sampler_diagnostics.md)
  with `fixed_param=TRUE`
- Improve numerical stability in calculation of effective sample size
  during `loo` method
  ([\#1057](https://github.com/stan-dev/cmdstanr/issues/1057))
- Improve numerical stablity with very small log-ratios in calculation
  of effective sample size during `loo` method
  ([\#1015](https://github.com/stan-dev/cmdstanr/issues/1015))
- Add warning if input data/inits have been coerced to ints
  ([\#994](https://github.com/stan-dev/cmdstanr/issues/994))

### Bugfixes

- Don’t require fixed_param for models with zero parameters (only GQs)
  for CmdStan \>= 2.36
  ([\#1046](https://github.com/stan-dev/cmdstanr/issues/1046))
- Improve detection/handling of `make`
  ([\#1036](https://github.com/stan-dev/cmdstanr/issues/1036))
- Fix saving of model objects to network drive
  ([\#1038](https://github.com/stan-dev/cmdstanr/issues/1038), thanks to
  [@bschneidr](https://github.com/bschneidr))
- Update usage of `untar` to fix installation errors
  ([\#1034](https://github.com/stan-dev/cmdstanr/issues/1034))
- Respect compilation flags in `make/local` when exposing functions or
  model methods
  ([\#1003](https://github.com/stan-dev/cmdstanr/issues/1003))
- Fix passing of include paths to CmdStan
  ([\#1000](https://github.com/stan-dev/cmdstanr/issues/1000))
- Fix passing of factor data to CmdStan
  ([\#999](https://github.com/stan-dev/cmdstanr/issues/999))
- Fix extraction and passing of array data/parameters as model inits
  ([\#993](https://github.com/stan-dev/cmdstanr/issues/993))

### Documentation Updates

- Clarifications to usage of `optimize` and `loo` methods
  ([\#1060](https://github.com/stan-dev/cmdstanr/issues/1060))
- Add documentation for faster model saving with large models
  ([\#1042](https://github.com/stan-dev/cmdstanr/issues/1042))
- Remove mentions of `rstan::read_stan_csv` due to incompatibility with
  newer CmdStan outputs
  ([\#1018](https://github.com/stan-dev/cmdstanr/issues/1018))
- Document global option `cmdstanr_print_line_numbers` for printing line
  numbers ([\#1017](https://github.com/stan-dev/cmdstanr/issues/1017))
- Change usage of ‘chapter’ to ‘section’ in documentation
  ([\#1014](https://github.com/stan-dev/cmdstanr/issues/1014))
- Remove examples of updating removed array syntax as functionality no
  longer supported in CmdStan
  ([\#1008](https://github.com/stan-dev/cmdstanr/issues/1008))
- Change usages of ‘sampling statement’ -\> ‘distribution statement’
  ([\#987](https://github.com/stan-dev/cmdstanr/issues/987))

## CmdStanR 0.8.1

### Minor changes

- Added `CMDSTANR_USE_RTOOLS` environment variable to force stock RTools
  on Windows by [@andrjohns](https://github.com/andrjohns) in
  [\#980](https://github.com/stan-dev/cmdstanr/issues/980)
- Added support for Windows ARM64 by
  [@andrjohns](https://github.com/andrjohns) in
  [\#990](https://github.com/stan-dev/cmdstanr/issues/990)
- Automatically initialise model methods when called, add `inc_warmup`
  argument to `$unconstrain_draws()` by
  [@andrjohns](https://github.com/andrjohns) in
  [\#985](https://github.com/stan-dev/cmdstanr/issues/985)

### Bugfixes

- Fix errors when using pathfinder object as initial values by
  [@avehtari](https://github.com/avehtari) in
  [\#984](https://github.com/stan-dev/cmdstanr/issues/984)
- Fix error with `$unconstrain_draws()` returning incorrect assumptions
  in some cases by [@andrjohns](https://github.com/andrjohns) in
  [\#983](https://github.com/stan-dev/cmdstanr/issues/983)
- Fix spurious errors about missing CmdStan config files by
  [@andrjohns](https://github.com/andrjohns) in
  [\#981](https://github.com/stan-dev/cmdstanr/issues/981)
- Fix linking error when exposing SUNDIALS/KINSOL functions or model
  methods by [@andrjohns](https://github.com/andrjohns) in
  [\#977](https://github.com/stan-dev/cmdstanr/issues/977)
- Fix long-standing error with OneDrive paths on Windows by
  [@andrjohns](https://github.com/andrjohns) in
  [\#990](https://github.com/stan-dev/cmdstanr/issues/990)

## CmdStanR 0.8.0

### Major new features

- Add functionality for passing `CmdStanFit` objects as initial values
  by [@SteveBronder](https://github.com/SteveBronder) in
  [\#937](https://github.com/stan-dev/cmdstanr/issues/937)

### Other improvements

- Add compatibility with CmdStan 2.35 by
  [@andrjohns](https://github.com/andrjohns) in
  [\#972](https://github.com/stan-dev/cmdstanr/issues/972)
- Add `show_messages` and `show_exceptions` arguments to all methods for
  controlling output by [@andrjohns](https://github.com/andrjohns) in
  [\#897](https://github.com/stan-dev/cmdstanr/issues/897)
- Drop RcppEigen dependency, implement basic Eigen -\> C++ interop by
  [@andrjohns](https://github.com/andrjohns) in
  [\#899](https://github.com/stan-dev/cmdstanr/issues/899)
- Add compatibility with CmdStan 2.34 by
  [@andrjohns](https://github.com/andrjohns) in
  [\#905](https://github.com/stan-dev/cmdstanr/issues/905)
  [\#910](https://github.com/stan-dev/cmdstanr/issues/910)
- Add a format argument to the
  [`unconstrain_draws()`](https://mc-stan.org/cmdstanr/rc/reference/fit-method-unconstrain_draws.md)
  method to specify draws format of return by
  [@andrjohns](https://github.com/andrjohns) in
  [\#886](https://github.com/stan-dev/cmdstanr/issues/886)
- Align `cmdstanr` EBFMI diagnostic threshold with CmdStan by
  [@andrjohns](https://github.com/andrjohns) in
  [\#892](https://github.com/stan-dev/cmdstanr/issues/892)
- Add global option `cmdstanr_print_line_numbers` to add line number to
  model printing by [@sbfnk](https://github.com/sbfnk) in
  [\#967](https://github.com/stan-dev/cmdstanr/issues/967)
- Add new CmdStan arguments `save_metric` and `save_cmdstan_config` by
  [@venpopov](https://github.com/venpopov) in
  [\#932](https://github.com/stan-dev/cmdstanr/issues/932)
- Add documentation for CmdStanR global options by
  [@jgabry](https://github.com/jgabry) in
  [\#951](https://github.com/stan-dev/cmdstanr/issues/951)
- Add documentation for how to obtain structured output similar to
  `rstan::extract()` using a combination of `cmdstanr` and `posterior`
  by [@jgabry](https://github.com/jgabry) in
  [\#955](https://github.com/stan-dev/cmdstanr/issues/955)
- Added coercion generics for CmdStanFit objects by
  [@gowerc](https://github.com/gowerc) in
  [\#943](https://github.com/stan-dev/cmdstanr/issues/943)
- `psis_resample` and `calculate_lp` arguments added to Pathfinder
  method by [@SteveBronder](https://github.com/SteveBronder) in
  [\#903](https://github.com/stan-dev/cmdstanr/issues/903)
- Documentation and tests for LOO method updated by
  [@jgabry](https://github.com/jgabry) in
  [\#923](https://github.com/stan-dev/cmdstanr/issues/923)
- Global option `cmdstanr_warn_inits` added to disable warnings about
  partially specified initial values by
  [@jgabry](https://github.com/jgabry) in
  [\#913](https://github.com/stan-dev/cmdstanr/issues/913)
- Updates to MCMC `output_dir` documentation by
  [@jgabry](https://github.com/jgabry) in
  [\#929](https://github.com/stan-dev/cmdstanr/issues/929)

### Bugfixes

- Fix broken link in OpenCL documentation by
  [@eipi10](https://github.com/eipi10) in
  [\#908](https://github.com/stan-dev/cmdstanr/issues/908)
- Fix a minor typo in the README by [@jgabry](https://github.com/jgabry)
  in [\#911](https://github.com/stan-dev/cmdstanr/issues/911)
- Make exported RNG functions respect changes to R’s seed by
  [@andrjohns](https://github.com/andrjohns) in
  [\#973](https://github.com/stan-dev/cmdstanr/issues/973)
- Optimisations for model methods functions by
  [@andrjohns](https://github.com/andrjohns) in
  [\#960](https://github.com/stan-dev/cmdstanr/issues/960)
- Bugfix for passing function for initial values with Pathfinder method
  and default `num_paths` by [@andrjohns](https://github.com/andrjohns)
  in [\#964](https://github.com/stan-dev/cmdstanr/issues/964)
- Continue with compilation if `compile_stanalone=TRUE` but no functions
  are found by [@jgabry](https://github.com/jgabry) in
  [\#956](https://github.com/stan-dev/cmdstanr/issues/956)
- Update tests and CI for compatibility with MacOS ARM64 by
  [@andrjohns](https://github.com/andrjohns) in
  [\#958](https://github.com/stan-dev/cmdstanr/issues/958)
- Fix handling of `inv_metric` argument with only 1 parameter by
  [@venpopov](https://github.com/venpopov) in
  [\#935](https://github.com/stan-dev/cmdstanr/issues/935)
- Fixes for compatibility with RTools44 by
  [@andrjohns](https://github.com/andrjohns) in
  [\#952](https://github.com/stan-dev/cmdstanr/issues/952)
  [\#959](https://github.com/stan-dev/cmdstanr/issues/959)

## CmdStanR 0.7.0

### Major new features

- New `laplace` method by [@jgabry](https://github.com/jgabry) in
  [\#800](https://github.com/stan-dev/cmdstanr/issues/800)
- New `pathfinder` method by
  [@SteveBronder](https://github.com/SteveBronder) in
  [\#848](https://github.com/stan-dev/cmdstanr/issues/848)

### Other improvements and bug fixes

- Add missing link to diagnose method in CmdStanModel doc by
  [@jgabry](https://github.com/jgabry) in
  [\#833](https://github.com/stan-dev/cmdstanr/issues/833)
- Improvements to compile tests by
  [@martinmodrak](https://github.com/martinmodrak) in
  [\#836](https://github.com/stan-dev/cmdstanr/issues/836)
- Changed the delay behavior in wsl_installed by
  [@martinmodrak](https://github.com/martinmodrak) in
  [\#839](https://github.com/stan-dev/cmdstanr/issues/839)
- Update array syntax in website vignette by
  [@andrjohns](https://github.com/andrjohns) in
  [\#841](https://github.com/stan-dev/cmdstanr/issues/841)
- Compatibility fixes for cmdstan 2.33+ by
  [@jgabry](https://github.com/jgabry) in
  [\#843](https://github.com/stan-dev/cmdstanr/issues/843)
- Suggest format method after error due to old syntax by
  [@jgabry](https://github.com/jgabry) in
  [\#852](https://github.com/stan-dev/cmdstanr/issues/852)
- Clarifications in R-markdown vignette by
  [@jgcolman](https://github.com/jgcolman) in
  [\#854](https://github.com/stan-dev/cmdstanr/issues/854)
- Update linux/wsl detection for install arch by
  [@andrjohns](https://github.com/andrjohns) in
  [\#856](https://github.com/stan-dev/cmdstanr/issues/856)
- Fix handling of single-length inits for containers by
  [@andrjohns](https://github.com/andrjohns) in
  [\#857](https://github.com/stan-dev/cmdstanr/issues/857)
- Add support/tests for exposing functions with tuples by
  [@andrjohns](https://github.com/andrjohns) in
  [\#860](https://github.com/stan-dev/cmdstanr/issues/860)
- Add support/tests for exporting functions with complex types by
  [@andrjohns](https://github.com/andrjohns) in
  [\#861](https://github.com/stan-dev/cmdstanr/issues/861)
- Add option for installing from release archive by
  [@andrjohns](https://github.com/andrjohns) in
  [\#866](https://github.com/stan-dev/cmdstanr/issues/866)
- Improve Pathfinder doc by [@avehtari](https://github.com/avehtari) in
  [\#875](https://github.com/stan-dev/cmdstanr/issues/875)
- Rename `jacobian_adjustment` argument to `jacobian` by
  [@jgabry](https://github.com/jgabry) in
  [\#879](https://github.com/stan-dev/cmdstanr/issues/879)
- Fix get_cmdstan_flags(‘STANCFLAGS’) in recursive make by
  [@pearsonca](https://github.com/pearsonca) in
  [\#881](https://github.com/stan-dev/cmdstanr/issues/881)

## CmdStanR 0.6.1

- Store return codes instead of always querying exit status by
  [@jgabry](https://github.com/jgabry) in
  [\#798](https://github.com/stan-dev/cmdstanr/issues/798)
- enable jacobian argument for optimization by
  [@jgabry](https://github.com/jgabry) in
  [\#799](https://github.com/stan-dev/cmdstanr/issues/799)
- Fix init_model_methods for models with no data by
  [@andrjohns](https://github.com/andrjohns) in
  [\#801](https://github.com/stan-dev/cmdstanr/issues/801)
- Document a CmdStan-focused way to pre-compile Stan models in R
  packages by [@wlandau](https://github.com/wlandau) in
  [\#809](https://github.com/stan-dev/cmdstanr/issues/809)
- Describe how to efficiently save model fit objects by
  [@wlandau](https://github.com/wlandau) in
  [\#816](https://github.com/stan-dev/cmdstanr/issues/816)
- fix errors in doc for new methods by
  [@jgabry](https://github.com/jgabry) in
  [\#823](https://github.com/stan-dev/cmdstanr/issues/823)
- Give informative error when exposing stan functions with precompiled
  model by [@andrjohns](https://github.com/andrjohns) in
  [\#831](https://github.com/stan-dev/cmdstanr/issues/831)
- Bugfixes in .stanfunctions, hessian model method, and exposing RNG
  functions by [@andrjohns](https://github.com/andrjohns) in
  [\#811](https://github.com/stan-dev/cmdstanr/issues/811)
- Fix variable_skeleton() with containers by
  [@andrjohns](https://github.com/andrjohns) in
  [\#832](https://github.com/stan-dev/cmdstanr/issues/832)
- Improve handling of user header by
  [@martinmodrak](https://github.com/martinmodrak) in
  [\#818](https://github.com/stan-dev/cmdstanr/issues/818)
- change duplicate stdout_file to stderr_file by
  [@jgabry](https://github.com/jgabry) in
  [\#834](https://github.com/stan-dev/cmdstanr/issues/834)

## CmdStanR 0.6.0

#### Major new features

- New
  [`expose_functions()`](https://mc-stan.org/cmdstanr/rc/reference/model-method-expose_functions.md)
  method to expose Stan functions to R by
  [@andrjohns](https://github.com/andrjohns) in
  [\#702](https://github.com/stan-dev/cmdstanr/issues/702). See
  [`?expose_functions`](https://mc-stan.org/cmdstanr/rc/reference/model-method-expose_functions.md).
- New methods for accessing log_prob, grad_log_prob, hessian,
  un/constrain variables by [@andrjohns](https://github.com/andrjohns)
  in [\#701](https://github.com/stan-dev/cmdstanr/issues/701). See
  [`?init_model_methods`](https://mc-stan.org/cmdstanr/rc/reference/fit-method-init_model_methods.md).

#### Other changes

- mod\$variables works w includes in precompile state (fix
  [\#680](https://github.com/stan-dev/cmdstanr/issues/680)) by
  [@MKyhos](https://github.com/MKyhos) in
  [\#682](https://github.com/stan-dev/cmdstanr/issues/682)
- Update broken link for Stan OpenCL support page by
  [@erictleung](https://github.com/erictleung) in
  [\#686](https://github.com/stan-dev/cmdstanr/issues/686)
- Add newline to check syntax output by
  [@rok-cesnovar](https://github.com/rok-cesnovar) in
  [\#689](https://github.com/stan-dev/cmdstanr/issues/689)
- Allow exposing functions without sampling by
  [@andrjohns](https://github.com/andrjohns) in
  [\#705](https://github.com/stan-dev/cmdstanr/issues/705)
- Expose skeleton by [@andrjohns](https://github.com/andrjohns) in
  [\#706](https://github.com/stan-dev/cmdstanr/issues/706)
- WSL - Run cmdstan and models under WSL filesystem by
  [@andrjohns](https://github.com/andrjohns) in
  [\#696](https://github.com/stan-dev/cmdstanr/issues/696)
- Bugfix - Deep copy method/function environments by
  [@andrjohns](https://github.com/andrjohns) in
  [\#709](https://github.com/stan-dev/cmdstanr/issues/709)
- Add option for including jacobian adjustments in hessian method by
  [@andrjohns](https://github.com/andrjohns) in
  [\#710](https://github.com/stan-dev/cmdstanr/issues/710)
- WSL Optimisations and Bugfixes for CI by
  [@andrjohns](https://github.com/andrjohns) in
  [\#711](https://github.com/stan-dev/cmdstanr/issues/711)
- add stancflags from make/local by
  [@rok-cesnovar](https://github.com/rok-cesnovar) in
  [\#690](https://github.com/stan-dev/cmdstanr/issues/690)
- Update co-authors by [@andrjohns](https://github.com/andrjohns) in
  [\#715](https://github.com/stan-dev/cmdstanr/issues/715)
- Update model methods parameter naming and extract skeleton function by
  [@andrjohns](https://github.com/andrjohns) in
  [\#724](https://github.com/stan-dev/cmdstanr/issues/724)
- Add method for unconstraining all parameter draws by
  [@andrjohns](https://github.com/andrjohns) in
  [\#729](https://github.com/stan-dev/cmdstanr/issues/729)
- Improve efficiency of variable matching by
  [@sbfnk](https://github.com/sbfnk) in
  [\#736](https://github.com/stan-dev/cmdstanr/issues/736)
- Add verbosity to download output and errors by
  [@andrjohns](https://github.com/andrjohns) in
  [\#745](https://github.com/stan-dev/cmdstanr/issues/745)
- Update handling of show_messages, add show_exceptions by
  [@andrjohns](https://github.com/andrjohns) in
  [\#746](https://github.com/stan-dev/cmdstanr/issues/746)
- Rtools43 support by [@andrjohns](https://github.com/andrjohns) in
  [\#755](https://github.com/stan-dev/cmdstanr/issues/755)
- Add stanc M1 make patch, suppress boost warnings by
  [@andrjohns](https://github.com/andrjohns) in
  [\#756](https://github.com/stan-dev/cmdstanr/issues/756)
- more examples of summary method by
  [@gravesti](https://github.com/gravesti) in
  [\#751](https://github.com/stan-dev/cmdstanr/issues/751)
- Fix model\\format and model\\check_syntax for compiled models with
  include-paths by [@adrian-lison](https://github.com/adrian-lison) in
  [\#775](https://github.com/stan-dev/cmdstanr/issues/775)
- Generalise RTools config/support by
  [@andrjohns](https://github.com/andrjohns) in
  [\#777](https://github.com/stan-dev/cmdstanr/issues/777)
- New posterior vignette by [@gravesti](https://github.com/gravesti) in
  [\#719](https://github.com/stan-dev/cmdstanr/issues/719)
- Add moment-matching support to \$loo() method by
  [@andrjohns](https://github.com/andrjohns) in
  [\#778](https://github.com/stan-dev/cmdstanr/issues/778)
- replace  with function by [@jsocolar](https://github.com/jsocolar) in
  [\#789](https://github.com/stan-dev/cmdstanr/issues/789)

## CmdStanR 0.5.3

#### New features

- On Windows, users can now install and use CmdStan with WSL (Windows
  Subsystem for Linux). Set `wsl=TRUE` in
  [`install_cmdstan()`](https://mc-stan.org/cmdstanr/rc/reference/install_cmdstan.md)
  to install CmdStan for use with WSL. This can offer significant
  speedups compared to native Windows execution.
  ([\#677](https://github.com/stan-dev/cmdstanr/issues/677),
  [@andrjohns](https://github.com/andrjohns))

#### Bug fixes

- In
  [`cmdstan_default_path()`](https://mc-stan.org/cmdstanr/rc/reference/cmdstan_default_path.md)
  we now ignore directories inside `.cmdstan` that don’t start with
  `"cmdstan-"`.
  ([\#651](https://github.com/stan-dev/cmdstanr/issues/651))

- Fixed Windows issue related to not locating `grep.exe` or when it is
  located in a path with spaces.
  ([@weshinsley](https://github.com/weshinsley),
  [\#661](https://github.com/stan-dev/cmdstanr/issues/661),
  [\#663](https://github.com/stan-dev/cmdstanr/issues/663))

- Fixed a bug with diagnostic checks when ebfmi is NaN.

- Fixed a bug that caused issues when using `~` or `.` in paths supplied
  to the `cmdstanr_write_stan_file_dir` global option.

- Fixed a bug that caused the
  [`time()`](https://mc-stan.org/cmdstanr/rc/reference/fit-method-time.md)
  method fail when some of the chains failed to finish succesfully.

## CmdStanR 0.5.2

- Refactored toolchain installation and checks for R 4.x on Windows and
  added support for Rtools42.
  ([\#645](https://github.com/stan-dev/cmdstanr/issues/645))

- Expanded the use of `CMDSTAN` environment variable to point to CmdStan
  installation *or* directory containing CmdStan installations.
  ([\#643](https://github.com/stan-dev/cmdstanr/issues/643))

- New vignette on how to handle deprecations using the `$format()`
  method. ([\#644](https://github.com/stan-dev/cmdstanr/issues/644))

## CmdStanR 0.5.1

- Temporarily disable `format="draws_rvars"` in the `$draws()` method
  due to a bug. Until this is fixed users can make use of
  [`posterior::as_draws_rvars()`](https://mc-stan.org/posterior/reference/draws_rvars.html)
  to convert draws from CmdStanR to the `draws_rvars` format.
  ([\#640](https://github.com/stan-dev/cmdstanr/issues/640))

## CmdStanR 0.5.0

#### Bug fixes

- Fixed bug that caused stdour/stderr not being read at the end of
  optimization.
  ([\#522](https://github.com/stan-dev/cmdstanr/issues/522))

- Fixed issue with handling `NA` as the reported external process
  status. ([\#544](https://github.com/stan-dev/cmdstanr/issues/544),
  [@myshkin](https://github.com/myshkin))

- Fixed issue with handling models with no parameters and CmdStan 2.27+.

#### New features

- Default directory changed to `.cmdstan` instead of `.cmdstanr` so that
  CmdStanPy and CmdStanR can use the same CmdStan installations. Using
  `.cmdstanr` will continue to be supported until version 1.0 but
  [`install_cmdstan()`](https://mc-stan.org/cmdstanr/rc/reference/install_cmdstan.md)
  will now default to `.cmdstan` and CmdStanR will first look for
  `.cmdstan` before falling back on `.cmdstanr`.
  ([\#454](https://github.com/stan-dev/cmdstanr/issues/454))

- New method
  [`diagnose()`](https://mc-stan.org/cmdstanr/rc/reference/model-method-diagnose.md)
  for CmdstanModel objects exposes CmdStan’s `diagnose` method for
  comparing Stan’s gradient computations to gradients computed via
  finite differences.
  ([\#485](https://github.com/stan-dev/cmdstanr/issues/485))

- New method `$variables()` for CmdstanModel objects that returns a list
  of variables in the Stan model, their types and number of dimensions.
  Does not require the model to be compiled.
  ([\#519](https://github.com/stan-dev/cmdstanr/issues/519))

- New method `$format()` for auto-formatting and canonicalizing the Stan
  models. ([\#625](https://github.com/stan-dev/cmdstanr/issues/625))

- Added the option to create `CmdStanModel` from the executable only
  with the `exe_file` argument.
  ([\#564](https://github.com/stan-dev/cmdstanr/issues/564))

- Added a convenience argument `user_header` to `$compile()` and
  [`cmdstan_model()`](https://mc-stan.org/cmdstanr/rc/reference/cmdstan_model.md)
  that simplifies the use of an external .hpp file to compile with the
  model.

- Added the `cmdstanr_force_recompile` global option that is used for
  forcing recompilation of Stan models.
  ([\#580](https://github.com/stan-dev/cmdstanr/issues/580))

- New method `$code()` for all fitted model objects that returns the
  Stan code associated with the fitted model.
  ([\#575](https://github.com/stan-dev/cmdstanr/issues/575))

- New method `$diagnostic_summary()` for CmdStanMCMC objects that
  summarizes the sampler diagnostics (divergences, treedepth, ebfmi) and
  can regenerate the related warning messages.
  ([\#205](https://github.com/stan-dev/cmdstanr/issues/205))

- New `diagnostics` argument for the `$sample()` method to specify which
  diagnostics are checked after sampling. Replaces `validate_csv`
  argument. ([\#205](https://github.com/stan-dev/cmdstanr/issues/205))

- Added E-BFMI checks that run automatically post sampling.
  ([\#500](https://github.com/stan-dev/cmdstanr/issues/500),
  [@jsocolar](https://github.com/jsocolar))

- New methods for
  [`posterior::as_draws()`](https://mc-stan.org/posterior/reference/draws.html)
  for CmdStanR fitted model objects. These are just wrappers around the
  `$draws()` method provided for convenience.
  ([\#532](https://github.com/stan-dev/cmdstanr/issues/532))

- [`write_stan_file()`](https://mc-stan.org/cmdstanr/rc/reference/write_stan_file.md)
  now choose file names deterministically based on the code so that
  models do not get unnecessarily recompiled when calling the function
  multiple times with the same code.
  ([\#495](https://github.com/stan-dev/cmdstanr/issues/495),
  [@martinmodrak](https://github.com/martinmodrak))

- The `dir` argument for
  [`write_stan_file()`](https://mc-stan.org/cmdstanr/rc/reference/write_stan_file.md)
  can now be set with a global option.
  ([\#537](https://github.com/stan-dev/cmdstanr/issues/537))

- [`write_stan_json()`](https://mc-stan.org/cmdstanr/rc/reference/write_stan_json.md)
  now handles data of class `"table"`. Tables are converted to vector,
  matrix, or array depending on the dimensions of the table.
  ([\#528](https://github.com/stan-dev/cmdstanr/issues/528))

- Improved processing of named lists supplied to the `data` argument to
  JSON data files: checking whether the list includes all required
  elements/Stan variables; improved differentiating arrays/vectors of
  length 1 and scalars when generating JSON data files; generating
  floating point numbers with decimal points to fix issue with parsing
  large numbers.
  ([\#538](https://github.com/stan-dev/cmdstanr/issues/538))

- [`install_cmdstan()`](https://mc-stan.org/cmdstanr/rc/reference/install_cmdstan.md)
  now automatically installs the Linux ARM CmdStan when Linux
  distributions running on ARM CPUs are detected.
  ([\#531](https://github.com/stan-dev/cmdstanr/issues/531))

- New function
  [`as_mcmc.list()`](https://mc-stan.org/cmdstanr/rc/reference/as_mcmc.list.md)
  for converting CmdStanMCMC objects to mcmc.list objects from the coda
  package. ([\#584](https://github.com/stan-dev/cmdstanr/issues/584),
  [@MatsuuraKentaro](https://github.com/MatsuuraKentaro))

## CmdStanR 0.4.0

#### Bug fixes

- Fixed issue with retrieving draws with models with spaces in their
  names. ([\#453](https://github.com/stan-dev/cmdstanr/issues/453))

- Fixed bug with spaces in path to the temporary folder on Windows.
  ([\#460](https://github.com/stan-dev/cmdstanr/issues/460))

- Fixed issue with not reporting model executable name clashing with
  folder name.
  ([\#461](https://github.com/stan-dev/cmdstanr/issues/461))

#### New features

- New function
  [`as_cmdstan_fit()`](https://mc-stan.org/cmdstanr/rc/reference/read_cmdstan_csv.md)
  that creates CmdStanMCMC/MLE/VB objects directly from CmdStan CSV
  files. ([\#412](https://github.com/stan-dev/cmdstanr/issues/412))

- [`read_cmdstan_csv()`](https://mc-stan.org/cmdstanr/rc/reference/read_cmdstan_csv.md)
  now also returns chain run times for MCMC sampling CSV files.
  ([\#414](https://github.com/stan-dev/cmdstanr/issues/414))

- Faster CSV reading for multiple chains.
  ([\#419](https://github.com/stan-dev/cmdstanr/issues/419))

- New `$profiles()` method for fitted model objects accesses profiling
  information from R if profiling used in the Stan program. Support for
  profiling Stan programs requires CmdStan \>= 2.26.
  ([\#434](https://github.com/stan-dev/cmdstanr/issues/434))

- New vignette on profiling Stan programs.
  ([\#435](https://github.com/stan-dev/cmdstanr/issues/435))

- New vignette on running Stan on the GPU with OpenCL. OpenCL device ids
  can now also be specified at runtime.
  ([\#439](https://github.com/stan-dev/cmdstanr/issues/439))

- New check for invalid parameter names when supplying init values.
  ([\#452](https://github.com/stan-dev/cmdstanr/issues/452),
  [@mike-lawrence](https://github.com/mike-lawrence))

- Suppressing compilation messages when not in interactive mode.
  ([\#462](https://github.com/stan-dev/cmdstanr/issues/462),
  [@wlandau](https://github.com/wlandau))

- New `error_on_NA` argument for
  [`cmdstan_version()`](https://mc-stan.org/cmdstanr/rc/reference/set_cmdstan_path.md)
  to optionally return `NULL` (instead of erroring) if the CmdStan path
  is not found
  ([\#467](https://github.com/stan-dev/cmdstanr/issues/467),
  [@wlandau](https://github.com/wlandau)).

- Global option `cmdstanr_max_rows` can be set as an alternative to
  specifying `max_rows` argument to the `$print()` method.
  ([\#470](https://github.com/stan-dev/cmdstanr/issues/470))

- New `output_basename` argument for the model fitting methods. Can be
  used in conjunction with `output_dir` to get completely predictable
  output CSV file paths.
  ([\#471](https://github.com/stan-dev/cmdstanr/issues/471))

- New `format` argument for `$draws()`, `$sampler_diagnostics()`,
  [`read_cmdstan_csv()`](https://mc-stan.org/cmdstanr/rc/reference/read_cmdstan_csv.md),
  and `as_cmdstan_fit`(). This controls the format of the draws returned
  or stored in the object. Changing the format can improve speed and
  memory usage for large models.
  ([\#482](https://github.com/stan-dev/cmdstanr/issues/482))

## CmdStanR 0.3.0

#### Bug fixes

- Fixed reading inverse mass matrix with values written in scientific
  format in the CSV.
  ([\#394](https://github.com/stan-dev/cmdstanr/issues/394))

- Fixed error caused by an empty data list. Previously if a model didn’t
  require data then `data` had to either be NULL or be a non-empty list,
  but now [`list()`](https://rdrr.io/r/base/list.html) is allowed.
  ([\#403](https://github.com/stan-dev/cmdstanr/issues/403))

#### New features

- Added `$sample_mpi()` for MCMC sampling with MPI.
  ([\#350](https://github.com/stan-dev/cmdstanr/issues/350))

- Added informative messages on compile errors caused by precompiled
  headers (PCH).
  ([\#384](https://github.com/stan-dev/cmdstanr/issues/384))

- Added the `cmdstanr_verbose` option for verbose mode. Intended for
  troubleshooting, debugging and development. See end of *How does
  CmdStanR work?* vignette for details.
  ([\#392](https://github.com/stan-dev/cmdstanr/issues/392))

- New `$loo()` method for CmdStanMCMC objects. Requires computing
  pointwise log-likelihood in Stan program.
  ([\#366](https://github.com/stan-dev/cmdstanr/issues/366))

- The `fitted_params` argument to the `$generate_quantities()` method
  now also accepts CmdStanVB,
  [`posterior::draws_array`](https://mc-stan.org/posterior/reference/draws_array.html),
  and
  [`posterior::draws_matrix`](https://mc-stan.org/posterior/reference/draws_matrix.html)
  objects. ([\#390](https://github.com/stan-dev/cmdstanr/issues/390))

- The `$optimize()` method now supports all of CmdStan’s
  tolerance-related arguments for (L)BFGS.
  ([\#398](https://github.com/stan-dev/cmdstanr/issues/398))

- The documentation for the R6 methods now uses `@param`, which allows
  package developers to import the CmdStanR documentation using
  roxygen2’s `@inheritParams`.
  ([\#408](https://github.com/stan-dev/cmdstanr/issues/408))

## CmdStanR 0.2.2

#### Bug fixes

- Fixed bug with reading Stan CSV when grep used coloring by default
  ([\#364](https://github.com/stan-dev/cmdstanr/issues/364),#371)

- Depend on posterior v0.1.3 to avoid a potential error in `$summary()`.
  ([\#383](https://github.com/stan-dev/cmdstanr/issues/383))

#### New features

- Added support for native execution on the macOS with the M1 ARM-based
  CPU. ([\#375](https://github.com/stan-dev/cmdstanr/issues/375))

- Added threading support via `threads` argument for `$optimize()` and
  `$variational()` (was already available via `threads_per_chain` for
  `$sample()`).
  ([\#369](https://github.com/stan-dev/cmdstanr/issues/369))

## CmdStanR 0.2.1

#### Bug fixes

- Fixed bug with processing stanc_options in
  [`check_syntax()`](https://mc-stan.org/cmdstanr/rc/reference/model-method-check_syntax.md).
  ([\#345](https://github.com/stan-dev/cmdstanr/issues/345))

- Fixed bug on access to one variable via
  [`draws()`](https://mc-stan.org/cmdstanr/rc/reference/fit-method-draws.md).
  ([\#348](https://github.com/stan-dev/cmdstanr/issues/348))

#### New features

- `compile()` and
  [`check_syntax()`](https://mc-stan.org/cmdstanr/rc/reference/model-method-check_syntax.md)
  methods gain argument `pedantic` for turning on pedantic mode, which
  warns about issues with the model beyond syntax errors.
  ([\#361](https://github.com/stan-dev/cmdstanr/issues/361))

## CmdStanR 0.2.0

#### Bug fixes

- Fix potential indexing error if using
  [`read_cmdstan_csv()`](https://mc-stan.org/cmdstanr/rc/reference/read_cmdstan_csv.md)
  with CSV files created by CmdStan without CmdStanR.
  ([\#291](https://github.com/stan-dev/cmdstanr/issues/291),
  [\#292](https://github.com/stan-dev/cmdstanr/issues/292),
  [@johnlees](https://github.com/johnlees))

- Fix error when returning draws or sampler diagnostics for a fit with
  only warmup and no samples.
  ([\#288](https://github.com/stan-dev/cmdstanr/issues/288),
  [\#293](https://github.com/stan-dev/cmdstanr/issues/293))

- Fix trailing slashes issue for `dir` in
  [`cmdstan_model()`](https://mc-stan.org/cmdstanr/rc/reference/cmdstan_model.md)
  and `output_dir` in fitting methods.
  ([\#281](https://github.com/stan-dev/cmdstanr/issues/281),
  [\#294](https://github.com/stan-dev/cmdstanr/issues/294))

- Fix dimensions error when processing a list of matrices passed in as
  `data`. ([\#296](https://github.com/stan-dev/cmdstanr/issues/296),
  [\#302](https://github.com/stan-dev/cmdstanr/issues/302))

- Fix reporting of time after using `fixed_param` method.
  ([\#303](https://github.com/stan-dev/cmdstanr/issues/303),
  [\#307](https://github.com/stan-dev/cmdstanr/issues/307))

- With `refresh = 0`, no output other than error messages is printed
  with `$optimize()` and `$variational()`.
  ([\#324](https://github.com/stan-dev/cmdstanr/issues/324))

- Fix issue where names of generated files could clash.
  ([\#326](https://github.com/stan-dev/cmdstanr/issues/326),
  [\#328](https://github.com/stan-dev/cmdstanr/issues/328))

- Fix missing `include_paths` in `$syntax_check()`.
  ([\#335](https://github.com/stan-dev/cmdstanr/issues/335),
  [@mike-lawrence](https://github.com/mike-lawrence))

#### New features

- CSV reading is now faster by using
  [`data.table::fread()`](https://rdrr.io/pkg/data.table/man/fread.html).
  ([\#318](https://github.com/stan-dev/cmdstanr/issues/318))

- [`install_cmdstan()`](https://mc-stan.org/cmdstanr/rc/reference/install_cmdstan.md)
  gains argument `version` for specifying which version of CmdStan to
  install. ([\#300](https://github.com/stan-dev/cmdstanr/issues/300),
  [\#308](https://github.com/stan-dev/cmdstanr/issues/308))

- New function
  [`check_cmdstan_toolchain()`](https://mc-stan.org/cmdstanr/rc/reference/install_cmdstan.md)
  that checks if the appropriate toolchains are available.
  ([\#289](https://github.com/stan-dev/cmdstanr/issues/289))

- `$sample()` method for CmdStanModel objects gains argument `chain_ids`
  for specifying custom chain IDs.
  ([\#319](https://github.com/stan-dev/cmdstanr/issues/319))

- Added support for the `sig_figs` argument in CmdStan versions 2.25 and
  above. ([\#327](https://github.com/stan-dev/cmdstanr/issues/327))

- Added checks if the user has the necessary permissions in the RTools
  and temporary folders.
  ([\#343](https://github.com/stan-dev/cmdstanr/issues/343))

## CmdStanR 0.1.3

- New `$check_syntax()` method for CmdStanModel objects.
  ([\#276](https://github.com/stan-dev/cmdstanr/issues/276),
  [\#277](https://github.com/stan-dev/cmdstanr/issues/277))

## CmdStanR 0.1.2

- User is notified by message at load time if a new release of CmdStan
  is available.
  ([\#265](https://github.com/stan-dev/cmdstanr/issues/265),
  [\#273](https://github.com/stan-dev/cmdstanr/issues/273))

- [`write_stan_file()`](https://mc-stan.org/cmdstanr/rc/reference/write_stan_file.md)
  replaces `write_stan_tempfile()`, which is now deprecated. With the
  addition of the `dir` argument, the file written is not necessarily
  temporary. ([\#267](https://github.com/stan-dev/cmdstanr/issues/267),
  [\#272](https://github.com/stan-dev/cmdstanr/issues/272))

## CmdStanR 0.1.1

- New knitr engine
  [`eng_cmdstan()`](https://mc-stan.org/cmdstanr/rc/reference/eng_cmdstan.md)
  and function
  [`register_knitr_engine()`](https://mc-stan.org/cmdstanr/rc/reference/register_knitr_engine.md)
  that allow Stan chunks in R markdown documents to be processed using
  CmdStanR instead of RStan. The new vignette *R Markdown CmdStan
  Engine* provides a demonstration.
  ([\#261](https://github.com/stan-dev/cmdstanr/issues/261),
  [\#264](https://github.com/stan-dev/cmdstanr/issues/264),
  [@bearloga](https://github.com/bearloga))

## CmdStanR 0.1.0

- Beta release
