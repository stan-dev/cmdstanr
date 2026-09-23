# CmdStanR (development version)

This is a major release with enough noteworthy items that we've broken the
release notes into sections. The **Building models** section covers the
compilation redesign. In particular, `cmdstan_model()` now compiles the model,
or reuses an up-to-date executable, before it returns, and the separate
`$compile()` method is gone (code that calls `$compile()` or passes
`compile = FALSE` will need to change). The other sections are independent of
it: **Data and initial values** covers improvements to how data and initial
values are handled, **Other new features** and **Bug fixes** cover what their
titles suggest, and **Removed and deprecated** lists everything else that could
break existing code beyond the compilation redesign.

Each entry says what changed and what it replaced, so you or your coding agent
can update existing code from them.

## Building models

* `cmdstan_model()` now compiles the model, or reuses an up-to-date executable,
before it returns. The `compile` argument and the `$compile()` method are gone.
To compile a Stan file without creating a model object use the new
`compile_stan_file()`, which returns the path to the executable. The new
`check_syntax_stan_file()`, `format_stan_file()` and `variables_stan_file()`
likewise work from a Stan file alone. (#1256)
* `cmdstan_model()` recompiles whenever anything it tracks has changed: the Stan
program and its `#include`s, the user header, `cpp_options`, `stanc_options`,
`make/local` or the CmdStan installation, and its message says which. Changes it
cannot see, such as a new C++ compiler, need `force_recompile = TRUE`. The
vignette "How does CmdStanR work?" lists them. (#1255, #1237, #1019)
* Each executable now comes with a build record, a hidden JSON file beside it
that says how it was built: `bernoulli` gets `.bernoulli.cmdstanr.json` and
`bernoulli.exe` gets `.bernoulli.exe.cmdstanr.json`. Add `.*.cmdstanr.json` to
`.gitignore` wherever the executable is already ignored. If the record is
missing, `cmdstan_model()` recompiles. The vignette "How does CmdStanR work?"
has the details. (#1238)
* The new `stan_build_info()` shows how an executable was built: the options it
was asked for, the files it read, the CmdStan installation that built it, and
what the executable itself reports. Without a usable record it says why and
shows only what the executable reports. `$build_info()` does the same for a
model object. (#1258)
* When a build depends on files CmdStanR cannot track, such as a makefile
included from `make/local` or a header included from the user header, the build
prints a one-line note saying that changes to them need `force_recompile =
TRUE`. (#1257)
* `pedantic = TRUE` now runs the pedantic check whether or not the executable is
rebuilt. Previously an up-to-date executable skipped the build and the check
with it. (#1258)
* Running a model whose executable is out of date is now an error that points at
`cmdstan_model()`. Previously the stale executable ran as if it were current.
Because the check reads the Stan file, a model created from one needs that file
whenever it runs. To run an executable without its Stan file, create the model
with `cmdstan_model(exe_file = )`. (#1255)
* A failed compilation leaves the previous executable in place. Previously a
failure at the C++ stage could leave the old executable paired with model
methods generated from the new program, and a failed installation could leave
the model with no executable at all. (#1235)
* The `compile_standalone` and `compile_model_methods` arguments are gone.
Previously both did nothing when the executable was already up to date. Call
`$expose_functions()` and `fit$init_model_methods()` instead, which work whether
the executable was just compiled or reused. (#1256, #1245)
* `cmdstan_model(exe_file = )` with no `stan_file` now rejects `cpp_options`,
`stanc_options`, `include_paths`, `user_header`, `force_recompile` and
`pedantic`: with no Stan file there is nothing to compile, so the executable is
used as it is. The `cmdstanr_force_recompile` option is ignored for such a
model. (#1258)
* `cmdstan_model()` no longer accepts `stan_file` and `exe_file` together. Use
`dir` to choose the directory the executable is built in. The executable is
named after the Stan file. Previously `exe_file` beside `stan_file` set the path
the executable was built at. (#1258)
* `$exe_file()` no longer accepts a path. Where the executable goes is set when
the model is created, with `dir`. (#1253)
* `$cmdstan_version()` now reports the version of CmdStan that compiled the
executable, not the version at `cmdstan_path()`. (#1249)
* `$cpp_options()` now returns exactly the options the model was created with,
spelled as make variables: `list(stan_threads = TRUE)` comes back as
`STAN_THREADS`. What the executable reports about its own build has moved to
`stan_build_info()`. (#1019, #1258)
* Every `cpp_options` entry must now be named, with a make variable name. An
unnamed entry gets an error saying where it belongs: `list(NAME = value)` for a
plain assignment, `cmdstan_make_local()` for `+=` and the other makefile
operators. Previously unnamed entries reached `make` but nothing else saw them.
(#1250)
* `cpp_options = list(stan_threads = FALSE)` now turns threading off, even when
`make/local` turns it on, and the same holds for `FALSE` on any option.
Previously `FALSE` was passed through as a value, which enabled the option.
(#1251)
* Running a model compiled with threading no longer errors when
`threads_per_chain` is not set. Asking for more than one thread from a model
compiled *without* threading is now an error. The thread count also no longer
lingers in the session's environment after the call. (#1258)
* Each build setting now has one place to go, and putting it anywhere else is an
error that points at the right argument. (#1258)
  - Include paths go in `include_paths`, not in `stanc_options`, in
    `cpp_options` as `STANCFLAGS` or in `make/local`'s `STANCFLAGS`. Previously
    a model with include paths in `stanc_options` compiled and then failed on
    `$sample()`.
  - The user header goes in `user_header`, not in `cpp_options`. The new
    `$user_header()` method returns its path, and `$cpp_options()` no longer
    includes it.
  - Other stanc flags go in `stanc_options`, not in `cpp_options` as
    `STANCFLAGS`.
  - `stanc_options` rejects the flags CmdStanR's own arguments set:
    `include-paths` (use `include_paths`), `warn-pedantic` (`pedantic`),
    `allow-undefined` (`user_header`), `use-opencl` (`cpp_options =
    list(stan_opencl = TRUE)`) and `name`, which comes from the file name.
* A flag in `stanc_options` now overrides the same flag in `make/local`'s
`STANCFLAGS`. (#1258)
* Named `stanc_options` values such as `list(canonicalize = "deprecations")` and
numeric ones such as `list("max-line-length" = 78)` now work. Previously the
named values reached `stanc` shell-quoted, which it rejected, and the numeric
ones were dropped. (#1227, #1233)
* A `stanc` error now stops the build immediately and shows `stanc`'s message.
Previously it surfaced several steps later. (#1227)
* An include path that does not exist is now reported by its absolute path.
(#1227)
* `$include_paths()` now returns absolute paths, resolved when the model is
created. Previously a relative include path was resolved on every `stanc` call,
so changing the working directory could point `#include` at the wrong directory.
(#1229)
* `#include` directories with spaces in their paths now work. (#820, #1230)
* Error messages from a running model now name the Stan file the model was
created from. Previously they named a temporary copy of the program that no
longer existed. (#1258)
* `$code()` and `$variables()` now describe the program the model was created
from, even after `$format(overwrite_file = TRUE)` rewrites the file. Create the
model again to pick up the change. Previously `$format(overwrite_file = TRUE)`
replaced what `$code()` returned. (#1258)
* `$expose_functions()` and `fit$init_model_methods()` now work on a model or
fit loaded with `readRDS()`, compiling the functions and methods again.
Previously they claimed to be compiled already and calling one failed with a
null symbol address. (#1157, #1158)
* Compiling no longer leaves a copy of the Stan program, the generated C++ and a
second copy of the executable in the session's temporary directory, and checking
syntax or reading a program's variables no longer leaves stanc's output there.
(#1258)

## Data and initial values

* Lists of matrices/vectors and data frames can now be supplied for variables
declared as `int` in the Stan program. Previously these worked only for `real`
variables and errored for `int` ones. (#817)
* Data frame columns that are not numeric, integer, logical, or
factor are now an error. Previously `data.matrix()` silently coerced them, so a
character column reached Stan as alphabetically ordered integer codes. Convert
the column explicitly, e.g. with `as.integer()`, if integer codes are what you
want. (#1225)
* Lists of logical vectors/matrices are now converted to integers like logical
variables are, instead of erroring. (#1225)
* Supplying a factor for a variable not declared as `int` is now an error. (#1225)
* Factors are now accepted for length-1 `int` arrays (e.g. `array[1] int x`),
which previously errored. (#1225)
* Pathfinder fits used as initial values now use uniform weights when CmdStan
already PSIS-resampled their draws, avoiding a second application of importance
weights. (#1206)
* Pathfinder fits used as initial values now correctly treat draws with different
initialization parameter values as distinct even when their log weights are
equal, and collapse duplicate resampled draws while retaining their selection
frequency. (#1207)
* `pathfinder()` now passes separately supplied initial values to every path 
instead of using only the first path's initial values. (#1206)
* Initial values taken from a fitted model object now work for a model created
with only `exe_file`. Previously the error was "'init' contains empty lists".
(#1171)
* Functions supplied as `init` are no longer called an extra time to validate
them. Validation occurs in the same pass. (#1195)

## Other new features

* The new `print_stan_file()` prints a Stan file, with syntax highlighting when
used in a Quarto or R Markdown document. (#1166)
* The new `$cmdstan_defaults()` method returns CmdStan's default argument
values for a method, `"sample"` by default, under the corresponding CmdStanR
argument names. (#1167)
* The new `$materialize()` method reads a fit's draws, sampler diagnostics,
initial values and profiles from the CSV files into R in one call. (#1181)
* `$save_object()` gains `format = "qs2"`, which saves with the qs2 package
instead of `saveRDS()` and is faster and uses less memory. (#1125)
* `as_cmdstan_fit()` gains a `variables` argument to read only some of the
variables from the CSV files. (#1121)
* `$generate_quantities()` gains the `show_messages` and `show_exceptions`
arguments that the other methods have. (#1142)
* `$loo()` now defaults to `r_eff = FALSE`, skipping the relative efficiency
computation, which can be very slow. Set `r_eff = TRUE` for the previous
behavior. (#1091)
* `$log_prob()`, `$grad_log_prob()`, and other model methods are now faster
after initialization. (#1274)
* `fit$init_model_methods()` and `$expose_functions()` gain a `quiet` argument
that suppresses the messages printed while the methods or functions compile.
(#914)
* `install_cmdstan()` now offers to copy the `make/local` flags of the
current installation into the new one before building it, so the new CmdStan is
built with the same flags. In an interactive session it shows the previous
`make/local` and asks if the `copy_make_local` argument isn't set to `TRUE` or
`FALSE`. (#1267)
* `cmdstan_make_local()` now skips flags that are already in `make/local`.
Previously copying the flags of a previous installation after every upgrade
added the same lines again each time. (#1266)
* Chain IDs in generated file names are now zero-padded to at least two digits, 
for example `01` instead of `1`. (#1244)
* When using CmdStan through WSL, paths for output, diagnostic, profile, config, 
and metric files now remain accessible to Windows R when an explicit output 
directory is supplied. (#1110; related: #1113)
* `check_cmdstan_toolchain()` now locates Windows toolchains using `R_TOOLS_SOFT` 
and falls back to `PATH`, improving support for alternate R distributions and 
future Rtools releases. (#1211)
* Exposing functions using names that are reserved keywords now throws an 
informative error message. (#1154)
* `save_cmdstan_config` and `save_metric` default to `FALSE` but can be
set to `TRUE` for an entire R session via new global options. (#1159)
* The compilation spinner can now be disabled for an entire R session by setting
the new `cmdstanr_spinner` global option to `FALSE`. The spinner shown while
installing or rebuilding CmdStan and while checking syntax also respects this
option, and is no longer shown when knitting. (#486)
* `$generate_quantities()` now also accepts `CmdStanMLE`, `CmdStanLaplace`,
and `CmdStanPathfinder` fitted model objects as `fitted_params`. (#1203)
* `$generate_quantities()` now reports per-process execution times with
CmdStan 2.39 or newer, and `read_cmdstan_csv()` returns these times from
standalone generated quantities CSV files. (#1168)

## Bug fixes

* `pathfinder()` now respects `save_single_paths = TRUE` instead of always
passing `0` to CmdStan.
* The `save_latent_dynamics` argument is now limited to `$sample()`, 
`$sample_mpi()`, and `$variational()`, matching the CmdStan algorithms 
that support diagnostic CSV output.
* `save_metric_files()` now gives an informative error when metric files were
not created and keeps saved metric files after the fitted model is
garbage-collected. (#1021)
* `cmdstan_model()` no longer fails when `MAKEFLAGS` turns on directory
printing. (#1163)
* Quoted values in `make/local`'s `STANCFLAGS` now reach `stanc` as one
argument. Previously they were split on whitespace. (#1232)
* `laplace()` no longer overwrites the internally generated optimizer CSV when
`mode = NULL` and `output_basename` is supplied. The internally generated
optimizer CSV now uses the filename `<output_basename>-mode-01.csv`.
* `read_cmdstan_csv()` now expands `~` in file paths. (#1098)
* Loading CmdStanR no longer causes `parallel::mclapply()` to leave zombie
processes behind. (#1105)
* Supplying data with a `NULL` element now gives an informative error. (#1129)
* `$metadata()` now reports the number of chains in `num_chains`. Previously it
was always 1. (#1187)
* `$lp_approx()` and `$mle()` now return numeric vectors whatever the
`cmdstanr_draws_format` option is set to. (#1190)
* A Stan file name with several spaces, or quotes, now gives a valid model name
for `stanc`. Previously only the first space was replaced and the quotes ended
up in the generated C++. (#1200)
* `$draws()` on a pathfinder fit now orders the diagnostic columns the same way
as the other methods. (#1205)
* An executable that cannot be run, for example one that lost its execute bit
or was built for another platform, now gives an error naming the executable
and saying how to rebuild it. Previously the fitting methods and
`$cmdstan_defaults()` surfaced a raw `processx` error. (#1246)
* On Windows a model executable is now launched with the TBB it was built
against. Previously the selected CmdStan installation's TBB was used, which was
wrong once `set_cmdstan_path()` had selected a different one. (#1261)

## Removed and deprecated

* Minimum R version increased to 4.0.0. (#1144)
* CmdStan versions older than 2.35.0 are no longer supported. To use an older
CmdStan version install an older CmdStanR release from GitHub. (#1144)
* The `CMDSTANR_NO_VER_CHECK` R option and environment variable are deprecated 
as of CmdStanR 1.0.0; use the lowercase `cmdstanr_no_ver_check` forms instead.
* `pathfinder()` now uses the `threads` argument (`num_threads` is deprecated),
to be consistent with other methods.
* Removed legacy Windows toolchain paths for older CmdStan releases. (#1144)
* `CMDSTANR_USE_MSYS_TOOLCHAIN` is now deprecated and ignored (with a warning). (#1144)
* Removed deprecated items (replacements in parentheses). (#1061)
  - `read_sample_csv()` (`read_cmdstan_csv()`)
  - `write_stan_tempfile()` (`write_stan_file()`)
  - `jacobian_adjustment` argument to `fit$log_prob()` and similar methods (`jacobian` argument)
  - `output_samples` argument to `model$variational()` (`draws` argument)
  - `hessian` argument to `fit$init_model_methods()` (`hessian` method always compiled now)
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

# CmdStanR 0.9.0

## General Improvements/Changes

* Added compatibility for RTools45 (#1066)
* CmdStanR will now use RTools with no additional toolchain updates needed on 
Windows (CmdStan 2.35+ only; #1065, #1054)
* Improve error messages when calling `sampler_diagnostics()` with `fixed_param=TRUE`
* Improve numerical stability in calculation of effective sample size during 
`loo` method (#1057)
* Improve numerical stablity with very small log-ratios in calculation of 
effective sample size during `loo` method (#1015)
* Add warning if input data/inits have been coerced to ints (#994)

## Bugfixes

 * Don't require fixed_param for models with zero parameters (only GQs) for 
 CmdStan >= 2.36 (#1046)
 * Improve detection/handling of `make` (#1036)
 * Fix saving of model objects to network drive (#1038, thanks to @bschneidr)
 * Update usage of `untar` to fix installation errors (#1034)
 * Respect compilation flags in `make/local` when exposing functions or model 
 methods (#1003)
 * Fix passing of include paths to CmdStan (#1000)
 * Fix passing of factor data to CmdStan (#999)
 * Fix extraction and passing of array data/parameters as model inits (#993)

## Documentation Updates

 * Clarifications to usage of `optimize` and `loo` methods (#1060)
 * Add documentation for faster model saving with large models (#1042)
 * Remove mentions of `rstan::read_stan_csv` due to incompatibility with newer 
 CmdStan outputs (#1018)
 * Document global option `cmdstanr_print_line_numbers` for printing line 
 numbers (#1017)
 * Change usage of 'chapter' to 'section' in documentation (#1014)
 * Remove examples of updating removed array syntax as functionality no longer 
 supported in CmdStan (#1008)
 * Change usages of 'sampling statement' -> 'distribution statement' (#987)

# CmdStanR 0.8.1

## Minor changes

* Added `CMDSTANR_USE_RTOOLS` environment variable to force stock RTools on 
Windows by @andrjohns in #980
* Added support for Windows ARM64 by @andrjohns in #990
* Automatically initialise model methods when called, add `inc_warmup` argument 
to `$unconstrain_draws()` by @andrjohns in #985

## Bugfixes

* Fix errors when using pathfinder object as initial values by @avehtari in #984
* Fix error with `$unconstrain_draws()` returning incorrect assumptions in some 
cases by @andrjohns in #983
* Fix spurious errors about missing CmdStan config files by @andrjohns in #981
* Fix linking error when exposing SUNDIALS/KINSOL functions or model methods by @andrjohns in #977
* Fix long-standing error with OneDrive paths on Windows by @andrjohns in #990

# CmdStanR 0.8.0

## Major new features

* Add functionality for passing `CmdStanFit` objects as initial values by 
@SteveBronder in #937

## Other improvements

* Add compatibility with CmdStan 2.35 by @andrjohns in #972
* Add `show_messages` and `show_exceptions` arguments to all methods for 
controlling output by @andrjohns in #897
* Drop RcppEigen dependency, implement basic Eigen -> C++ interop 
by @andrjohns in #899
* Add compatibility with CmdStan 2.34 by @andrjohns in #905 #910
* Add a format argument to the `unconstrain_draws()` method to specify draws 
format of return by @andrjohns in #886
* Align `cmdstanr` EBFMI diagnostic threshold with CmdStan by @andrjohns in #892
* Add global option `cmdstanr_print_line_numbers` to add line number to model 
printing by @sbfnk in #967
* Add new CmdStan arguments `save_metric` and `save_cmdstan_config` 
by @venpopov in #932
* Add documentation for CmdStanR global options by @jgabry in #951
* Add documentation for how to obtain structured output similar to 
`rstan::extract()` using a combination of `cmdstanr` and `posterior` 
by @jgabry in #955
* Added coercion generics for CmdStanFit objects by @gowerc in #943
* `psis_resample` and `calculate_lp` arguments added to Pathfinder method 
by @SteveBronder in #903
* Documentation and tests for LOO method updated by @jgabry in #923
* Global option `cmdstanr_warn_inits` added to disable warnings about partially 
specified initial values by @jgabry in #913
* Updates to MCMC `output_dir` documentation by @jgabry in #929

## Bugfixes

* Fix broken link in OpenCL documentation by @eipi10 in #908
* Fix a minor typo in the README by @jgabry in #911
* Make exported RNG functions respect changes to R's seed by @andrjohns in #973
* Optimisations for model methods functions by @andrjohns in #960
* Bugfix for passing function for initial values with Pathfinder method and 
default `num_paths` by @andrjohns in #964
* Continue with compilation if `compile_stanalone=TRUE` but no functions are 
found by @jgabry in #956
* Update tests and CI for compatibility with MacOS ARM64 by @andrjohns in #958
* Fix handling of `inv_metric` argument with only 1 parameter by @venpopov in #935
* Fixes for compatibility with RTools44 by @andrjohns in #952 #959

# CmdStanR 0.7.0

## Major new features

* New `laplace` method by @jgabry in #800
* New `pathfinder` method by @SteveBronder in #848

## Other improvements and bug fixes

* Add missing link to diagnose method in CmdStanModel doc by @jgabry in #833
* Improvements to compile tests by @martinmodrak in #836
* Changed the delay behavior in wsl_installed by @martinmodrak in #839
* Update array syntax in website vignette by @andrjohns in #841
* Compatibility fixes for cmdstan 2.33+ by @jgabry in #843
* Suggest format method after error due to old syntax  by @jgabry in #852
* Clarifications in R-markdown vignette by @jgcolman in #854
* Update linux/wsl detection for install arch by @andrjohns in #856
* Fix handling of single-length inits for containers by @andrjohns in #857
* Add support/tests for exposing functions with tuples by @andrjohns in #860
* Add support/tests for exporting functions with complex types by @andrjohns in #861
* Add option for installing from release archive by @andrjohns in #866
* Improve Pathfinder doc by @avehtari in #875
* Rename `jacobian_adjustment` argument to `jacobian` by @jgabry in #879
* Fix get_cmdstan_flags('STANCFLAGS') in recursive make by @pearsonca in #881

# CmdStanR 0.6.1

* Store return codes instead of always querying exit status by @jgabry in #798
* enable jacobian argument for optimization by @jgabry in #799
* Fix init_model_methods for models with no data by @andrjohns in #801
* Document a CmdStan-focused way to pre-compile Stan models in R packages 
by @wlandau in #809
* Describe how to efficiently save model fit objects by @wlandau in #816
* fix errors in doc for new methods by @jgabry in #823
* Give informative error when exposing stan functions with precompiled model 
by @andrjohns in #831
* Bugfixes in .stanfunctions, hessian model method, and exposing RNG functions 
by @andrjohns in #811
* Fix variable_skeleton() with containers by @andrjohns in #832
* Improve handling of user header by @martinmodrak in #818
* change duplicate stdout_file to stderr_file by @jgabry in #834


# CmdStanR 0.6.0

### Major new features

* New `expose_functions()` method to expose Stan functions to R by @andrjohns 
in #702. See `?expose_functions`.
* New methods for accessing log_prob, grad_log_prob, hessian, un/constrain 
variables by @andrjohns in #701. See `?init_model_methods`.

### Other changes

* mod$variables works w includes in precompile state (fix #680) by @MKyhos in #682
* Update broken link for Stan OpenCL support page by @erictleung in #686
* Add newline to check syntax output by @rok-cesnovar in #689
* Allow exposing functions without sampling by @andrjohns in #705
* Expose skeleton by @andrjohns in #706
* WSL - Run cmdstan and models under WSL filesystem by @andrjohns in #696
* Bugfix - Deep copy method/function environments by @andrjohns in #709
* Add option for including jacobian adjustments in hessian method by @andrjohns in #710
* WSL Optimisations and Bugfixes for CI by @andrjohns in #711
* add stancflags from make/local by @rok-cesnovar in #690
* Update co-authors by @andrjohns in #715
* Update model methods parameter naming and extract skeleton function by @andrjohns in #724
* Add method for unconstraining all parameter draws by @andrjohns in #729
* Improve efficiency of variable matching by @sbfnk in #736
* Add verbosity to download output and errors by @andrjohns in #745
* Update handling of show_messages, add show_exceptions by @andrjohns in #746
* Rtools43 support by @andrjohns in #755
* Add stanc M1 make patch, suppress boost warnings by @andrjohns in #756
* more examples of summary method by @gravesti in #751
* Fix model$format and model$check_syntax for compiled models with 
include-paths by @adrian-lison in #775
* Generalise RTools config/support by @andrjohns in #777
* New posterior vignette by @gravesti in #719
* Add moment-matching support to $loo() method by @andrjohns in #778
* replace \ with function by @jsocolar in #789

# CmdStanR 0.5.3

### New features

* On Windows, users can now install and use CmdStan with WSL (Windows
Subsystem for Linux). Set `wsl=TRUE` in `install_cmdstan()` to install CmdStan
for use with WSL. This can offer significant speedups compared to native
Windows execution. (#677, @andrjohns)

### Bug fixes

* In `cmdstan_default_path()` we now ignore directories inside `.cmdstan` 
that don't start with `"cmdstan-"`. (#651)

* Fixed Windows issue related to not locating `grep.exe` or when it is located 
in a path with spaces. (@weshinsley, #661, #663)

* Fixed a bug with diagnostic checks when ebfmi is NaN.

* Fixed a bug that caused issues when using `~` or `.` in paths supplied to the
`cmdstanr_write_stan_file_dir` global option.

* Fixed a bug that caused the `time()` method fail when some of the chains 
failed to finish succesfully.

# CmdStanR 0.5.2

* Refactored toolchain installation and checks for R 4.x on Windows and added 
support for Rtools42. (#645)

* Expanded the use of `CMDSTAN` environment variable to point to CmdStan 
installation _or_ directory containing CmdStan installations. (#643)

* New vignette on how to handle deprecations using the `$format()` method. (#644)


# CmdStanR 0.5.1

* Temporarily disable `format="draws_rvars"` in the `$draws()` method due to a
bug. Until this is fixed users can make use of `posterior::as_draws_rvars()` to
convert draws from CmdStanR to the `draws_rvars` format. (#640)

# CmdStanR 0.5.0

### Bug fixes

* Fixed bug that caused stdour/stderr not being read at the end of
optimization. (#522)

* Fixed issue with handling `NA` as the reported external process
status. (#544, @myshkin)

* Fixed issue with handling models with no parameters and CmdStan
2.27+.

### New features

* Default directory changed to `.cmdstan` instead of `.cmdstanr` so that
CmdStanPy and CmdStanR can use the same CmdStan installations. Using `.cmdstanr`
will continue to be supported until version 1.0 but `install_cmdstan()` will now
default to `.cmdstan` and CmdStanR will first look for `.cmdstan` before falling
back on `.cmdstanr`. (#454)

* New method `diagnose()` for CmdstanModel objects exposes CmdStan's `diagnose`
method for comparing Stan's gradient computations to gradients computed via
finite differences. (#485)

* New method `$variables()` for CmdstanModel objects that returns a list of
variables in the Stan model, their types and number of dimensions. Does
not require the model to be compiled. (#519)

* New method `$format()` for auto-formatting and canonicalizing the Stan models. (#625)

* Added the option to create `CmdStanModel` from the executable only with the
`exe_file` argument. (#564)

* Added a convenience argument `user_header` to `$compile()` and `cmdstan_model()`
that simplifies the use of an external .hpp file to compile with the model.

* Added the `cmdstanr_force_recompile` global option that is used for forcing
recompilation of Stan models. (#580)

* New method `$code()` for all fitted model objects that returns the Stan code
associated with the fitted model. (#575)

* New method `$diagnostic_summary()` for CmdStanMCMC objects that summarizes the
sampler diagnostics (divergences, treedepth, ebfmi) and can regenerate the
related warning messages. (#205)

* New `diagnostics` argument for the `$sample()` method to specify which
diagnostics are checked after sampling. Replaces `validate_csv` argument. (#205)

* Added E-BFMI checks that run automatically post sampling. (#500, @jsocolar)

* New methods for `posterior::as_draws()` for CmdStanR fitted model objects.
These are just wrappers around the `$draws()` method provided for convenience. (#532)

* `write_stan_file()` now choose file names deterministically based on the code
so that models do not get unnecessarily recompiled when calling the function
multiple times with the same code. (#495, @martinmodrak)

* The `dir` argument for `write_stan_file()` can now be set with a global
option. (#537)

* `write_stan_json()` now handles data of class `"table"`. Tables are converted
to vector, matrix, or array depending on the dimensions of the table. (#528)

* Improved processing of named lists supplied to the `data` argument to JSON
data files: checking whether the list includes all required elements/Stan
variables; improved differentiating arrays/vectors of length 1 and scalars
when generating JSON data files; generating floating point numbers with
decimal points to fix issue with parsing large numbers. (#538)

* `install_cmdstan()` now automatically installs the Linux ARM CmdStan when
Linux distributions running on ARM CPUs are detected. (#531)

* New function `as_mcmc.list()` for converting CmdStanMCMC objects to mcmc.list
objects from the coda package. (#584, @MatsuuraKentaro)


# CmdStanR 0.4.0

### Bug fixes

* Fixed issue with retrieving draws with models with spaces in their names. (#453)

* Fixed bug with spaces in path to the temporary folder on Windows. (#460)

* Fixed issue with not reporting model executable name clashing with folder name. (#461)

### New features

* New function `as_cmdstan_fit()` that creates CmdStanMCMC/MLE/VB objects
directly from CmdStan CSV files. (#412)

* `read_cmdstan_csv()` now also returns chain run times for MCMC sampling CSV
files. (#414)

* Faster CSV reading for multiple chains. (#419)

* New `$profiles()` method for fitted model objects accesses profiling
information from R if profiling used in the Stan program. Support for profiling
Stan programs requires CmdStan >= 2.26. (#434)

* New vignette on profiling Stan programs. (#435)

* New vignette on running Stan on the GPU with OpenCL. OpenCL device ids can
now also be specified at runtime. (#439)

* New check for invalid parameter names when supplying init values. (#452, @mike-lawrence)

* Suppressing compilation messages when not in interactive mode. (#462, @wlandau)

* New `error_on_NA` argument for `cmdstan_version()` to optionally return `NULL`
(instead of erroring) if the CmdStan path is not found (#467, @wlandau).

* Global option `cmdstanr_max_rows` can be set as an alternative to specifying
`max_rows` argument to the `$print()` method. (#470)

* New `output_basename` argument for the model fitting methods. Can be used in
conjunction with `output_dir` to get completely predictable output CSV file
paths. (#471)

* New `format` argument for `$draws()`, `$sampler_diagnostics()`,
`read_cmdstan_csv()`, and `as_cmdstan_fit`(). This controls the format of the
draws returned or stored in the object. Changing the format can improve speed
and memory usage for large models. (#482)

# CmdStanR 0.3.0

### Bug fixes

* Fixed reading inverse mass matrix with values written in scientific format in
the CSV. (#394)

* Fixed error caused by an empty data list. Previously if a model didn't require
data then `data` had to either be NULL or be a non-empty list, but now `list()`
is allowed. (#403)

### New features

* Added `$sample_mpi()` for MCMC sampling with MPI. (#350)

* Added informative messages on compile errors caused by precompiled headers (PCH). (#384)

* Added the `cmdstanr_verbose` option for verbose mode. Intended for
troubleshooting, debugging and development. See end of *How does CmdStanR work?*
vignette for details. (#392)

* New `$loo()` method for CmdStanMCMC objects. Requires computing pointwise
log-likelihood in Stan program. (#366)

* The `fitted_params` argument to the `$generate_quantities()` method now also
accepts CmdStanVB, `posterior::draws_array`, and `posterior::draws_matrix`
objects. (#390)

* The `$optimize()` method now supports all of CmdStan's tolerance-related
arguments for (L)BFGS. (#398)

* The documentation for the R6 methods now uses `@param`, which allows package
developers to import the CmdStanR documentation using roxygen2's
`@inheritParams`. (#408)

# CmdStanR 0.2.2

### Bug fixes

* Fixed bug with reading Stan CSV when grep used coloring by default (#364,#371)

* Depend on posterior v0.1.3 to avoid a potential error in `$summary()`. (#383)

### New features

* Added support for native execution on the macOS with the M1 ARM-based CPU. (#375)

* Added threading support via `threads` argument for `$optimize()` and `$variational()`
  (was already available via `threads_per_chain` for `$sample()`). (#369)

# CmdStanR 0.2.1

### Bug fixes

* Fixed bug with processing stanc_options in `check_syntax()`. (#345)

* Fixed bug on access to one variable via `draws()`. (#348)

### New features

* `compile()` and `check_syntax()` methods gain argument `pedantic` for turning
on pedantic mode, which warns about issues with the model beyond syntax errors.
(#361)

# CmdStanR 0.2.0

### Bug fixes

* Fix potential indexing error if using `read_cmdstan_csv()` with CSV files
created by CmdStan without CmdStanR. (#291, #292, @johnlees)

* Fix error when returning draws or sampler diagnostics for a fit with only warmup
and no samples. (#288, #293)

* Fix trailing slashes issue for `dir` in `cmdstan_model()` and `output_dir`
in fitting methods. (#281, #294)

* Fix dimensions error when processing a list of matrices passed in as `data`. (#296, #302)

* Fix reporting of time after using `fixed_param` method. (#303, #307)

* With `refresh = 0`, no output other than error messages is printed with
`$optimize()` and `$variational()`. (#324)

* Fix issue where names of generated files could clash. (#326, #328)

* Fix missing `include_paths` in `$syntax_check()`. (#335, @mike-lawrence)

### New features

* CSV reading is now faster by using `data.table::fread()`. (#318)

* `install_cmdstan()` gains argument `version` for specifying which version of
CmdStan to install. (#300, #308)

* New function `check_cmdstan_toolchain()` that checks if the appropriate
toolchains are available. (#289)

* `$sample()` method for CmdStanModel objects gains argument `chain_ids` for
specifying custom chain IDs. (#319)

* Added support for the `sig_figs` argument in CmdStan versions 2.25 and above. (#327)

* Added checks if the user has the necessary permissions in the RTools and 
temporary folders. (#343)

# CmdStanR 0.1.3

* New `$check_syntax()` method for CmdStanModel objects. (#276, #277)

# CmdStanR 0.1.2

* User is notified by message at load time if a new release of CmdStan is
available. (#265, #273)

* `write_stan_file()` replaces `write_stan_tempfile()`, which is now deprecated.
With the addition of the `dir` argument, the file written is not necessarily
temporary. (#267, #272)


# CmdStanR 0.1.1

* New knitr engine `eng_cmdstan()` and function `register_knitr_engine()` that
allow Stan chunks in R markdown documents to be processed using CmdStanR
instead of RStan. The new vignette _R Markdown CmdStan Engine_ provides a
demonstration. (#261, #264, @bearloga)

# CmdStanR 0.1.0

* Beta release
