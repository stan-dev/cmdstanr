* Added links regarding `Contributing` to README.md 

# cmdstanr 0.9.0

## General Improvements/Changes

 * Added compatibility for RTools45 (#1066)
 * cmdstanr will now use RTools with no additional toolchain updates needed on Windows (CmdStan 2.35+ only; #1065, #1054)
 * Improve error messages when calling `sampler_diagnostics()` with `fixed_param=TRUE`
 * Improve numerical stability in calculation of effective sample size during `loo` method (#1057)
 * Improve numerical stablity with very small log-ratios in calculation of effective sample size during `loo` method (#1015)
 * Add warning if input data/inits have been coerced to ints (#994)

## Bugfixes

 * Don't require fixed_param for models with zero parameters (only GQs) for CmdStan >= 2.36 (#1046)
 * Improve detection/handling of `make` (#1036)
 * Fix saving of model objects to network drive (#1038, thanks to @bschneidr)
 * Update usage of `untar` to fix installation errors (#1034)
 * Respect compilation flags in `make/local` when exposing functions or model methods (#1003)
 * Fix passing of include paths to CmdStan (#1000)
 * Fix passing of factor data to CmdStan (#999)
 * Fix extraction and passing of array data/parameters as model inits (#993)

## Documentation Updates

 * Clarifications to usage of `optimize` and `loo` methods (#1060)
 * Add documentation for faster model saving with large models (#1042)
 * Remove mentions of `rstan::read_stan_csv` due to incompatibility with newer CmdStan outputs (#1018)
 * Document global option `cmdstanr_print_line_numbers` for printing line numbers (#1017)
 * Change usage of 'chapter' to 'section' in documentation (#1014)
 * Remove examples of updating removed array syntax as functionality no longer supported in CmdStan (#1008)
 * Change usages of 'sampling statement' -> 'distribution statement' (#987)

# cmdstanr 0.8.1

## Minor changes

* Added `CMDSTANR_USE_RTOOLS` environment variable to force stock RTools on Windows by @andrjohns in #980
* Added support for Windows ARM64 by @andrjohns in #990
* Automatically initialise model methods when called, add `inc_warmup` argument to `$unconstrain_draws()` by @andrjohns in #985

## Bugfixes

* Fix errors when using pathfinder object as initial values by @avehtari in #984
* Fix error with `$unconstrain_draws()` returning incorrect assumptions in some cases by @andrjohns in #983
* Fix spurious errors about missing CmdStan config files by @andrjohns in #981
* Fix linking error when exposing SUNDIALS/KINSOL functions or model methods by @andrjohns in #977
* Fix long-standing error with OneDrive paths on Windows by @andrjohns in #990

# cmdstanr 0.8.0

## Major new features

* Add functionality for passing `CmdStanFit` objects as initial values by @SteveBronder in #937

## Other improvements

* Add compatibility with CmdStan 2.35 by @andrjohns in #972
* Add `show_messages` and `show_exceptions` arguments to all methods for controlling output by @andrjohns in #897
* Drop RcppEigen dependency, implement basic Eigen -> C++ interop by @andrjohns in #899
* Add compatibility with CmdStan 2.34 by @andrjohns in #905 #910
* Add a format argument to the `unconstrain_draws()` method to specify draws format of return by @andrjohns in #886
* Align `cmdstanr` EBFMI diagnostic threshold with CmdStan by @andrjohns in #892
* Add global option `cmdstanr_print_line_numbers` to add line number to model printing by @sbfnk in #967
* Add new CmdStan arguments `save_metric` and `save_cmdstan_config` by @venpopov in #932
* Add documentation for CmdStanR global options by @jgabry in #951
* Add documentation for how to obtain structured output similar to `rstan::extract()` using a combination of `cmdstanr` and `posterior` by @jgabry in #955
* Added coercion generics for CmdStanFit objects by @gowerc in #943
* `psis_resample` and `calculate_lp` arguments added to Pathfinder method by @SteveBronder in #903
* Documentation and tests for LOO method updated by @jgabry in #923
* Global option `cmdstanr_warn_inits` added to disable warnings about partially specified initial values by @jgabry in #913
* Updates to MCMC `output_dir` documentation by @jgabry in #929

## Bugfixes

* Fix broken link in OpenCL documentation by @eipi10 in #908
* Fix a minor typo in the README by @jgabry in #911
* Make exported RNG functions respect changes to R's seed by @andrjohns in #973
* Optimisations for model methods functions by @andrjohns in #960
* Bugfix for passing function for initial values with Pathfinder method and default `num_paths` by @andrjohns in #964
* Continue with compilation if `compile_stanalone=TRUE` but no functions are found by @jgabry in #956
* Update tests and CI for compatibility with MacOS ARM64 by @andrjohns in #958
* Fix handling of `inv_metric` argument with only 1 parameter by @venpopov in #935
* Fixes for compatibility with RTools44 by @andrjohns in #952 #959

# cmdstanr 0.7.0

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

# cmdstanr 0.6.1

* Store return codes instead of always querying exit status by @jgabry in #798
* enable jacobian argument for optimization by @jgabry in #799
* Fix init_model_methods for models with no data by @andrjohns in #801
* Document a CmdStan-focused way to pre-compile Stan models in R packages by @wlandau in #809
* Describe how to efficiently save model fit objects by @wlandau in #816
* fix errors in doc for new methods by @jgabry in #823
* Give informative error when exposing stan functions with precompiled model by @andrjohns in #831
* Bugfixes in .stanfunctions, hessian model method, and exposing RNG functions by @andrjohns in #811
* Fix variable_skeleton() with containers by @andrjohns in #832
* Improve handling of user header by @martinmodrak in #818
* change duplicate stdout_file to stderr_file by @jgabry in #834


# cmdstanr 0.6.0

### Major new features

* New `expose_functions()` method to expose Stan functions to R by @andrjohns in #702. See `?expose_functions`.
* New methods for accessing log_prob, grad_log_prob, hessian, un/constrain variables by @andrjohns in #701. See `?init_model_methods`.

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
* Fix model$format and model$check_syntax for compiled models with include-paths by @adrian-lison in #775
* Generalise RTools config/support by @andrjohns in #777
* New posterior vignette by @gravesti in #719
* Add moment-matching support to $loo() method by @andrjohns in #778
* replace \ with function by @jsocolar in #789

# cmdstanr 0.5.3

### New features

* On Windows, users can now install and use CmdStan with WSL (Windows
Subsystem for Linux). Set `wsl=TRUE` in `install_cmdstan()` to install CmdStan
for use with WSL. This can offer significant speedups compared to native
Windows execution. (#677, @andrjohns)

### Bug fixes

* In `cmdstan_default_path()` we now ignore directories inside `.cmdstan` that don't start
with `"cmdstan-"`. (#651)

* Fixed Windows issue related to not locating `grep.exe` or when it is located in a path
with spaces. (@weshinsley, #661, #663)

* Fixed a bug with diagnostic checks when ebfmi is NaN.

* Fixed a bug that caused issues when using `~` or `.` in paths supplied to the
`cmdstanr_write_stan_file_dir` global option.

* Fixed a bug that caused the `time()` method fail when some of the chains failed to finish
succesfully.

# cmdstanr 0.5.2

* Refactored toolchain installation and checks for R 4.x on Windows and added support
for Rtools42. (#645)

* Expanded the use of `CMDSTAN` environment variable to point to CmdStan installation
_or_ directory containing CmdStan installations. (#643)

* New vignette on how to handle deprecations using the `$format()` method. (#644)


# cmdstanr 0.5.1

* Temporarily disable `format="draws_rvars"` in the `$draws()` method due to a
bug. Until this is fixed users can make use of `posterior::as_draws_rvars()` to
convert draws from CmdStanR to the `draws_rvars` format. (#640)

# cmdstanr 0.5.0

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


# cmdstanr 0.4.0

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

# cmdstanr 0.3.0

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

# cmdstanr 0.2.2

### Bug fixes

* Fixed bug with reading Stan CSV when grep used coloring by default (#364,#371)

* Depend on posterior v0.1.3 to avoid a potential error in `$summary()`. (#383)

### New features

* Added support for native execution on the macOS with the M1 ARM-based CPU. (#375)

* Added threading support via `threads` argument for `$optimize()` and `$variational()`
  (was already available via `threads_per_chain` for `$sample()`). (#369)

# cmdstanr 0.2.1

### Bug fixes

* Fixed bug with processing stanc_options in `check_syntax()`. (#345)

* Fixed bug on access to one variable via `draws()`. (#348)

### New features

* `compile()` and `check_syntax()` methods gain argument `pedantic` for turning
on pedantic mode, which warns about issues with the model beyond syntax errors.
(#361)

# cmdstanr 0.2.0

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

* Added checks if the user has the necessary permissions in the RTools and temporary folders. (#343)

# cmdstanr 0.1.3

* New `$check_syntax()` method for CmdStanModel objects. (#276, #277)

# cmdstanr 0.1.2

* User is notified by message at load time if a new release of CmdStan is
available. (#265, #273)

* `write_stan_file()` replaces `write_stan_tempfile()`, which is now deprecated.
With the addition of the `dir` argument, the file written is not necessarily
temporary. (#267, #272)


# cmdstanr 0.1.1

* New knitr engine `eng_cmdstan()` and function `register_knitr_engine()` that
allow Stan chunks in R markdown documents to be processed using CmdStanR
instead of RStan. The new vignette _R Markdown CmdStan Engine_ provides a
demonstration. (#261, #264, @bearloga)

# cmdstanr 0.1.0

* Beta release
