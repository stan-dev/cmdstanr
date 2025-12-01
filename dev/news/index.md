# Changelog

## cmdstanr (development version)

## cmdstanr 0.9.0

### General Improvements/Changes

- Added compatibility for RTools45
  ([\#1066](https://github.com/stan-dev/cmdstanr/issues/1066))
- cmdstanr will now use RTools with no additional toolchain updates
  needed on Windows (CmdStan 2.35+ only;
  [\#1065](https://github.com/stan-dev/cmdstanr/issues/1065),
  [\#1054](https://github.com/stan-dev/cmdstanr/issues/1054))
- Improve error messages when calling
  [`sampler_diagnostics()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-sampler_diagnostics.md)
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

## cmdstanr 0.8.1

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

## cmdstanr 0.8.0

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
  [`unconstrain_draws()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-unconstrain_draws.md)
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

## cmdstanr 0.7.0

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

## cmdstanr 0.6.1

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

## cmdstanr 0.6.0

#### Major new features

- New
  [`expose_functions()`](https://mc-stan.org/cmdstanr/dev/reference/model-method-expose_functions.md)
  method to expose Stan functions to R by
  [@andrjohns](https://github.com/andrjohns) in
  [\#702](https://github.com/stan-dev/cmdstanr/issues/702). See
  [`?expose_functions`](https://mc-stan.org/cmdstanr/dev/reference/model-method-expose_functions.md).
- New methods for accessing log_prob, grad_log_prob, hessian,
  un/constrain variables by [@andrjohns](https://github.com/andrjohns)
  in [\#701](https://github.com/stan-dev/cmdstanr/issues/701). See
  [`?init_model_methods`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-init_model_methods.md).

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

## cmdstanr 0.5.3

#### New features

- On Windows, users can now install and use CmdStan with WSL (Windows
  Subsystem for Linux). Set `wsl=TRUE` in
  [`install_cmdstan()`](https://mc-stan.org/cmdstanr/dev/reference/install_cmdstan.md)
  to install CmdStan for use with WSL. This can offer significant
  speedups compared to native Windows execution.
  ([\#677](https://github.com/stan-dev/cmdstanr/issues/677),
  [@andrjohns](https://github.com/andrjohns))

#### Bug fixes

- In
  [`cmdstan_default_path()`](https://mc-stan.org/cmdstanr/dev/reference/cmdstan_default_path.md)
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
  [`time()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-time.md)
  method fail when some of the chains failed to finish succesfully.

## cmdstanr 0.5.2

- Refactored toolchain installation and checks for R 4.x on Windows and
  added support for Rtools42.
  ([\#645](https://github.com/stan-dev/cmdstanr/issues/645))

- Expanded the use of `CMDSTAN` environment variable to point to CmdStan
  installation *or* directory containing CmdStan installations.
  ([\#643](https://github.com/stan-dev/cmdstanr/issues/643))

- New vignette on how to handle deprecations using the `$format()`
  method. ([\#644](https://github.com/stan-dev/cmdstanr/issues/644))

## cmdstanr 0.5.1

- Temporarily disable `format="draws_rvars"` in the `$draws()` method
  due to a bug. Until this is fixed users can make use of
  [`posterior::as_draws_rvars()`](https://mc-stan.org/posterior/reference/draws_rvars.html)
  to convert draws from CmdStanR to the `draws_rvars` format.
  ([\#640](https://github.com/stan-dev/cmdstanr/issues/640))

## cmdstanr 0.5.0

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
  [`install_cmdstan()`](https://mc-stan.org/cmdstanr/dev/reference/install_cmdstan.md)
  will now default to `.cmdstan` and CmdStanR will first look for
  `.cmdstan` before falling back on `.cmdstanr`.
  ([\#454](https://github.com/stan-dev/cmdstanr/issues/454))

- New method
  [`diagnose()`](https://mc-stan.org/cmdstanr/dev/reference/model-method-diagnose.md)
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
  [`cmdstan_model()`](https://mc-stan.org/cmdstanr/dev/reference/cmdstan_model.md)
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

- [`write_stan_file()`](https://mc-stan.org/cmdstanr/dev/reference/write_stan_file.md)
  now choose file names deterministically based on the code so that
  models do not get unnecessarily recompiled when calling the function
  multiple times with the same code.
  ([\#495](https://github.com/stan-dev/cmdstanr/issues/495),
  [@martinmodrak](https://github.com/martinmodrak))

- The `dir` argument for
  [`write_stan_file()`](https://mc-stan.org/cmdstanr/dev/reference/write_stan_file.md)
  can now be set with a global option.
  ([\#537](https://github.com/stan-dev/cmdstanr/issues/537))

- [`write_stan_json()`](https://mc-stan.org/cmdstanr/dev/reference/write_stan_json.md)
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

- [`install_cmdstan()`](https://mc-stan.org/cmdstanr/dev/reference/install_cmdstan.md)
  now automatically installs the Linux ARM CmdStan when Linux
  distributions running on ARM CPUs are detected.
  ([\#531](https://github.com/stan-dev/cmdstanr/issues/531))

- New function
  [`as_mcmc.list()`](https://mc-stan.org/cmdstanr/dev/reference/as_mcmc.list.md)
  for converting CmdStanMCMC objects to mcmc.list objects from the coda
  package. ([\#584](https://github.com/stan-dev/cmdstanr/issues/584),
  [@MatsuuraKentaro](https://github.com/MatsuuraKentaro))

## cmdstanr 0.4.0

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
  [`as_cmdstan_fit()`](https://mc-stan.org/cmdstanr/dev/reference/read_cmdstan_csv.md)
  that creates CmdStanMCMC/MLE/VB objects directly from CmdStan CSV
  files. ([\#412](https://github.com/stan-dev/cmdstanr/issues/412))

- [`read_cmdstan_csv()`](https://mc-stan.org/cmdstanr/dev/reference/read_cmdstan_csv.md)
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
  [`cmdstan_version()`](https://mc-stan.org/cmdstanr/dev/reference/set_cmdstan_path.md)
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
  [`read_cmdstan_csv()`](https://mc-stan.org/cmdstanr/dev/reference/read_cmdstan_csv.md),
  and `as_cmdstan_fit`(). This controls the format of the draws returned
  or stored in the object. Changing the format can improve speed and
  memory usage for large models.
  ([\#482](https://github.com/stan-dev/cmdstanr/issues/482))

## cmdstanr 0.3.0

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

## cmdstanr 0.2.2

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

## cmdstanr 0.2.1

#### Bug fixes

- Fixed bug with processing stanc_options in
  [`check_syntax()`](https://mc-stan.org/cmdstanr/dev/reference/model-method-check_syntax.md).
  ([\#345](https://github.com/stan-dev/cmdstanr/issues/345))

- Fixed bug on access to one variable via
  [`draws()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-draws.md).
  ([\#348](https://github.com/stan-dev/cmdstanr/issues/348))

#### New features

- [`compile()`](https://mc-stan.org/cmdstanr/dev/reference/model-method-compile.md)
  and
  [`check_syntax()`](https://mc-stan.org/cmdstanr/dev/reference/model-method-check_syntax.md)
  methods gain argument `pedantic` for turning on pedantic mode, which
  warns about issues with the model beyond syntax errors.
  ([\#361](https://github.com/stan-dev/cmdstanr/issues/361))

## cmdstanr 0.2.0

#### Bug fixes

- Fix potential indexing error if using
  [`read_cmdstan_csv()`](https://mc-stan.org/cmdstanr/dev/reference/read_cmdstan_csv.md)
  with CSV files created by CmdStan without CmdStanR.
  ([\#291](https://github.com/stan-dev/cmdstanr/issues/291),
  [\#292](https://github.com/stan-dev/cmdstanr/issues/292),
  [@johnlees](https://github.com/johnlees))

- Fix error when returning draws or sampler diagnostics for a fit with
  only warmup and no samples.
  ([\#288](https://github.com/stan-dev/cmdstanr/issues/288),
  [\#293](https://github.com/stan-dev/cmdstanr/issues/293))

- Fix trailing slashes issue for `dir` in
  [`cmdstan_model()`](https://mc-stan.org/cmdstanr/dev/reference/cmdstan_model.md)
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
  [`data.table::fread()`](https://rdatatable.gitlab.io/data.table/reference/fread.html).
  ([\#318](https://github.com/stan-dev/cmdstanr/issues/318))

- [`install_cmdstan()`](https://mc-stan.org/cmdstanr/dev/reference/install_cmdstan.md)
  gains argument `version` for specifying which version of CmdStan to
  install. ([\#300](https://github.com/stan-dev/cmdstanr/issues/300),
  [\#308](https://github.com/stan-dev/cmdstanr/issues/308))

- New function
  [`check_cmdstan_toolchain()`](https://mc-stan.org/cmdstanr/dev/reference/install_cmdstan.md)
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

## cmdstanr 0.1.3

- New `$check_syntax()` method for CmdStanModel objects.
  ([\#276](https://github.com/stan-dev/cmdstanr/issues/276),
  [\#277](https://github.com/stan-dev/cmdstanr/issues/277))

## cmdstanr 0.1.2

- User is notified by message at load time if a new release of CmdStan
  is available.
  ([\#265](https://github.com/stan-dev/cmdstanr/issues/265),
  [\#273](https://github.com/stan-dev/cmdstanr/issues/273))

- [`write_stan_file()`](https://mc-stan.org/cmdstanr/dev/reference/write_stan_file.md)
  replaces
  [`write_stan_tempfile()`](https://mc-stan.org/cmdstanr/dev/reference/write_stan_tempfile.md),
  which is now deprecated. With the addition of the `dir` argument, the
  file written is not necessarily temporary.
  ([\#267](https://github.com/stan-dev/cmdstanr/issues/267),
  [\#272](https://github.com/stan-dev/cmdstanr/issues/272))

## cmdstanr 0.1.1

- New knitr engine
  [`eng_cmdstan()`](https://mc-stan.org/cmdstanr/dev/reference/eng_cmdstan.md)
  and function
  [`register_knitr_engine()`](https://mc-stan.org/cmdstanr/dev/reference/register_knitr_engine.md)
  that allow Stan chunks in R markdown documents to be processed using
  CmdStanR instead of RStan. The new vignette *R Markdown CmdStan
  Engine* provides a demonstration.
  ([\#261](https://github.com/stan-dev/cmdstanr/issues/261),
  [\#264](https://github.com/stan-dev/cmdstanr/issues/264),
  [@bearloga](https://github.com/bearloga))

## cmdstanr 0.1.0

- Beta release
