# Items for next tagged release

### Bug fixes

### New features

* Added `$sample_mpi()` for MCMC sampling with MPI. (#350)

* Added informative messages on compile errors caused by precompiled headers (PCH). (#384)

* Added the `cmdstan_verbose` option for verbose mode intended for troubleshooting,
  debugging and development.

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
