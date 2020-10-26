# Items for next tagged release

### Bug fixes

* Fix potential indexing error if using `read_cmdstan_csv()` with CSV files
created by CmdStan without CmdStanR. (#291, #292, @johnlees)

* Fix error when returning draws or sampler diagnostics for a fit with only warmup
and no samples. (#288, #293)

* Fix trailing slashes issue for `dir` in `cmdstan_model()` and `output_dir` 
in fitting methods. (#281, #294)

* Fix dimensions error when processing a list of matrices passed in as `data`. (#296, #302)

* Fix reporting of time after using `fixed_param` method. (#303, #307)

### New features

* `install_cmdstan()` gains argument `version` for specifying which version of 
CmdStan to install. (#300, #308)

* New function `check_cmdstan_toolchain()` that checks if the appropriate toolchains are 
available. (#289)

* `$sample()` method for CmdStanModel objects gains argument `chain_ids` for specifying
 custom chain IDs. (#319)


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
