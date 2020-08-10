# Placeholder for next tagged release

* `write_stan_file()` replaces `write_stan_tempfile()`, which is now deprecated.
With the addition of the `dir` argument, the file written is not necessarily
temporary. (#267)

# cmdstanr 0.1.1

* New knitr engine `eng_cmdstan()` and function `register_knitr_engine()` that
allow Stan chunks in R markdown documents to be processed using CmdStanR
instead of RStan. The new vignette _R Markdown CmdStan Engine_ provides a 
demonstration. (#261, #264, @bearloga)

# cmdstanr 0.1.0

* Beta release
