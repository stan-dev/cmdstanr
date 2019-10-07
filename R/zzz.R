.onAttach <- function(...) {
  ver <- utils::packageVersion("cmdstanr")
  packageStartupMessage("This is cmdstanr version ", ver)
  # packageStartupMessage("- Online documentation and vignettes at mc-stan.org/cmdstanr")
  if (is.null(.cmdstanr$PATH)) {
    packageStartupMessage("- Use set_cmdstan_path() to set the path to CmdStan.")
  } else {
    packageStartupMessage("- CmdStan path set to ", cmdstan_path(), "")
  }
}

.onLoad <- function(...) {
  cmdstanr_initialize()
}


