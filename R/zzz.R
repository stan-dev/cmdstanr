.onAttach <- function(...) {
  ver <- utils::packageVersion("cmdstanr")
  packageStartupMessage("This is cmdstanr version ", ver)
  packageStartupMessage("- Online documentation and vignettes at mc-stan.org/cmdstanr")
  if (is.null(.cmdstanr$PATH)) {
    packageStartupMessage("- Use set_cmdstan_path() to set the path to CmdStan")
    packageStartupMessage("- Use install_cmdstan() to install CmdStan")
  } else {
    packageStartupMessage("- CmdStan path set to: ", cmdstan_path(), "")
    packageStartupMessage("- Use set_cmdstan_path() to change the path")
  }
}

.onLoad <- function(...) {
  # TODO: Remove once posterior is on CRAN
  if (!requireNamespace("posterior", quietly = TRUE)) {
    ver_posterior <- utils::packageVersion("posterior")
    if(ver_posterior < "0.0.2") {
      warning(
        paste0(
          "posterior is version ",
          ver_posterior, 
          ", but should be >= 0.0.2. Please upgrade the posterior package using devtools::install_github(\"jgabry/posterior\")"
        )
      )
    }
  }
  cmdstanr_initialize()
}


