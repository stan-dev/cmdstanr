.onAttach <- function(...) {
  ver <- utils::packageVersion("cmdstanr")
  packageStartupMessage("This is cmdstanr version ", ver)
  # packageStartupMessage("- Online documentation and vignettes at mc-stan.org/cmdstanr")
}

.onLoad <- function(...) {
  .cmdstanr$PATH <- Sys.getenv("CMDSTAN")
  .cmdstanr$TEMP_DIR <- tempdir(check = TRUE)
}
