.onAttach <- function(...) {
  ver <- utils::packageVersion("cmdstanr")
  packageStartupMessage("This is cmdstanr version ", ver)
  # packageStartupMessage("- Online documentation and vignettes at mc-stan.org/cmdstanr")
}
