library(testthat)
library(cmdstanr)

if (identical(Sys.getenv("NOT_CRAN"), "true") && !nzchar(Sys.getenv("TESTTHAT_CPUS"))) {
  Sys.setenv(TESTTHAT_CPUS = parallel::detectCores())
}

test_check("cmdstanr")
