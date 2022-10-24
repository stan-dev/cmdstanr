library(testthat)
library(cmdstanr)

if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  test_check("cmdstanr")
}
