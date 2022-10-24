library(testthat)
library(cmdstanr)

if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  #test_check("cmdstanr")
  test_file("testthat/test-fit-shared.R")
}
