library(testthat)
library(cmdstanr)

NOT_CRAN <- identical(Sys.getenv("NOT_CRAN"), "true")

if (NOT_CRAN) {
  test_check("cmdstanr")
} else {
  test_check("cmdstanr", filter = "fit|model", invert = TRUE)
}
