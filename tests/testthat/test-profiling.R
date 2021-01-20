context("read_cmdstan_csv")

if (not_on_cran()) {
  set_cmdstan_path()
  fit_logistic_no_profiling <- testing_fit("logistic", method = "sample", seed = 123)
  fit_logistic_profiling <- testing_fit("logistic_profiling", method = "sample", seed = 123)
}

test_that("profiling works if profiling data is present", {
  skip_on_cran()
  expect_equal(length(fit_logistic_profiling$profile_files()), 4)
  profiles <- fit_logistic_profiling$profiles()
  expect_equal(length(profiles), 4)
  expect_equal(dim(profiles[[1]]), c(3,9))
  expect_equal(profiles[[1]][,"name"], c("glm", "priors", "udf"))
})

test_that("profiling works if no profiling data is present", {
  skip_on_cran()
  expect_equal(fit_logistic_no_profiling$profile_files(), character(0))
  profiles <- fit_logistic_no_profiling$profiles()
  expect_equal(length(profiles), 0)
})

