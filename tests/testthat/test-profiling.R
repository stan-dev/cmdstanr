context("profiling")

test_that("profiling works if profiling data is present", {
  skip_on_cran()
  mod <- testing_model("logistic_profiling")
  utils::capture.output(
    fit <- mod$sample(data = testing_data("logistic_profiling"), refresh = 0)
  )
  expect_equal(length(fit$profile_files()), 4)
  profiles <- fit$profiles()
  expect_equal(length(profiles), 4)
  expect_equal(dim(profiles[[1]]), c(3,9))
  expect_equal(profiles[[1]][,"name"], c("glm", "priors", "udf"))
})

test_that("profiling works if no profiling data is present", {
  skip_on_cran()
  mod <- testing_model("logistic")
  utils::capture.output(
    fit <- mod$sample(data = testing_data("logistic"), refresh = 0)
  )
  expect_error(fit$profile_files(),
               "No profile files found. The model that produced the fit did not use any profiling.")
  expect_error(fit$profiles(),
               "No profile files found. The model that produced the fit did not use any profiling.")
  expect_error(fit$save_profile_files(),
               "No profile files found. The model that produced the fit did not use any profiling.")
})

