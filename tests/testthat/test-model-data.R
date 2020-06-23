context("model-data")
# see separate test-json for testing writing data to JSON

if (not_on_cran()) {
  set_cmdstan_path()
  mod <- testing_model("logistic")
  data_list <- testing_data("logistic")
}

test_that("error if data is list and CmdStan < 2.22", {
  skip_on_cran()
  ver <- cmdstan_version()
  .cmdstanr$VERSION <- "2.21.0"
  expect_sample_output(
    fit <- mod$sample(data = data_list)
  )

  data_list$X <- array(1, dim = c(100, 0))
  expect_error(
    fit <- mod$sample(data = data_list),
    "To use a named list please update your CmdStan installation"
  )

  data_list$X <- array(1, dim = c(100, 3, 0))
  expect_error(
    fit <- mod$sample(data = data_list),
    "To use a named list please update your CmdStan installation"
  )
  .cmdstanr$VERSION <- ver
})

