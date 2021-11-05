context("model-data")
# see separate test-json for testing writing data to JSON

set_cmdstan_path()
mod <- testing_model("logistic")
data_list <- testing_data("logistic")


test_that("error if CmdStan < 2.22 and any 0-dimensional data objects", {
  ver <- cmdstan_version()
  .cmdstanr$VERSION <- "2.21.0"
  expect_sample_output(
    fit <- mod$sample(data = data_list)
  )

  data_list$X <- array(1, dim = 0)
  expect_error(
    fit <- mod$sample(data = data_list),
    "Data includes 0-dimensional data structures."
  )

  data_list$X <- array(1, dim = c(100, 0))
  expect_error(
    fit <- mod$sample(data = data_list),
    "Data includes 0-dimensional data structures."
  )

  data_list$X <- array(1, dim = c(100, 3, 0))
  expect_error(
    fit <- mod$sample(data = data_list),
    "Data includes 0-dimensional data structures"
  )
  .cmdstanr$VERSION <- ver
})

test_that("error if data contains NA elements", {
  # some different ways data can contain NAs
  data_list1 <- data_list2 <- data_list3 <- data_list
  data_list1$N <- NA
  data_list2$y[3] <- NA
  data_list3$X[3, 2] <- NA

  expect_error(mod$sample(data = data_list1), "Variable 'N' has NA values")
  expect_error(mod$sample(data = data_list2), "Variable 'y' has NA values")
  expect_error(mod$sample(data = data_list3), "Variable 'X' has NA values")
})

test_that("empty data list doesn't error if no data block", {
  mod <- cmdstan_model(write_stan_file("
  parameters {
    real x;
  }
  model {
    x ~ normal(0, 1);
  }
  "))

  expect_sample_output(
    fit <- mod$sample(data = list(), chains = 1)
  )

  # would error if fitting failed
  expect_silent(fit$draws())
})
