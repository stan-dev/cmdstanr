context("model-diagnose")

set_cmdstan_path()
mod <- testing_model("bernoulli")
data_list <- testing_data("bernoulli")

# these are all valid for diagnose()
ok_arg_values <- list(
  data = data_list,
  init = NULL,
  seed = 12345,
  epsilon = 0.000001,
  error = 0.000001
)

# using any of these should cause diagnose() to error
bad_arg_values <- list(
  data = "NOT_A_FILE",
  init = "NOT_A_FILE",
  seed = "NOT_A_SEED",
  epsilon = -10,
  error = -10
)

ok_arg_sci_nota_values <- list(
  data = data_list,
  init = NULL,
  seed = 12345,
  epsilon = 1e-6,
  error = 1e-6
)


test_that("diagnose() method runs when all arguments specified validly", {
  # specifying all arguments validly
  fit1 <- do.call(mod$diagnose, ok_arg_values)
  expect_is(fit1, "CmdStanDiagnose")

  # leaving all at default (except 'data' and 'seed')
  fit2 <- mod$diagnose(data = data_list, seed = 123)
  expect_is(fit2, "CmdStanDiagnose")
})

test_that("diagnose() method runs when arguments are specified in scientific notation", {


  # specifying all arguments validly
  fit1 <- do.call(mod$diagnose, ok_arg_sci_nota_values)
  expect_is(fit1, "CmdStanDiagnose")
})

test_that("diagnose() method errors for any invalid argument before calling cmdstan", {
  for (nm in names(bad_arg_values)) {
    args <- ok_arg_values
    args[[nm]] <- bad_arg_values[[nm]]
    expect_error(do.call(mod$diagnose, args), regexp = nm)
  }
})

test_that("diagnose() errors with bad combination of arguments", {
  # check a few examples (if these errors are correct then they will be correct
  # for all similar args because of how it's implemented)
  expect_error(
    mod$diagnose(data = data_list, epsilon = -1),
    "Assertion on 'self$epsilon' failed",
    fixed = TRUE
  )
  expect_error(
    mod$diagnose(data = data_list, error = -1),
    "Assertion on 'self$error' failed",
    fixed = TRUE
  )
})

test_that("diagnose() works with specified args", {
  fit <- mod$diagnose(
    data = data_list,
    init = 3,
    seed = 123,
    epsilon = 1e-6,
    error = 1e-6
  )
  expect_true(is.data.frame(fit$gradients()))
  expect_equal(dim(fit$gradients()), c(1, 5))
  expect_true(is.numeric(fit$lp()))
})

test_that("diagnose() works for examples", {
  fit_logistic <- cmdstanr_example(example = "logistic", method = "diagnose")
  expect_true(is.data.frame(fit_logistic$gradients()))
  expect_equal(dim(fit_logistic$gradients()), c(4, 5))
  expect_true(is.numeric(fit_logistic$lp()))
  fit_schools <- cmdstanr_example(example = "schools", method = "diagnose")
  expect_true(is.data.frame(fit_schools$gradients()))
  expect_equal(dim(fit_schools$gradients()), c(10, 5))
  expect_true(is.numeric(fit_schools$lp()))
  expect_true(is.list(fit_schools$metadata()))
  expect_equal(fit_schools$metadata()$test, "gradient")
})

