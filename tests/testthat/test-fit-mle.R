context("fitted-mle")

set_cmdstan_path()
fit_mle <- testing_fit("logistic", method = "optimize", seed = 123)
mod <- testing_model("bernoulli")
data_list <- testing_data("bernoulli")
PARAM_NAMES <- c("alpha", "beta[1]", "beta[2]", "beta[3]")


test_that("mle and lp methods work after optimization", {
  expect_named(fit_mle$mle(), PARAM_NAMES)
  checkmate::expect_numeric(fit_mle$lp(), len = 1)
})

test_that("summary method works after optimization", {
  x <- fit_mle$summary()
  expect_s3_class(x, "draws_summary")
  expect_equal(colnames(x), c("variable", "estimate"))
  expect_equal(x$variable, c("lp__", PARAM_NAMES))

  x <- fit_mle$summary("lp__")
  expect_equal(colnames(x), c("variable", "estimate"))
  expect_equal(x$variable, "lp__")
})

test_that("print() method works after optimization", {
  expect_output(expect_s3_class(fit_mle$print(), "CmdStanMLE"), "estimate")
  expect_output(fit_mle$print(max_rows = 1), "# showing 1 of 5 rows")
  expect_error(
    fit_mle$print(variable = "unknown", max_rows = 20),
    "Can't find the following variable(s): unknown",
    fixed = TRUE
  ) # unknown parameter
})

test_that("time() method works after optimization", {
  run_times <- fit_mle$time()
  checkmate::expect_list(run_times, names = "strict", any.missing = FALSE)
  testthat::expect_named(run_times, c("total"))
  checkmate::expect_number(run_times$total, finite = TRUE)
})


test_that("output() works for optimization", {
  expect_output(fit_mle$output(),
                "method = optimize")
})

test_that("time is reported after optimization", {
  expect_output(mod$optimize(data = data_list, seed = 123),
                "Finished in")
})

test_that("no error when checking estimates after failure", {
  fit <- cmdstanr_example("schools", method = "optimize", seed = 123) # optim ålways fails for this
  expect_silent(fit$summary()) # no error
})

test_that("draws() works for different formats", {
  a <- fit_mle$draws()
  expect_true(posterior::is_draws_matrix(a))
  a <- fit_mle$draws(format = "list")
  expect_true(posterior::is_draws_list(a))
  a <- fit_mle$draws(format = "array")
  expect_true(posterior::is_draws_array(a))
  a <- fit_mle$draws(format = "df")
  expect_true(posterior::is_draws_df(a))
})

test_that("draws() errors if invalid format", {
  expect_error(
    fit_mle$draws(format = "bad_format"),
    "The supplied draws format is not valid"
  )
})
