context("fitted-vb")

set_cmdstan_path()
fit_laplace <- testing_fit("logistic", method = "laplace", seed = 100)
PARAM_NAMES <- c("alpha", "beta[1]", "beta[2]", "beta[3]")


test_that("summary() and print() methods works after vb", {
  x <- fit_laplace$summary()
  expect_s3_class(x, "draws_summary")
  expect_equal(x$variable, c("lp__", "lp_approx__", PARAM_NAMES))

  x <- fit_laplace$summary(variables = NULL, c("mean", "sd"))
  expect_s3_class(x, "draws_summary")
  expect_equal(x$variable, c("lp__", "lp_approx__", PARAM_NAMES))
  expect_equal(colnames(x), c("variable", "mean", "sd"))

  expect_output(expect_s3_class(fit_laplace$print(), "CmdStanLaplace"), "variable")
  expect_output(fit_laplace$print(max_rows = 1), "# showing 1 of 6 rows")
})

test_that("draws() method returns posterior sample (reading csv works)", {
  draws <- fit_laplace$draws()
  expect_type(draws, "double")
  expect_s3_class(draws, "draws_matrix")
  expect_equal(posterior::variables(draws), c("lp__", "lp_approx__", PARAM_NAMES))
})

test_that("lp(), lp_approx() methods return vectors (reading csv works)", {
  lp <- fit_laplace$lp()
  lg <- fit_laplace$lp_approx()
  expect_type(lp, "double")
  expect_type(lg, "double")
  expect_equal(length(lp), nrow(fit_laplace$draws()))
  expect_equal(length(lg), length(lp))
})

test_that("time() method works after laplace", {
  run_times <- fit_laplace$time()
  checkmate::expect_list(run_times, names = "strict", any.missing = FALSE)
  testthat::expect_named(run_times, c("total"))
  checkmate::expect_number(run_times$total, finite = TRUE)
})

test_that("output() works for laplace", {
  expect_output(fit_laplace$output(), "method = laplace")
})

test_that("draws() works for different formats", {
  a <- fit_laplace$draws()
  expect_true(posterior::is_draws_matrix(a))
  a <- fit_laplace$draws(format = "list")
  expect_true(posterior::is_draws_list(a))
  a <- fit_laplace$draws(format = "array")
  expect_true(posterior::is_draws_array(a))
  a <- fit_laplace$draws(format = "df")
  expect_true(posterior::is_draws_df(a))
})

test_that("draws() errors if invalid format", {
  expect_error(
    fit_laplace$draws(format = "bad_format"),
    "The supplied draws format is not valid"
  )
})

