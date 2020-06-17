context("fitted-vb")

if (not_on_cran()) {
  set_cmdstan_path()
  fit_vb <- testing_fit("logistic", method = "variational", seed = 123)
  fit_vb_sci_not <- testing_fit("logistic", method = "variational", seed = 123, iter = 200000, adapt_iter = 100000)
  PARAM_NAMES <- c("alpha", "beta[1]", "beta[2]", "beta[3]")
}

test_that("summary() method works after vb", {
  skip_on_cran()
  x <- fit_vb$summary()
  expect_s3_class(x, "draws_summary")
  expect_equal(x$variable, c("lp__", "lp_approx__", PARAM_NAMES))

  x <- fit_vb$summary(variables = NULL, c("mean", "sd"))
  expect_s3_class(x, "draws_summary")
  expect_equal(x$variable, c("lp__", "lp_approx__", PARAM_NAMES))
  expect_equal(colnames(x), c("variable", "mean", "sd"))
})

test_that("print() method works after vb", {
  skip_on_cran()
  expect_output(expect_s3_class(fit_vb$print(), "CmdStanVB"), "variable")
  expect_output(fit_vb$print(max_rows = 1), "# showing 1 of 6 rows")
})

test_that("draws() method returns posterior sample (reading csv works)", {
  skip_on_cran()
  draws <- fit_vb$draws()
  expect_type(draws, "double")
  expect_s3_class(draws, "draws_matrix")
  expect_equal(posterior::variables(draws), c("lp__", "lp_approx__", PARAM_NAMES))
})

test_that("lp(), lp_approx() methods return vectors (reading csv works)", {
  skip_on_cran()
  lp <- fit_vb$lp()
  lg <- fit_vb$lp_approx()
  expect_type(lp, "double")
  expect_type(lg, "double")
  expect_equal(length(lp), nrow(fit_vb$draws()))
  expect_equal(length(lg), length(lp))
})

test_that("vb works with scientific notation args", {
  skip_on_cran()
  x <- fit_vb_sci_not$summary()
  expect_s3_class(x, "draws_summary")
  expect_equal(x$variable, c("lp__", "lp_approx__", PARAM_NAMES))

  x <- fit_vb_sci_not$summary(variables = NULL, c("mean", "sd"))
  expect_s3_class(x, "draws_summary")
  expect_equal(x$variable, c("lp__", "lp_approx__", PARAM_NAMES))
  expect_equal(colnames(x), c("variable", "mean", "sd"))
})

test_that("time() method works after vb", {
  skip_on_cran()
  run_times <- fit_vb$time()
  checkmate::expect_list(run_times, names = "strict", any.missing = FALSE)
  testthat::expect_named(run_times, c("total"))
  checkmate::expect_number(run_times$total, finite = TRUE)
})
