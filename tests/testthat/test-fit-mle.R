context("fitted-mle")

if (not_on_cran()) {
  set_cmdstan_path()
  fit_mle <- testing_fit("logistic", method = "optimize", seed = 123)
  PARAM_NAMES <- c("alpha", "beta[1]", "beta[2]", "beta[3]")
}

test_that("mle and lp methods work after optimization", {
  skip_on_cran()
  expect_named(fit_mle$mle(), PARAM_NAMES)
  checkmate::expect_numeric(fit_mle$lp(), len = 1)
})

test_that("summary method doesn't error for optimization", {
  skip_on_cran()
  x <- fit_mle$summary()
  expect_s3_class(x, "draws_summary")
  expect_equal(colnames(x), c("variable", "estimate"))
  expect_equal(x$variable, c("lp__", PARAM_NAMES))

  x <- fit_mle$summary("lp__")
  expect_equal(colnames(x), c("variable", "estimate"))
  expect_equal(x$variable, "lp__")
})

test_that("print method works error for optimization", {
  skip_on_cran()
  x <- fit_mle$print()
  expect_s3_class(x, "data.frame")
  expect_equal(colnames(x), "estimate")
  expect_equal(rownames(x), c("lp__", PARAM_NAMES))

  x <- fit_mle$print("lp__")
  expect_equal(colnames(x), c("estimate"))
  expect_equal(rownames(x), "lp__")
})
