context("fitted-mle")

if (not_on_cran()) {
  set_cmdstan_path_for_tests()
  stan_program <- test_path("resources/stan/logistic.stan")
  data_file_json <- test_path("resources/data/logistic.data.json")
  mod <- cmdstan_model(stan_file = stan_program)
  utils::capture.output(
    fit_mle <- mod$optimize(data = data_file_json, seed = 123)
  )
  PARAM_NAMES <- c("alpha", "beta[1]", "beta[2]", "beta[3]")
}

test_that("reading in csv optimization output works", {
  skip_on_cran()
  expect_named(fit_mle$mle(), PARAM_NAMES)
  expect_named(fit_mle$lp(), "lp__")
})

test_that("summary method doesn't error for optimization", {
  skip_on_cran()
  # summary method for optimization isn't fully designed yet
  x <- fit_mle$summary()
  expect_s3_class(x, "draws_summary")
  expect_equal(colnames(x), c("variable", "estimate"))
  expect_equal(x$variable, PARAM_NAMES)
})
