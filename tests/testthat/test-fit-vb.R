context("fitted-vb")

if (not_on_cran()) {
  set_cmdstan_path()
  fit_vb <- testing_fit("logistic", method = "variational", seed = 123)
  PARAM_NAMES <- c("alpha", "beta[1]", "beta[2]", "beta[3]")
}

# test_that("summary() method works after vb", {
#   skip_on_cran()
#   x <- fit_vb$summary()
#   expect_s3_class(x, "draws_summary")
#   expect_equal(x$variable, PARAM_NAMES)

#   x <- fit_vb$summary(measures = c("mean", "sd"))
#   expect_s3_class(x, "draws_summary")
#   expect_equal(x$variable, PARAM_NAMES)
#   expect_equal(colnames(x), c("variable", "mean", "sd"))
# })

test_that("draws() method returns posterior sample (reading csv works)", {
  skip_on_cran()
  draws <- fit_vb$draws()
  expect_type(draws, "double")
  expect_s3_class(draws, "draws_matrix")
  expect_equal(posterior::variables(draws), PARAM_NAMES)
})

test_that("log_p(), log_g() methods return vectors (reading csv works)", {
  skip_on_cran()
  lp <- fit_vb$log_p()
  lg <- fit_vb$log_g()
  expect_type(lp, "double")
  expect_type(lg, "double")
  expect_equal(length(lp), nrow(fit_vb$draws()))
  expect_equal(length(lg), nrow(fit_vb$draws()))
})
