context("fitted-mcmc")

if (not_on_cran()) {
  set_cmdstan_path()
  fit_mcmc <- testing_fit("logistic", method = "sample",
                          seed = 123, chains = 2)
  fit_mcmc_0 <- testing_fit("logistic", method = "sample",
                            seed = 123, chains = 2,
                            refresh = 0)
  fit_mcmc_1 <- testing_fit("logistic", method = "sample",
                            seed = 123, chains = 2,
                            refresh = 0, save_warmup = TRUE)
  PARAM_NAMES <- c("alpha", "beta[1]", "beta[2]", "beta[3]")
}


test_that("draws() method returns draws_array (reading csv works)", {
  skip_on_cran()
  draws <- fit_mcmc$draws()
  expect_type(draws, "double")
  expect_s3_class(draws, "draws_array")
  expect_equal(posterior::variables(draws), c("lp__", PARAM_NAMES))
  expect_equal(posterior::nchains(draws), fit_mcmc$num_chains())
})

test_that("summary() method works after mcmc", {
  skip_on_cran()
  x <- fit_mcmc$summary()
  expect_s3_class(x, "draws_summary")
  expect_equal(x$variable, c("lp__", PARAM_NAMES))

  x <- fit_mcmc$summary(c("rhat", "sd"))
  expect_equal(colnames(x), c("variable", "rhat", "sd"))
})

test_that("output() method works after mcmc", {
  skip_on_cran()
  checkmate::expect_list(
    fit_mcmc$output(),
    types = "character",
    any.missing = FALSE,
    len = fit_mcmc$runset$num_runs()
  )
  expect_output(fit_mcmc$output(id = 1), "Gradient evaluation took")
})

test_that("time() method works after mcmc", {
  skip_on_cran()
  run_times <- fit_mcmc$time()
  checkmate::expect_list(run_times, names = "strict", any.missing = FALSE)
  testthat::expect_named(run_times, c("total", "chains"))
  checkmate::expect_number(run_times$total, finite = TRUE)
  checkmate::expect_data_frame(
    run_times$chains,
    any.missing = FALSE,
    types = c("integer", "numeric"),
    nrows = fit_mcmc$runset$num_runs(),
    ncols = 4
  )

  # after refresh=0 warmup and sampling times should be NA
  testthat::expect_warning(
    run_times_0 <- fit_mcmc_0$time(),
    "Separate warmup and sampling times are not available"
  )
  checkmate::expect_number(run_times_0$total, finite = TRUE)
  checkmate::expect_data_frame(run_times_0$chains,
                               any.missing = TRUE,
                               types = c("integer", "numeric"),
                               nrows = fit_mcmc_0$runset$num_runs(),
                               ncols = 4)
  for (j in 1:nrow(run_times_0$chains)) {
    checkmate::expect_scalar_na(run_times_0$chains$warmup[j])
    checkmate::expect_scalar_na(run_times_0$chains$sampling[j])
  }
})

test_that("inc_warmup in draws() works", {
  skip_on_cran()
  x0 <- fit_mcmc_0$draws(inc_warmup = FALSE)
  x1 <- fit_mcmc_1$draws(inc_warmup = FALSE)
  x2 <- fit_mcmc_1$draws(inc_warmup = TRUE)
  expect_equal(dim(x0), c(1000, 2, 5))
  expect_error(fit_mcmc_0$draws(inc_warmup = TRUE),
               "Warmup draws were requested from a fit object without them! Please restart the sampling with save_warmup = TRUE.")
  expect_equal(dim(x1), c(1000, 2, 5))
  expect_equal(dim(x2), c(2000, 2, 5))
  y0 <- fit_mcmc_0$sampler_diagnostics(inc_warmup = FALSE)
  y1 <- fit_mcmc_1$sampler_diagnostics(inc_warmup = FALSE)
  y2 <- fit_mcmc_1$sampler_diagnostics(inc_warmup = TRUE)
  expect_equal(dim(y0), c(1000, 2, 6))
  expect_error(fit_mcmc_0$sampler_diagnostics(inc_warmup = TRUE),
               "Warmup sampler diagnostics were requested from a fit object without them! Please restart the sampling with save_warmup = TRUE.")
  expect_equal(dim(y1), c(1000, 2, 6))
  expect_equal(dim(y2), c(2000, 2, 6))

})
