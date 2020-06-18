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
  fit_mcmc_2 <- testing_fit("logistic", method = "sample",
                            seed = 123, chains = 1,
                            iter_sampling = 100000,
                            refresh = 0, metric = "dense_e")
  PARAM_NAMES <- c("alpha", "beta[1]", "beta[2]", "beta[3]")
}


test_that("draws() stops for unkown variables", {
  skip_on_cran()
  expect_error(
    draws_betas <- fit_mcmc$draws(variables = "ABCD"),
    "Can't find the following variable(s) in the sampling output: ABCD",
    fixed = TRUE
  )
  fit_mcmc$draws()
  expect_error(
    draws_betas <- fit_mcmc$draws(variables = c("ABCD", "EFGH")),
    "Can't find the following variable(s) in the sampling output: ABCD, EFGH",
    fixed = TRUE
  )
})

test_that("draws() method returns draws_array (reading csv works)", {
  skip_on_cran()
  draws <- fit_mcmc$draws()
  draws_betas <- fit_mcmc$draws(variables = "beta")
  draws_beta <- fit_mcmc$draws(variables = "beta[1]")
  draws_alpha_beta <- fit_mcmc$draws(variables = c("alpha", "beta"))
  draws_all_after <- fit_mcmc$draws()
  expect_type(draws, "double")
  expect_s3_class(draws, "draws_array")
  expect_equal(posterior::variables(draws), c("lp__", PARAM_NAMES))
  expect_equal(posterior::nchains(draws), fit_mcmc$num_chains())
  expect_s3_class(draws_betas, "draws_array")
  expect_equal(posterior::nvariables(draws_betas), 3)
  expect_equal(posterior::nchains(draws_betas), fit_mcmc$num_chains())
  expect_s3_class(draws_beta, "draws_array")
  expect_equal(posterior::nvariables(draws_beta), 1)
  expect_equal(posterior::nchains(draws_beta), fit_mcmc$num_chains())
  expect_s3_class(draws_alpha_beta, "draws_array")
  expect_equal(posterior::nvariables(draws_alpha_beta), 4)
  expect_equal(posterior::nchains(draws_alpha_beta), fit_mcmc$num_chains())
  expect_s3_class(draws_all_after, "draws_array")
  expect_equal(posterior::nvariables(draws_all_after), 5)
  expect_equal(posterior::nchains(draws_all_after), fit_mcmc$num_chains())
})

test_that("inv_metric method works after mcmc", {
  skip_on_cran()
  x <- fit_mcmc_1$inv_metric()
  expect_length(x, fit_mcmc_1$num_chains())
  checkmate::expect_matrix(x[[1]])
  checkmate::expect_matrix(x[[2]])
  expect_equal(x[[1]], diag(diag(x[[1]])))

  x <- fit_mcmc_1$inv_metric(matrix=FALSE)
  expect_length(x, fit_mcmc_1$num_chains())
  expect_null(dim(x[[1]]))
  checkmate::expect_numeric(x[[1]])
  checkmate::expect_numeric(x[[2]])

  x <- fit_mcmc_2$inv_metric()
  expect_length(x, fit_mcmc_2$num_chains())
  checkmate::expect_matrix(x[[1]])
  expect_false(x[[1]][1,2] == 0) # dense
})

test_that("summary() method works after mcmc", {
  skip_on_cran()
  x <- fit_mcmc$summary()
  expect_s3_class(x, "draws_summary")
  expect_equal(x$variable, c("lp__", PARAM_NAMES))

  x <- fit_mcmc$summary(NULL, c("rhat", "sd"))
  expect_equal(colnames(x), c("variable", "rhat", "sd"))

  x <- fit_mcmc$summary("lp__", c("median", "mad"))
  expect_equal(x$variable, "lp__")
  expect_equal(colnames(x), c("variable", "median", "mad"))
})

test_that("print() method works after mcmc", {
  skip_on_cran()
  expect_output(expect_s3_class(fit_mcmc$print(), "CmdStanMCMC"), "variable")
  expect_output(fit_mcmc$print(max_rows = 1), "# showing 1 of 5 rows")
  expect_output(fit_mcmc$print(NULL, c("ess_sd")), "ess_sd")
})


test_that("output() method works after mcmc", {
  skip_on_cran()
  checkmate::expect_list(
    fit_mcmc$output(),
    types = "character",
    any.missing = FALSE,
    len = fit_mcmc$runset$num_procs()
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
    nrows = fit_mcmc$runset$num_procs(),
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
                               nrows = fit_mcmc_0$runset$num_procs(),
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
  x2_a <- fit_mcmc_1$draws(inc_warmup = TRUE, variables = c("alpha"))
  x2_b <- fit_mcmc_1$draws(inc_warmup = TRUE, variables = c("beta"))
  x2_after <- fit_mcmc_1$draws(inc_warmup = TRUE)
  expect_equal(dim(x0), c(1000, 2, 5))
  expect_error(fit_mcmc_0$draws(inc_warmup = TRUE),
               "Warmup draws were requested from a fit object without them!")
  expect_equal(dim(x1), c(1000, 2, 5))
  expect_equal(dim(x2), c(2000, 2, 5))
  expect_equal(dim(x2_a), c(2000, 2, 1))
  expect_equal(dim(x2_b), c(2000, 2, 3))
  expect_equal(dim(x2_after), c(2000, 2, 5))
  y0 <- fit_mcmc_0$sampler_diagnostics(inc_warmup = FALSE)
  y1 <- fit_mcmc_1$sampler_diagnostics(inc_warmup = FALSE)
  y2 <- fit_mcmc_1$sampler_diagnostics(inc_warmup = TRUE)
  expect_equal(dim(y0), c(1000, 2, 6))
  expect_error(fit_mcmc_0$sampler_diagnostics(inc_warmup = TRUE),
               "Warmup sampler diagnostics were requested from a fit object without them!")
  expect_equal(dim(y1), c(1000, 2, 6))
  expect_equal(dim(y2), c(2000, 2, 6))
})

test_that("inc_warmup in draws() works", {
  skip_on_cran()
  x3 <- fit_mcmc_2$draws(inc_warmup = FALSE)
  expect_equal(dim(x3), c(100000, 1, 5))
  expect_error(fit_mcmc_2$draws(inc_warmup = TRUE),
               "Warmup draws were requested from a fit object without them! Please rerun the model with save_warmup = TRUE.")
  y3 <- fit_mcmc_2$sampler_diagnostics(inc_warmup = FALSE)
  expect_equal(dim(y3), c(100000, 1, 6))
})
