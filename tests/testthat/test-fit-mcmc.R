context("fitted-mcmc")

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
                          seed = 1234, chains = 1,
                          iter_sampling = 10000,
                          refresh = 0, metric = "dense_e")
fit_mcmc_3 <- testing_fit("logistic", method = "sample",
                          seed = 1234, chains = 1,
                          iter_warmup = 100,
                          iter_sampling = 0,
                          save_warmup = 1,
                          refresh = 0, metric = "dense_e")
PARAM_NAMES <- c("alpha", "beta[1]", "beta[2]", "beta[3]")

test_that("draws() stops for unkown variables", {
  expect_error(
    draws_betas <- fit_mcmc$draws(variables = "ABCD"),
    "Can't find the following variable(s) in the output: ABCD",
    fixed = TRUE
  )
  fit_mcmc$draws()
  expect_error(
    draws_betas <- fit_mcmc$draws(variables = c("ABCD", "EFGH")),
    "Can't find the following variable(s) in the output: ABCD, EFGH",
    fixed = TRUE
  )
})

test_that("draws() works when gradually adding variables", {
  fit <- testing_fit("logistic", method = "sample", refresh = 0,
                     save_warmup = TRUE)

  draws_lp__ <- fit$draws(variables = c("lp__"), inc_warmup = TRUE)
  sampler_diagnostics <- fit$sampler_diagnostics(inc_warmup = TRUE)
  expect_type(draws_lp__, "double")
  expect_s3_class(draws_lp__, "draws_array")
  expect_equal(posterior::variables(draws_lp__), c("lp__"))
  expect_type(sampler_diagnostics, "double")
  expect_s3_class(sampler_diagnostics, "draws_array")
  expect_equal(posterior::variables(sampler_diagnostics), c(c("treedepth__", "divergent__", "energy__", "accept_stat__", "stepsize__", "n_leapfrog__")))
  draws_alpha <- fit$draws(variables = c("alpha"), inc_warmup = TRUE)
  expect_type(draws_alpha, "double")
  expect_s3_class(draws_alpha, "draws_array")
  expect_equal(posterior::variables(draws_alpha), c("alpha"))
  draws_beta <- fit$draws(variables = c("beta"), inc_warmup = TRUE)
  expect_type(draws_beta, "double")
  expect_s3_class(draws_beta, "draws_array")
  expect_equal(posterior::variables(draws_beta), c("beta[1]", "beta[2]", "beta[3]"))
})

test_that("draws() method returns draws_array (reading csv works)", {
  draws <- fit_mcmc$draws()
  draws_betas <- fit_mcmc$draws(variables = "beta")
  draws_beta <- fit_mcmc$draws(variables = "beta[1]")
  draws_alpha_beta <- fit_mcmc$draws(variables = c("alpha", "beta"))
  draws_beta_alpha <- fit_mcmc$draws(variables = c("beta", "alpha"))
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

  # check the order of the draws
  expect_equal(posterior::variables(draws_alpha_beta), c("alpha", "beta[1]", "beta[2]", "beta[3]"))
  expect_equal(posterior::variables(draws_beta_alpha), c("beta[1]", "beta[2]", "beta[3]", "alpha"))
})

test_that("inv_metric() method works after mcmc", {
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
  expect_output(expect_s3_class(fit_mcmc$print(), "CmdStanMCMC"), "variable")
  expect_output(fit_mcmc$print(max_rows = 1), "# showing 1 of 5 rows")
  expect_output(fit_mcmc$print(NULL, c("ess_sd")), "ess_sd")

  # test on model with more parameters
  fit <- cmdstanr_example("schools_ncp")
  expect_output(fit$print(), "showing 10 of 19 rows")
  expect_output(fit$print(max_rows = 2), "showing 2 of 19 rows")
  expect_output(fit$print(max_rows = 19), "theta[8]", fixed=TRUE) # last parameter
  expect_output(fit$print("theta", max_rows = 2), "showing 2 of 8 rows")
  expect_error(
    fit$print(variable = "unknown", max_rows = 20),
    "Can't find the following variable(s): unknown",
    fixed = TRUE
  ) # unknown parameter

  out <- capture.output(fit$print("theta"))
  expect_length(out, 9) # columns names + 8 thetas
  expect_match(out[1], "variable")
  expect_match(out[2], "theta[1]", fixed = TRUE)
  expect_match(out[9], "theta[8]", fixed = TRUE)
  expect_false(any(grepl("mu|tau|theta_raw", out)))

  # make sure the row order is correct
  out <- capture.output(fit$print(c("theta[1]", "tau", "mu", "theta_raw[3]")))
  expect_length(out, 5)
  expect_match(out[1], " variable", out[1])
  expect_match(out[2], " theta[1]", fixed = TRUE)
  expect_match(out[3], " tau")
  expect_match(out[4], " mu")
  expect_match(out[5], " theta_raw[3]", fixed = TRUE)
})

test_that("output() method works after mcmc", {
  checkmate::expect_list(
    fit_mcmc$output(),
    types = "character",
    any.missing = FALSE,
    len = fit_mcmc$runset$num_procs()
  )
  expect_output(fit_mcmc$output(id = 1), "Gradient evaluation took")
})

test_that("time() method works after mcmc", {
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

  run_times_0 <- fit_mcmc_0$time()
  checkmate::expect_number(run_times_0$total, finite = TRUE)
  checkmate::expect_data_frame(run_times_0$chains,
                               any.missing = TRUE,
                               types = c("integer", "numeric"),
                               nrows = fit_mcmc_0$runset$num_procs(),
                               ncols = 4)
  for (j in 1:nrow(run_times_0$chains)) {
    checkmate::expect_number(run_times_0$chains$warmup[j])
    checkmate::expect_number(run_times_0$chains$sampling[j])
  }
  # check that reported times match the times reported in the CSV
  for (j in 1:nrow(run_times_0$chains)) {
    sampling_time <- NULL
    warmup_time <- NULL
    total_time <- NULL
    for (l in readLines(fit_mcmc_0$output_files()[j])) {
      if (grepl("seconds (Sampling)", l, fixed = TRUE)) {
        l <- sub("seconds (Sampling)", "", l, fixed = TRUE)
        l <- trimws(sub("#", "", l, fixed = TRUE))
        sampling_time <- as.double(l)
      }
      if (grepl("seconds (Warm-up)", l, fixed = TRUE)) {
        l <- sub("seconds (Warm-up)", "", l, fixed = TRUE)
        l <- trimws(sub("#  Elapsed Time: ", "", l, fixed = TRUE))
        warmup_time <- as.double(l)
      }
      if (grepl("seconds (Total)", l, fixed = TRUE)) {
        l <- sub("seconds (Total)", "", l, fixed = TRUE)
        l <- trimws(sub("#", "", l, fixed = TRUE))
        total_time <- as.double(l)
      }
    }
    expect_equal(run_times_0$chains$warmup[j], warmup_time)
    expect_equal(run_times_0$chains$sampling[j], sampling_time)
    expect_equal(run_times_0$chains$total[j], total_time)
  }
})

test_that("inc_warmup in draws() works", {
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
  y3 <- fit_mcmc_3$sampler_diagnostics(inc_warmup = TRUE)
  y4 <- fit_mcmc_3$sampler_diagnostics(inc_warmup = FALSE)
  expect_equal(dim(y0), c(1000, 2, 6))
  expect_error(fit_mcmc_0$sampler_diagnostics(inc_warmup = TRUE),
               "Warmup sampler diagnostics were requested from a fit object without them!")
  expect_equal(dim(y1), c(1000, 2, 6))
  expect_equal(dim(y2), c(2000, 2, 6))
  expect_equal(dim(y3), c(100, 1, 6))
  expect_equal(dim(y4), NULL)
})

test_that("inc_warmup in draws() works", {
  x3 <- fit_mcmc_2$draws(inc_warmup = FALSE)
  expect_equal(dim(x3), c(10000, 1, 5))
  expect_error(fit_mcmc_2$draws(inc_warmup = TRUE),
               "Warmup draws were requested from a fit object without them! Please rerun the model with save_warmup = TRUE.")
  y3 <- fit_mcmc_2$sampler_diagnostics(inc_warmup = FALSE)
  expect_equal(dim(y3), c(10000, 1, 6))
})

test_that("output() shows informational messages depening on show_messages", {
  fit_info_msg <- testing_fit("info_message")
  expect_output(
    fit_info_msg$output(1),
    "Informational Message: The current Metropolis proposal is about to be rejected"
  )
  fit_info_msg <- testing_fit("info_message", show_messages = FALSE)
  expect_output(
    fit_info_msg$output(1),
    "Informational Message: The current Metropolis proposal is about to be rejected"
  )
})

test_that("loo method works if log_lik is available", {
  skip_if_not_installed("loo")
  fit_bernoulli <- testing_fit("bernoulli_log_lik")
  expect_s3_class(suppressWarnings(fit_bernoulli$loo(cores = 1, save_psis = TRUE)), "loo")
  expect_s3_class(suppressWarnings(fit_bernoulli$loo(r_eff = FALSE)), "loo")
})

test_that("loo errors if it can't find log lik variables", {
  skip_if_not_installed("loo")
  fit_schools <- testing_fit("schools")
  expect_error(
    fit_schools$loo(),
    "Can't find the following variable(s) in the output: log_lik",
    fixed = TRUE
  )
})

test_that("loo works for all draws storage formats", {
  skip_if_not_installed("loo")
  fit <- testing_fit("bernoulli_log_lik")

  options(cmdstanr_draws_format = "draws_array")
  expect_s3_class(suppressWarnings(fit$loo()), "loo")

  options(cmdstanr_draws_format = "draws_df")
  expect_s3_class(suppressWarnings(fit$loo()), "loo")

  options(cmdstanr_draws_format = "draws_matrix")
  expect_s3_class(suppressWarnings(fit$loo()), "loo")

  options(cmdstanr_draws_format = "draws_list")
  expect_s3_class(suppressWarnings(fit$loo()), "loo")

  options(cmdstanr_draws_format = "draws_rvars")
  expect_s3_class(suppressWarnings(fit$loo()), "loo")

  # reset option
  options(cmdstanr_draws_format = NULL)
})

test_that("draws() works for different formats", {
  a <- fit_mcmc$draws()
  expect_true(posterior::is_draws_array(a))
  a <- fit_mcmc$draws(format = "list")
  expect_true(posterior::is_draws_list(a))
  a <- fit_mcmc$draws(format = "array")
  expect_true(posterior::is_draws_array(a))
  a <- fit_mcmc$draws(format = "df")
  expect_true(posterior::is_draws_df(a))
})

test_that("draws() errors if invalid format", {
  expect_error(
    fit_mcmc$draws(format = "bad_format"),
    "The supplied draws format is not valid"
  )
})

test_that("diagnostic_summary() works", {
  # will have divergences and treedepth problems
  fit <- suppressMessages(testing_fit("schools", max_treedepth = 3, seed = 123))

  expect_message(
    diagnostics <- fit$diagnostic_summary(),
    "transitions ended with a divergence"
  )
  expect_equal(
    diagnostics$num_divergent,
    suppressMessages(check_divergences(fit$sampler_diagnostics()))
  )
  expect_message(
    diagnostics <- fit$diagnostic_summary(),
    "transitions hit the maximum treedepth limit of 3"
  )
  expect_equal(
    diagnostics$num_max_treedepth,
    suppressMessages(check_max_treedepth(fit$sampler_diagnostics(), fit$metadata()))
  )
  expect_equal(
    diagnostics$ebfmi,
    suppressMessages(check_ebfmi(fit$sampler_diagnostics()))
  )

  # ebfmi not defined if iter < 3
  fit <- suppressWarnings(suppressMessages(testing_fit("schools", iter_sampling = 2)))
  expect_warning(
    diagnostics <- fit$diagnostic_summary(),
    "E-BFMI not computed"
  )
  expect_equal(diagnostics$ebfmi, NA)

  expect_equal(fit$diagnostic_summary(""), list())
  expect_equal(fit$diagnostic_summary(NULL), list())
})

test_that("metadata()$time has chains rowss", {
  expect_equal(nrow(fit_mcmc$metadata()$time), fit_mcmc$num_chains())
  expect_equal(nrow(fit_mcmc_0$metadata()$time), fit_mcmc_0$num_chains())
  expect_equal(nrow(fit_mcmc_1$metadata()$time), fit_mcmc_1$num_chains())
  expect_equal(nrow(fit_mcmc_2$metadata()$time), fit_mcmc_2$num_chains())
  expect_equal(nrow(fit_mcmc_3$metadata()$time), fit_mcmc_3$num_chains())
})
