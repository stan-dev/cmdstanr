# Setup -------------------------------------------------------------------
NOT_CRAN <-
  identical(Sys.getenv("NOT_CRAN"), "true") ||
  identical(Sys.getenv("TRAVIS"), "true")

if (NOT_CRAN) {
  set_cmdstan_path()
  stan_program <- test_path("resources/stan/logistic.stan")
  # data_file_r <- test_path("resources/data/logistic.data.R")
  data_file_json <- test_path("resources/data/logistic.data.json")
  utils::capture.output(mod <- cmdstan_model(stan_file = stan_program))
  utils::capture.output(fit_mcmc <- mod$sample(data = data_file_json,
                                               num_chains = 2,
                                               save_diagnostics = TRUE))
  utils::capture.output(suppressWarnings(
    fit_mle <- mod$optimize(data = data_file_json, save_diagnostics = FALSE)
  ))
  utils::capture.output(suppressWarnings(
    fit_vb <- mod$variational(data = data_file_json, save_diagnostics = TRUE)
  ))

  PARAM_NAMES <- c("alpha", "beta[1]", "beta[2]", "beta[3]")

  # cleanup
  file.remove(paste0(mod$exe_file(), c("", ".o",".hpp")))
}


# CmdStanMCMC -------------------------------------------------------------
context("CmdStanMCMC")

test_that("saving csv mcmc output works", {
  skip_on_cran()
  checkmate::expect_file_exists(fit_mcmc$output_files(), extension = "csv")

  paths <- fit_mcmc$save_output_files(tempdir(), basename = "testing-mcmc-output")
  expect_true(all(file.exists(paths)))
  checkmate::expect_file_exists(paths, extension = "csv")
  expect_match(paths[1], "testing-mcmc-output-1")
  expect_match(paths[2], "testing-mcmc-output-2")
})

test_that("saving data file after mcmc works", {
  skip_on_cran()
  checkmate::expect_file_exists(fit_mcmc$data_file(), extension = "json")

  path <- fit_mcmc$save_data_file(tempdir(), basename = "testing-mcmc-data")
  checkmate::expect_file_exists(path, extension = "json")
  expect_match(path, "testing-mcmc-data_")
})

test_that("saving diagnostic csv mcmc output works", {
  skip_on_cran()
  checkmate::expect_file_exists(fit_mcmc$diagnostic_files(), extension = "csv")

  paths <- fit_mcmc$save_diagnostic_files(tempdir(), basename = "testing-mcmc")
  expect_true(all(file.exists(paths)))
  expect_true(all(file.size(paths) > 0))
  checkmate::expect_file_exists(paths, extension = "csv")
  expect_match(paths[1], "testing-mcmc-diagnostic-1")
  expect_match(paths[2], "testing-mcmc-diagnostic-2")
})


test_that("summary() method succesfully calls bin/stansummary", {
  skip_on_cran()
  expect_output(
    fit_mcmc$summary(),
    "Inference for Stan model: logistic_model",
    fixed = TRUE
  )
})

test_that("diagnose() method succesfully calls bin/diagnose", {
  skip_on_cran()
  expect_output(fit_mcmc$diagnose(), "Checking sampler transitions for divergences")
})

test_that("draws() method returns posterior sample (reading csv works)", {
  skip_on_cran()
  draws <- fit_mcmc$draws()
  expect_type(draws, "double")
  expect_true(is.array(draws))
  expect_true(length(dim(draws)) == 3)
  expect_equal(dimnames(draws)[[3]], c(PARAM_NAMES, "lp__"))
})


# CmdStanMLE --------------------------------------------------------------
context("CmdStanMLE")

test_that("saving csv optimzation output works", {
  skip_on_cran()
  checkmate::expect_file_exists(fit_mle$output_files(), extension = "csv")

  path <- fit_mle$save_output_files(tempdir(), basename = "testing-mle-output")
  expect_length(path, 1)
  checkmate::expect_file_exists(path, extension = "csv")
  expect_match(path, "testing-mle-output-1")
})

test_that("saving data file after optimization works", {
  skip_on_cran()
  checkmate::expect_file_exists(fit_mle$data_file(), extension = "json")

  path <- fit_mle$save_data_file(tempdir(), basename = "testing-mle-data")
  checkmate::expect_file_exists(path, extension = "json")
  expect_match(path, "testing-mle-data_")
})

test_that("saving diagnostics files errors if don't exist", {
  skip_on_cran()
  expect_error(fit_mle$save_diagnostic_files(), "No diagnostic files found")
  expect_error(fit_mle$diagnostic_files(), "No diagnostic files found")
})

test_that("reading in csv optimization output works", {
  skip_on_cran()
  expect_named(fit_mle$mle(), PARAM_NAMES)
  expect_named(fit_mle$lp(), "lp__")
})

test_that("summary method doesn't error for optimization", {
  skip_on_cran()
  # summary method for optimization isn't fully designed yet
  expect_output(fit_mle$summary(), "Estimates from optimization")
})

# CmdStanVB -------------------------------------------------------------
context("CmdStanVB")

test_that("saving csv variational output works", {
  skip_on_cran()
  checkmate::expect_file_exists(fit_vb$output_files(), extension = "csv")

  path <- fit_vb$save_output_files(tempdir(), basename = "testing-vb-output")
  expect_length(path, 1)
  checkmate::expect_file_exists(path, extension = "csv")
  expect_match(path, "testing-vb-output-1")
})

test_that("saving data file after variational works", {
  skip_on_cran()
  checkmate::expect_file_exists(fit_vb$data_file(), extension = "json")

  path <- fit_vb$save_data_file(tempdir(), basename = "testing-vb-data")
  checkmate::expect_file_exists(path, extension = "json")
  expect_match(path, "testing-vb-data_")
})

test_that("saving diagnostic csv from variational works", {
  skip_on_cran()
  checkmate::expect_file_exists(fit_vb$diagnostic_files(), extension = "csv")

  paths <- fit_vb$save_diagnostic_files(tempdir(), basename = "testing-vb")
  expect_true(all(file.exists(paths)))
  expect_true(all(file.size(paths) > 0))
  checkmate::expect_file_exists(paths, extension = "csv")
  expect_match(paths[1], "testing-vb-diagnostic-1")
})

test_that("summary() method after variation succesfully calls bin/stansummary", {
  skip_on_cran()
  expect_output(fit_vb$summary(), "Inference for Stan model")
})

test_that("draws() method returns approx posterior sample (reading csv works)", {
  skip_on_cran()
  draws <- fit_vb$draws()
  expect_type(draws, "double")
  expect_true(is.matrix(draws))
  expect_equal(colnames(draws), PARAM_NAMES)
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



# RunSet ------------------------------------------------------------------

test_that("RunSet methods return valid output", {
  skip_on_cran()

  runset <- fit_mcmc$runset
  checkmate::expect_r6(runset$args(), "CmdStanArgs")
  checkmate::expect_list(
    runset$command_args(),
    types = "character",
    len = runset$num_runs(),
    unique = TRUE
  )
  expect_equal(runset$model_name(), "logistic")
  expect_equal(runset$method(), "sample")
  expect_equal(runset$run_ids(), seq_len(runset$num_runs()))
  expect_equal(runset$run_ids(), seq_len(runset$num_chains()))
  checkmate::expect_file_exists(runset$data_file())
  checkmate::expect_file_exists(runset$output_files())
  checkmate::expect_file_exists(runset$console_files())
  checkmate::expect_file_exists(runset$diagnostic_files())
})

