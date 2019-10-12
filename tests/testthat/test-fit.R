# Setup -------------------------------------------------------------------
NOT_CRAN <-
  identical(Sys.getenv("NOT_CRAN"), "true") ||
  identical(Sys.getenv("TRAVIS"), "true")

if (NOT_CRAN) {
  set_cmdstan_path()
  stan_program <- file.path(cmdstan_path(), "examples/bernoulli/bernoulli.stan")
  data_list <- list(N = 10, y = c(0,1,0,0,0,0,0,0,0,1))
  mod <- cmdstan_model(stan_file = stan_program)
  capture.output(mod$compile())
  capture.output(fit_mcmc <- mod$sample(data = data_list, num_chains = 2))
  capture.output(suppressWarnings(
    fit_mle <- mod$optimize(data = data_list)
  ))
  capture.output(suppressWarnings(
    fit_vb <- mod$variational(data = data_list)
  ))
}


# CmdStanMCMC -------------------------------------------------------------
context("CmdStanMCMC")

test_that("saving csv mcmc output works", {
  skip_on_cran()
  paths <- fit_mcmc$save_output_files(tempdir(), basename = "testing-mcmc-output")
  expect_true(all(file.exists(paths)))
  expect_match(paths[1], "testing-mcmc-output-1")
  expect_match(paths[2], "testing-mcmc-output-2")
  expect_match(paths, ".csv$")
})

test_that("saving data file after mcmc works", {
  skip_on_cran()
  path <- fit_mcmc$save_data_file(tempdir(), basename = "testing-mcmc-data")
  expect_true(file.exists(path))
  expect_match(path, "testing-mcmc-data_")
  expect_match(path, ".data.R$")
})

test_that("summary() method succesfully calls bin/stansummary", {
  skip_on_cran()
  expect_output(fit_mcmc$summary(), "Inference for Stan model")
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
})


# CmdStanMLE --------------------------------------------------------------
context("CmdStanMLE")

test_that("saving csv optimzation output works", {
  skip_on_cran()
  path <- fit_mle$save_output_files(tempdir(), basename = "testing-mle-output")
  expect_length(path, 1)
  expect_true(file.exists(path))
  expect_match(path, "testing-mle-output-1")
  expect_match(path, ".csv$")
})

test_that("saving data file after optimization works", {
  skip_on_cran()
  path <- fit_mle$save_data_file(tempdir(), basename = "testing-mle-data")
  expect_true(file.exists(path))
  expect_match(path, "testing-mle-data_")
  expect_match(path, ".data.R$")
})

test_that("reading in csv optimization output works", {
  skip_on_cran()
  expect_named(fit_mle$mle(), "theta")
  expect_named(fit_mle$lp(), "lp__")
})


# CmdStanVB -------------------------------------------------------------
context("CmdStanVB")

test_that("saving csv variational output works", {
  skip_on_cran()
  path <- fit_vb$save_output_files(tempdir(), basename = "testing-vb-output")
  expect_length(path, 1)
  expect_true(file.exists(path))
  expect_match(path, "testing-vb-output-1")
  expect_match(path, ".csv$")
})

test_that("saving data file after variational works", {
  skip_on_cran()
  path <- fit_vb$save_data_file(tempdir(), basename = "testing-vb-data")
  expect_true(file.exists(path))
  expect_match(path, "testing-vb-data_")
  expect_match(path, ".data.R$")
})

test_that("summary() method succesfully calls bin/stansummary", {
  skip_on_cran()
  expect_output(fit_vb$summary(), "Inference for Stan model")
})

test_that("draws() method returns approx posterior sample (reading csv works)", {
  skip_on_cran()
  draws <- fit_vb$draws()
  expect_type(draws, "double")
  expect_true(is.matrix(draws))
  expect_equal(colnames(draws), "theta")
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
    runset$commands(),
    types = "character",
    len = runset$num_runs(),
    unique = TRUE
  )
  checkmate::expect_file_exists(runset$data_file())
  checkmate::expect_file_exists(runset$output_files())
  checkmate::expect_file_exists(runset$console_files())
  expect_equal(runset$run_ids(), seq_len(runset$num_runs()))
  expect_equal(runset$run_ids(), seq_len(runset$num_chains()))
  expect_equal(runset$model_name(), "bernoulli")
})

