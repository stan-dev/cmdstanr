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
  tmp <- tempdir()
  fit_mcmc$save_output_files(tmp, basename = "output-testing")
  expect_true(file.exists(file.path(tmp, "output-testing-1.csv")))
  expect_true(file.exists(file.path(tmp, "output-testing-2.csv")))
})

test_that("saving data file works", {
  skip_on_cran()
  tmp <- tempdir()
  fit_mcmc$save_data_file(tmp, basename = "data-testing")
  expect_true(file.exists(file.path(tmp, "data-testing.data.R")))
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

test_that("reading in csv optimization output works", {
  skip_on_cran()
  expect_named(fit_mle$mle(), "theta")
  expect_named(fit_mle$lp(), "lp__")
})

test_that("saving csv optimzation output works", {
  skip_on_cran()
  tmp <- tempdir()
  fit_mle$save_output_files(tmp, basename = "optim-output-testing")
  expect_true(file.exists(file.path(tmp, "optim-output-testing-1.csv")))
})

test_that("saving data file works", {
  skip_on_cran()
  tmp <- tempdir()
  fit_mle$save_data_file(tmp, basename = "optim-data-testing")
  expect_true(file.exists(file.path(tmp, "optim-data-testing.data.R")))
})


# CmdStanVB -------------------------------------------------------------
context("CmdStanVB")

test_that("saving csv vb output works", {
  skip_on_cran()
  tmp <- tempdir()
  fit_vb$save_output_files(tmp, basename = "vb-output-testing")
  expect_true(file.exists(file.path(tmp, "vb-output-testing-1.csv")))
})

test_that("saving data file works", {
  skip_on_cran()
  tmp <- tempdir()
  fit_vb$save_data_file(tmp, basename = "vb-data-testing")
  expect_true(file.exists(file.path(tmp, "vb-data-testing.data.R")))
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
