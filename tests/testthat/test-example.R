context("cmdstanr_example")

test_that("cmdstanr_example works", {
  skip_on_cran()
  fit_mcmc <- cmdstanr_example("logistic", chains = 2)
  checkmate::expect_r6(fit_mcmc, "CmdStanMCMC")
  expect_equal(fit_mcmc$num_chains(), 2)

  expect_output(cmdstanr_example("logistic", chains = 2, quiet = FALSE),
                "Running MCMC with 2 sequential chains")

  fit_mle <- cmdstanr_example("logistic", method = "optimize")
  checkmate::expect_r6(fit_mle, "CmdStanMLE")

  fit_vb <- cmdstanr_example("logistic", method = "variational")
  checkmate::expect_r6(fit_vb, "CmdStanVB")

  expect_output(print_example_program("schools"), "vector[J] theta", fixed=TRUE)
  expect_output(print_example_program("schools_ncp"), "vector[J] theta_raw", fixed=TRUE)
})

test_that("write_stan_tempfile works", {
  stan_program <- "
  data {
    int<lower=0> N;
    int<lower=0,upper=1> y[N];
  }
  parameters {
    real<lower=0,upper=1> theta;
  }
  model {
    y ~ bernoulli(theta);
  }
  "

  f1 <- write_stan_tempfile(stan_program)
  checkmate::expect_file_exists(f1, extension = "stan")
  f1_lines <- readLines(f1)

  f2 <- write_stan_tempfile(f1_lines)
  checkmate::expect_file_exists(f2, extension = "stan")
  f2_lines <- readLines(f2)

  expect_identical(f1_lines, f2_lines)
})
