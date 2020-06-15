context("cmdstanr_example")

test_that("cmdstanr_example works", {
  skip_on_cran()
  fit_mcmc <- cmdstanr_example("logistic", chains = 2)
  checkmate::expect_r6(fit_mcmc, "CmdStanMCMC")
  expect_equal(fit_mcmc$num_chains(), 2)

  fit_mle <- cmdstanr_example("logistic", method = "optimize")
  checkmate::expect_r6(fit_mle, "CmdStanMLE")

  fit_vb <- cmdstanr_example("logistic", method = "variational")
  checkmate::expect_r6(fit_vb, "CmdStanVB")
})
