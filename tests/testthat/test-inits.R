test_that("generate inits works", {
  fit_mcmc <- cmdstanr_example("logistic", chains = 2)
  inits1 <- generate_inits(fit_mcmc)
  inits2 <- generate_inits(fit_mcmc, draws = "last")
  inits3 <- generate_inits(fit_mcmc, FUN = median)
  inits4 <- generate_inits(fit_mcmc, FUN = quantile, probs = 0.5)

  draws <- fit_mcmc$draws()
  inits5 <- generate_inits(draws)
  inits6 <- generate_inits(draws, variables = c('beta'))

  files <- fit_mcmc$output_files()
  inits7 <- generate_inits(files)

  expect_length(inits1, 2)
  expect_length(inits2, 2)
  expect_length(inits3, 2)
  expect_length(inits4, 2)
  expect_length(inits5, 2)
  expect_length(inits6, 2)
  expect_length(inits7, 2)

  expect_equal(names(inits1[[1]]), c('alpha','beta'))
  expect_equal(names(inits2[[1]]), c('alpha','beta'))
  expect_equal(names(inits3[[1]]), c('alpha','beta'))
  expect_equal(names(inits4[[1]]), c('alpha','beta'))
  expect_equal(names(inits5[[1]]), c('lp__','alpha','beta','log_lik'))
  expect_equal(names(inits6[[1]]), c('beta'))
  expect_equal(names(inits5[[1]]), c('lp__','alpha','beta','log_lik'))

  dims <- variable_dims(fit_mcmc$metadata()$variables)
  expect_equal(length(inits5[[1]]$alpha), dims$alpha)
  expect_equal(length(inits5[[1]]$beta), dims$beta)
})
