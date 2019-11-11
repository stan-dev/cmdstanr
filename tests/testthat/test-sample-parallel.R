context("model-sample-parallel")

# Setup -------------------------------------------------------------------
NOT_CRAN <- identical(Sys.getenv("NOT_CRAN"), "true")

if (NOT_CRAN) {
  set_cmdstan_path()
  stan_program <- file.path(cmdstan_path(), "examples", "bernoulli", "bernoulli.stan")
  data_list <- list(N = 10, y = c(0,1,0,0,0,0,0,0,0,1))
  mod <- cmdstan_model(stan_file = stan_program)
}

test_that("sampling in parallel works", {
  skip_on_cran()
  expect_output(
    mod$sample(data = data_list, num_chains = 2, num_cores = 2),
    "Running MCMC with 2 chain(s) on 2 core(s)",
    fixed = TRUE
  )

  expect_output(
    mod$sample(data = data_list, num_chains = 2, num_cores = 2),
    "Both chains finished succesfully",
    fixed = TRUE
  )
})

test_that("mc.cores option detected", {
  skip_on_cran()

  options(mc.cores = 3)
  expect_output(
    mod$sample(data = data_list, num_chains = 3),
    "Running MCMC with 3 chain(s) on 3 core(s)",
    fixed = TRUE
  )

  options(mc.cores = NULL)
  expect_output(
    mod$sample(data = data_list, num_chains = 2),
    "Running MCMC with 2 chain(s) on 1 core(s)",
    fixed = TRUE
  )
})

