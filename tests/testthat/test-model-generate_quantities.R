context("model-generate-quantities")

set_cmdstan_path()
fit <- testing_fit("bernoulli", method = "sample", seed = 123)
mod_gq <- testing_model("bernoulli_ppc")
data_list <- testing_data("bernoulli")

# these are all valid for generate_quantities()
ok_arg_values <- list(
  fitted_params = fit,
  data = data_list,
  seed = 12345,
  parallel_chains = 1
)

# using any of these should cause optimize() to error
bad_arg_values <- list(
  fitter_params = "NOT_A_FILE",
  data = "NOT_A_FILE",
  seed = "NOT_A_SEED",
  parallel_chains = -20
)


test_that("generate_quantities() method runs when all arguments specified validly", {
  # specifying all arguments validly
  expect_gq_output(fit1 <- do.call(mod_gq$generate_quantities, ok_arg_values))
  expect_is(fit1, "CmdStanGQ")

  # leaving all at default (except 'data')
  expect_gq_output(fit2 <- mod_gq$generate_quantities(fitted_params = fit, data = data_list))
  expect_is(fit2, "CmdStanGQ")
})

test_that("generate_quantities() method errors for any invalid argument before calling cmdstan", {
  for (nm in names(bad_arg_values)) {
    args <- ok_arg_values
    args[[nm]] <- bad_arg_values[[nm]]
    expect_error(do.call(mod_gq$generate_quantities, args), regexp = nm)
  }
})

test_that("generate_quantities work for different chains and parallel_chains", {
  fit_1_chain <- testing_fit("bernoulli", method = "sample", seed = 123, chains = 1)
  fit_gq <- testing_fit("bernoulli_ppc", method = "generate_quantities", seed = 123, fitted_params = fit)
  expect_gq_output(
    mod_gq$generate_quantities(data = data_list, fitted_params = fit_1_chain)
  )
  expect_gq_output(
    mod_gq$generate_quantities(data = data_list, fitted_params = fit, parallel_chains = 2)
  )
  expect_gq_output(
    mod_gq$generate_quantities(data = data_list, fitted_params = fit, parallel_chains = 4)
  )
  mod_gq <- cmdstan_model(testing_stan_file("bernoulli_ppc"), cpp_options = list(stan_threads = TRUE))
  expect_gq_output(
    mod_gq$generate_quantities(data = data_list, fitted_params = fit_1_chain, threads_per_chain = 2)
  )
  expect_output(
    mod_gq$generate_quantities(data = data_list, fitted_params = fit_1_chain, threads_per_chain = 2),
    "2 thread(s) per chain",
    fixed = TRUE
  )
})

test_that("generate_quantities works with draws_array", {
  fit_1_chain <- testing_fit("bernoulli", method = "sample", seed = 123, chains = 1)
  expect_gq_output(
    mod_gq$generate_quantities(data = data_list, fitted_params = fit_1_chain$draws())
  )
  expect_gq_output(
    mod_gq$generate_quantities(data = data_list, fitted_params = fit$draws(), parallel_chains = 2)
  )
  expect_gq_output(
    mod_gq$generate_quantities(data = data_list, fitted_params = fit$draws(), parallel_chains = 4)
  )
})

test_that("generate_quantities works with VB and draws_matrix", {
  fit <- testing_fit("bernoulli", method = "variational", seed = 123)
  expect_gq_output(
    mod_gq$generate_quantities(data = data_list, fitted_params = fit)
  )
  expect_gq_output(
    mod_gq$generate_quantities(data = data_list, fitted_params = fit$draws())
  )
})

test_that("generate_quantities() warns if threads specified but not enabled", {
  expect_warning(
    expect_gq_output(fit_gq <- mod_gq$generate_quantities(data = data_list, fitted_params = fit, threads_per_chain = 4)),
    "'threads_per_chain' will have no effect"
  )
})
