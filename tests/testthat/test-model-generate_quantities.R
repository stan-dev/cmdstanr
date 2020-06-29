context("model-generate-quantities")

# Setup -------------------------------------------------------------------
if (not_on_cran()) {
  set_cmdstan_path()
  fit <- testing_fit("bernoulli", method = "sample", seed = 123)
  mod_gq <- testing_model("bernoulli_ppc")
  data_list <- testing_data("bernoulli")

  # these are all valid for generate_quantities()
  ok_arg_values <- list(
    fitted_params = fit,
    data = data_list,
    refresh = 5,
    seed = 12345,
    parallel_chains = 1
  )

  # using any of these should cause optimize() to error
  bad_arg_values <- list(
    fitter_params = "NOT_A_FILE",
    data = "NOT_A_FILE",
    refresh = -20,
    seed = "NOT_A_SEED",
    parallel_chains = -20
  )
}


test_that("generate_quantities() method runs when all arguments specified validly", {
  skip_on_cran()

  # specifying all arguments validly
  expect_gq_output(fit1 <- do.call(mod_gq$generate_quantities, ok_arg_values))
  expect_is(fit1, "CmdStanGQ")

  # leaving all at default (except 'data')
  expect_gq_output(fit2 <- mod_gq$generate_quantities(fitted_params = fit, data = data_list))
  expect_is(fit2, "CmdStanGQ")
})

test_that("generate_quantities() method errors for any invalid argument before calling cmdstan", {
  skip_on_cran()

  for (nm in names(bad_arg_values)) {
    args <- ok_arg_values
    args[[nm]] <- bad_arg_values[[nm]]
    expect_error(do.call(mod_gq$generate_quantities, args), regexp = nm)
  }
})
