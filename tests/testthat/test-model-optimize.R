context("model-optimize")

# Setup -------------------------------------------------------------------
if (not_on_cran()) {
  set_cmdstan_path()
  mod <- cmdstan_model(stan_file = beroulli_example_file())
  data_list <- bernoulli_example_data()

  # these are all valid for optimize()
  ok_arg_values <- list(
    data = data_list,
    refresh = 5,
    init = NULL,
    seed = 12345,
    algorithm = "lbfgs",
    iter = 100,
    init_alpha = 0.002,
    save_diagnostics = FALSE
  )

  # using any of these should cause optimize() to error
  bad_arg_values <- list(
    data = "NOT_A_FILE",
    refresh = -20,
    init = "NOT_A_FILE",
    seed = "NOT_A_SEED",
    algorithm = "NOT_AN_ALGORITHM",
    iter = -20,
    init_alpha = -20,
    save_diagnostics = "NOT_LOGICAL"
  )
}


test_that("optimize() method runs when all arguments specified validly", {
  skip_on_cran()

  # specifying all arguments validly
  expect_optim_output(fit1 <- do.call(mod$optimize, ok_arg_values))
  expect_is(fit1, "CmdStanMLE")

  # leaving all at default (except 'data')
  expect_optim_output(fit2 <- mod$optimize(data = data_list))
  expect_is(fit2, "CmdStanMLE")
})

test_that("optimize() method errors for any invalid argument before calling cmdstan", {
  skip_on_cran()

  for (nm in names(bad_arg_values)) {
    args <- ok_arg_values
    args[[nm]] <- bad_arg_values[[nm]]
    expect_error(
      expect_experimental_warning(do.call(mod$optimize, args)),
      regexp = nm
    )
  }
})

test_that("optimize() errors when combining 'newton' with 'init_alpha'", {
  skip_on_cran()
  expect_error(
    expect_experimental_warning(
      mod$optimize(data = data_list, algorithm = "newton", init_alpha = 0.1)
    ),
    "'init_alpha' can't be used when algorithm is 'newton'"
  )
})
