context("model-variational")

if (not_on_cran()) {
  set_cmdstan_path()
  mod <- cmdstan_model(stan_file = beroulli_example_file())
  data_list <- bernoulli_example_data()

  # these are all valid for variational()
  ok_arg_values <- list(
    data = data_list,
    refresh = 5,
    init = 0,
    seed = 12345,
    algorithm = "meanfield",
    iter = 10000,
    grad_samples = 2,
    elbo_samples = 101,
    eta = 1.5,
    adapt_engaged = TRUE,
    adapt_iter = 51,
    tol_rel_obj = 0.011,
    eval_elbo = 101,
    output_samples = 10,
    save_diagnostics = FALSE
  )

  # using any one of these should cause variational() to error
  bad_arg_values <- list(
    data = "NOT_A_FILE",
    refresh = -10,
    init = -10,
    seed = "NOT_A_SEED",
    algorithm = "NOT_AN_ALGORITHM",
    iter = -10,
    grad_samples = -10,
    elbo_samples = -10,
    eta = -1.5,
    adapt_engaged = "NOT_VALID",
    adapt_iter = -10,
    tol_rel_obj = -0.5,
    eval_elbo = -10,
    output_samples = -10,
    save_diagnostics = "NOT_LOGICAL"
  )
}

test_that("variational() method runs when all arguments specified validly", {
  skip_on_cran()

  # specifying all arguments validly
  expect_vb_output(fit1 <- do.call(mod$variational, ok_arg_values))
  expect_is(fit1, "CmdStanVB")

  # leaving all at default (except 'data')
  expect_vb_output(fit2 <- mod$variational(data = data_list))
  expect_is(fit2, "CmdStanVB")
})

test_that("variational() method errors for any invalid argument before calling cmdstan", {
  skip_on_cran()

  for (nm in names(bad_arg_values)) {
    args <- ok_arg_values
    args[[nm]] <- bad_arg_values[[nm]]
    expect_error(
      expect_experimental_warning(do.call(mod$variational, args)),
      regexp = nm
    )
  }
})
