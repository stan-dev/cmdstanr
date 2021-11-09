context("model-variational")

set_cmdstan_path()
mod <- testing_model("bernoulli")
data_list <- testing_data("bernoulli")

# these are all valid for variational()
ok_arg_values <- list(
  data = data_list,
  refresh = 5,
  init = 0,
  seed = 123,
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
  save_latent_dynamics = FALSE
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
  save_latent_dynamics = "NOT_LOGICAL"
)


test_that("variational() method runs when all arguments specified validly", {
  # specifying all arguments validly
  expect_vb_output(fit1 <- do.call(mod$variational, ok_arg_values))
  expect_is(fit1, "CmdStanVB")

  # leaving all at default (except data and seed)
  expect_vb_output(fit2 <- mod$variational(data = data_list, seed = 123))
  expect_is(fit2, "CmdStanVB")
})

test_that("variational() warns if threads specified but not enabled", {
  expect_warning(
    expect_vb_output(fit <- mod$variational(data = data_list, threads = 2, seed = 123)),
    "'threads' will have no effect"
  )
})

test_that("variational() method errors for any invalid argument before calling cmdstan", {
  for (nm in names(bad_arg_values)) {
    args <- ok_arg_values
    args[[nm]] <- bad_arg_values[[nm]]
    expect_error(do.call(mod$variational, args), regexp = nm)
  }
})

test_that("variational() method runs when the stan file is removed", {
  stan_file_tmp <- tempfile(pattern = "tmp", fileext = ".stan")
  file.copy(testing_stan_file("bernoulli"), stan_file_tmp)
  mod_tmp <- cmdstan_model(stan_file_tmp)
  file.remove(stan_file_tmp)
  expect_vb_output(
    mod_tmp$variational(data = data_list, seed = 123)
  )
})
