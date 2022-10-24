context("model-optimize")

set_cmdstan_path()
mod <- testing_model("bernoulli")
data_list <- testing_data("bernoulli")

# these are all valid for optimize()
ok_arg_values <- list(
  data = data_list,
  refresh = 5,
  init = NULL,
  seed = 12345,
  algorithm = "lbfgs",
  iter = 100,
  init_alpha = 0.002,
  save_latent_dynamics = FALSE
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
  save_latent_dynamics = "NOT_LOGICAL"
)

ok_arg_sci_nota_values <- list(
  data = data_list,
  refresh = 5,
  init = NULL,
  seed = 12345,
  algorithm = "lbfgs",
  iter = 100000,
  init_alpha = 0.002,
  save_latent_dynamics = FALSE
)


test_that("optimize() method runs when all arguments specified validly", {
  # specifying all arguments validly
  expect_optim_output(fit1 <- do.call(mod$optimize, ok_arg_values))
  expect_is(fit1, "CmdStanMLE")

  # leaving all at default (except 'data')
  expect_optim_output(fit2 <- mod$optimize(data = data_list, seed = 123))
  expect_is(fit2, "CmdStanMLE")
})

test_that("optimize() method runs when arguments are specified in scientific notation", {
  # specifying all arguments validly
  expect_optim_output(fit1 <- do.call(mod$optimize, ok_arg_sci_nota_values))
  expect_is(fit1, "CmdStanMLE")
})

test_that("optimize() warns if threads specified but not enabled", {
  expect_warning(
    expect_optim_output(fit <- mod$optimize(data = data_list, threads = 2,
                                            seed = 123)),
    "'threads' will have no effect"
  )
})

test_that("optimize() method errors for any invalid argument before calling cmdstan", {
  for (nm in names(bad_arg_values)) {
    args <- ok_arg_values
    args[[nm]] <- bad_arg_values[[nm]]
    expect_error(do.call(mod$optimize, args), regexp = nm)
  }
})

test_that("optimize() errors with bad combination of arguments", {
  # check a few examples (if these errors are correct then they will be correct
  # for all similar args because of how it's implemented)
  expect_error(
    mod$optimize(data = data_list, algorithm = "newton", tol_grad = 0.1),
    "'tol_grad' can't be used when algorithm is 'newton'"
  )
  expect_error(
    mod$optimize(data = data_list, algorithm = "bfgs", tol_obj = -10),
    "not >= 0"
  )
  expect_error(
    mod$optimize(data = data_list, init_alpha = 0.1),
    "Please specify 'algorithm' in order to use 'init_alpha'"
  )

  # history size only allowed with lbfgs and must be positive integer
  expect_error(
    mod$optimize(data = data_list, history_size = 1),
    "'history_size' is only allowed if 'algorithm' is specified as 'lbfgs'"
  )
  expect_error(
    mod$optimize(data = data_list, algorithm = "bfgs", history_size = 1),
    "'history_size' is only allowed if 'algorithm' is specified as 'lbfgs'"
  )
  expect_error(
    mod$optimize(data = data_list, algorithm = "lbfgs", history_size = 1.5),
    "Must be of type 'integerish'"
  )
  expect_error(
    mod$optimize(data = data_list, algorithm = "lbfgs", history_size = -1),
    "not >= 1"
  )
})

test_that("optimize() works with (L-)BFGS tolerances specified", {
  expect_optim_output(
    fit <- mod$optimize(
      data = data_list,
      algorithm = "lbfgs",
      # using values that aren't the defaults
      init_alpha = 0.002,
      tol_obj = 2e-11,
      tol_rel_obj = 10001,
      tol_grad = 5e-07,
      tol_rel_grad = 10000001,
      tol_param = 5e-07,
      history_size = 6,
      seed = 123
    )
  )
  metadata <- fit$metadata()
  expect_equal(metadata$init_alpha, 0.002)
  expect_equal(metadata$tol_obj, 2e-11)
  expect_equal(metadata$tol_rel_obj, 10001)
  expect_equal(metadata$tol_grad, 5e-07)
  expect_equal(metadata$tol_rel_grad, 10000001)
  expect_equal(metadata$tol_param, 5e-07)
  expect_equal(metadata$history_size, 6)
})

test_that("optimize() method runs when the stan file is removed", {
  stan_file_tmp <- tempfile(pattern = "tmp", fileext = ".stan")
  file.copy(testing_stan_file("bernoulli"), stan_file_tmp)
  mod_tmp <- cmdstan_model(stan_file_tmp)
  file.remove(stan_file_tmp)
  expect_optim_output(
    mod_tmp$optimize(data = data_list)
  )
})
