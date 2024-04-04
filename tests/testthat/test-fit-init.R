library(cmdstanr)
set_cmdstan_path()

mod_params <- testing_model("parameter_types")
mod_schools <- testing_model("schools")
mod_logistic <- testing_model("logistic")
mod_logistic_simple <- testing_model("logistic_simple")
data_list_schools <- testing_data("schools")
data_list_logistic <- testing_data("logistic")
test_inits <- function(mod, fit_init, data_list = NULL) {
  utils::capture.output(fit_sample <- mod$sample(data = data_list, chains = 1,
                                                 init = fit_init, iter_sampling = 100, iter_warmup = 100, refresh = 0,
                                                 seed = 1234))
  utils::capture.output(fit_sample <- mod$sample(data = data_list, chains = 5,
                                                 init = fit_init, iter_sampling = 100, iter_warmup = 100, refresh = 0,
                                                 seed = 1234))
  utils::capture.output(fit_vb <- mod$variational(data = data_list, refresh = 0,
                                                  seed = 1234, init = fit_init, algorithm = "fullrank"))
  utils::capture.output(fit_path <- mod$pathfinder(data = data_list, seed=1234,
                                                   refresh = 0, num_paths = 4, init = fit_init))
  utils::capture.output(fit_laplace <- mod$laplace(data = data_list,
                                                   seed = 1234, refresh=0, init=fit_init))
  utils::capture.output(fit_ml <- mod$optimize(data = data_list, seed = 1234,
                                               refresh = 0, init = fit_init, history_size = 400, jacobian = TRUE,
                                               algorithm = "lbfgs", tol_param = 1e-12, tol_rel_grad = 1e-12,
                                               tol_grad = 1e-12, tol_rel_obj = 1e-12, tol_obj = 1e-12, init_alpha = 1e-4,
                                               iter = 400))
  draws = posterior::as_draws_rvars(fit_sample$draws())
  utils::capture.output(fit_sample <- mod$sample(data = data_list, chains = 1,
                                                 init = draws, iter_sampling = 100, iter_warmup = 100, refresh = 0,
                                                 seed = 1234))
  return(0)
}

test_that("Sample method works as init", {
    utils::capture.output(fit_sample_init <- mod_params$sample(chains = 1,
    iter_warmup = 100, iter_sampling = 100, refresh = 0, seed = 1234))
  utils::capture.output(fit_sample_multi_init <- mod_params$sample(chains = 4, init = fit_sample_init,
    iter_warmup = 100, iter_sampling = 100, refresh = 0, seed = 1234))
  expect_no_error(test_inits(mod_params, fit_sample_init))
  expect_no_error(test_inits(mod_params, fit_sample_multi_init))
})

test_that("Subsets of parameters are allowed", {
  utils::capture.output(fit_sample_init_simple <- mod_logistic_simple$sample(chains = 1,
    data = data_list_logistic, iter_warmup = 100, iter_sampling = 100,
    refresh = 0, seed = 1234))
  expect_message(test_inits(mod_logistic, fit_sample_init_simple,
    data_list_logistic))
})


test_that("Pathfinder method works as init", {
  utils::capture.output(fit_path_init <- mod_params$pathfinder(seed=1234,
                                                               refresh = 0, num_paths = 4))
  expect_no_error(test_inits(mod_params, fit_path_init))
  utils::capture.output(fit_path_init <- mod_params$pathfinder(seed=1234,
                                                               refresh = 0, num_paths = 1))
  expect_no_error(test_inits(mod_params, fit_path_init))
})

test_that("Laplace method works as init", {
  utils::capture.output(fit_laplace_init <- mod_logistic$laplace(
    data = data_list_logistic, seed = 1234, refresh=0))
  expect_no_error(test_inits(mod_logistic, fit_laplace_init,
                             data_list_logistic))
})

test_that("Variational method works as init", {
  utils::capture.output(fit_vb_init <- mod_logistic$variational(
    data = data_list_logistic, seed=1234, refresh = 0))
  expect_no_error(test_inits(mod_logistic, fit_vb_init, data_list_logistic))
})

test_that("Optimization method works as init", {
  utils::capture.output(fit_ml_init <- mod_logistic$optimize(
    data = data_list_logistic, seed=1234, refresh = 0))
  expect_no_error(test_inits(mod_logistic, fit_ml_init, data_list_logistic))
})


test_that("Draws Object with NA or Inf throws error", {
  utils::capture.output(fit_laplace_init <- mod_logistic$laplace(
    data = data_list_logistic, seed = 1234, refresh=0))
  draws_df = fit_laplace_init$draws()
  draws_df[1, 3] = NA
  expect_error(mod_logistic$laplace(
    data = data_list_logistic, seed = 1234, refresh=0, init = draws_df[1, ]), "alpha contains NA or Inf values!")
  draws_df[1, 4] = NA
  expect_error(mod_logistic$sample(
    data = data_list_logistic, seed = 1234, refresh=0, init = draws_df[1:4, ]), "alpha, beta contains NA or Inf values!")
  draws_df = fit_laplace_init$draws()
  draws_df[1, 3] = Inf
  expect_error(mod_logistic$sample(
    data = data_list_logistic, seed = 1234, refresh=0, init = draws_df[1:4, ]), "alpha contains NA or Inf values!")
  draws_df[1, 4] = NA
  expect_error(mod_logistic$sample(
    data = data_list_logistic, seed = 1234, refresh=0, init = draws_df[1:4, ]), "alpha, beta contains NA or Inf values!")
})
