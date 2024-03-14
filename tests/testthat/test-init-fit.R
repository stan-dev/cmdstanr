library(cmdstanr)
set_cmdstan_path("/home/sbronder/open_source/stan/origin/cmdstan")

mod_params <- testing_model("parameter_types")
mod_schools <- testing_model("schools")
mod_logistic <- testing_model("logistic")
data_list_schools <- testing_data("schools")
data_list_logistic <- testing_data("logistic")
test_inits <- function(mod, fit_init, data_list = NULL) {
  fit_sample <- mod$sample(data = data_list, chains = 1, init = fit_init, iter_sampling = 100, iter_warmup = 100, refresh = 0, seed = 1234, threads = 1)
  fit_sample <- mod$sample(data = data_list, chains = 5, init = fit_init, iter_sampling = 100, iter_warmup = 100, refresh = 0, seed = 1234, threads = 1)
  fit_vb <- mod$variational(data = data_list, refresh = 0, seed = 1234, threads = 1, init = fit_init)
  fit_path = mod$pathfinder(data = data_list, seed=1234, refresh = 0, num_paths = 4, threads = 4, init = fit_init)
  fit_laplace = mod$laplace(data = data_list, seed = 1234, refresh=0, init=fit_init)
  fit_ml = mod$optimize(data = data_list, seed = 1234, refresh = 0, init = fit_init, history_size = 400,
                             jacobian = TRUE, algorithm = "lbfgs", tol_param = 1e-12, tol_rel_grad = 1e-12,
                             tol_grad = 1e-12, tol_rel_obj = 1e-12, tol_obj = 1e-12, init_alpha = 1e-4, iter = 400)
  return(0)
}
test_that("Sample method works as init", {
  fit_sample_init <- mod_params$sample(chains = 1, iter_warmup = 100, iter_sampling = 100, refresh = 0, seed = 1234, threads = 1)
  fit_sample_multi_init <- mod_params$sample(chains = 4, iter_warmup = 100, iter_sampling = 100, refresh = 0, seed = 1234, threads = 1)
  test_inits(mod_params, fit_sample_init)
  test_inits(mod_params, fit_sample_multi_init)
})

test_that("Pathfinder method works as init", {
  fit_path_init = mod_params$pathfinder(seed=1234, refresh = 0, num_paths = 4, threads = 4)
  test_inits(mod_params, fit_path_init)
})

test_that("Laplace method works as init", {
  fit_laplace_init = mod_logistic$laplace(data = data_list_logistic, seed = 1234, refresh=0)
  test_inits(mod_logistic, fit_laplace_init, data_list_logistic)
})

test_that("Variational method works as init", {
  fit_vb_init = mod_logistic$variational(data = data_list_logistic, seed=1234, refresh = 0)
  test_inits(mod_logistic, fit_vb_init, data_list_logistic)
})

test_that("Optimization method works as init", {
  fit_ml_init = mod_logistic$optimize(data = data_list_logistic, seed=1234, refresh = 0)
  test_inits(mod_logistic, fit_ml_init, data_list_logistic)
})
