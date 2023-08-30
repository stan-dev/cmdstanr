context("model-sample")

set_cmdstan_path()
stan_program <- testing_stan_file("bernoulli")
mod <- testing_model("bernoulli")
stan_program_fp <- testing_stan_file("bernoulli_fp")
mod_fp <- testing_model("bernoulli_fp")

# valid ways to supply data
data_list <- testing_data("bernoulli")
data_file_r <- test_path("resources", "data", "bernoulli.data.R")
data_file_json <- test_path("resources", "data", "bernoulli.data.json")

test_that("Pathfinder Runs", {
  fit <- mod$pathfinder(data=data_list, seed=1234, refresh = 1, sig_figs = 15, tol_obj = 0, tol_rel_grad = 0)
  fit$summary()
  fit_samp = mod$sample(data = data_list)
  fit_samp$summary()
})
