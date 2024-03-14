context("model-pathfinder")

set_cmdstan_path("/home/sbronder/open_source/stan/origin/cmdstan")

stan_program <- testing_stan_file("bernoulli")

mod <- testing_model("bernoulli")
stan_program_fp <- testing_stan_file("bernoulli_fp")
mod_fp <- testing_model("bernoulli_fp")

mod_params <- testing_model("parameter_types")
# valid ways to supply data
data_list <- testing_data("bernoulli")
data_file_r <- test_path("resources", "data", "bernoulli.data.R")
data_file_json <- test_path("resources", "data", "bernoulli.data.json")

# these are all valid for sample()
ok_arg_values <- list(
  data = data_list,
  output_dir = tempdir(),
  refresh = 5,
  init = 1.5,
  seed = 12345,
  init_alpha = 1,
  tol_obj = 1e-12,
  tol_rel_obj = 1e-12,
  tol_grad = 1e-12,
  tol_rel_grad = 1e-12,
  tol_param = 1e-12,
  history_size = 5,
  num_elbo_draws = 10,
  single_path_draws = 10,
  draws = 100,
  num_paths = 4,
  max_lbfgs_iters = 100,
  save_single_paths = FALSE,
  calculate_lp = TRUE,
  psis_resample=TRUE)

# using any one of these should cause sample() to error
bad_arg_values <- list(
  data = "NOT_A_FILE",
  output_dir = "NOT_A_DIRECTORY",
  refresh = -1,
  init = "maybe :P",
  seed = -80,
  init_alpha = "cat.jpeg",
  init_alpha = -3,
  tol_obj = -1,
  tol_rel_obj = -4,
  tol_grad = -5,
  tol_rel_grad = -9,
  tol_param = -2,
  history_size = -6,
  num_elbo_draws = -8,
  draws = "no thanks",
  single_path_draws = "Just one plz",
  num_paths = -1,
  max_lbfgs_iters = "idk :/"
)

bad_arg_values_2 <- list(
  data = "NOT_A_FILE",
  output_dir = "NOT_A_DIRECTORY",
  refresh = -1,
  init = "maybe :P",
  seed = -80,
  init_alpha = -3,
  tol_obj = -1,
  tol_rel_obj = -4,
  tol_grad = -5,
  tol_rel_grad = -9,
  tol_param = -2,
  history_size = -6,
  num_elbo_draws = -8,
  draws = "no thanks",
  single_path_draws = "nope",
  num_paths = -1,
  max_lbfgs_iters = "idk :/",
  save_single_paths = "Mby"
)

bad_arg_values_3 <- list(
  data = matrix(1:10),
  output_dir = 8,
  init = "maybe :P",
  seed = -80,
  init_alpha = -3,
  tol_obj = -1,
  tol_rel_obj = -4,
  tol_grad = -5,
  tol_rel_grad = -9,
  tol_param = -2,
  history_size = -6,
  num_elbo_draws = -8,
  draws = "no thanks",
  single_path_draws = " ",
  num_paths = "NO!",
  max_lbfgs_iters = "idk :/"
)
expect_pathfinder_output <- function(object, num_chains = NULL) {
  expect_output(object, regexp = "Finished in (.*) seconds.")
}


test_that("Pathfinder Runs", {
  expect_pathfinder_output(fit <- mod$pathfinder(data=data_list, seed=1234, refresh = 0))
  expect_is(fit, "CmdStanPathfinder")
})
test_that("Pathfinder Runs", {
  expect_pathfinder_output(fit <- mod_params$pathfinder(data=data_list, seed=1234, refresh = 0))
  expect_is(fit, "CmdStanPathfinder")
})


test_that("pathfinder() method works with data files", {
  expect_pathfinder_output(fit_r <- mod$pathfinder(data = data_file_r))
  expect_is(fit_r, "CmdStanPathfinder")

  expect_pathfinder_output(fit_json <- mod$pathfinder(data = data_file_json))
  expect_is(fit_json, "CmdStanPathfinder")
})

test_that("pathfinder() method works with init file", {
  init_list <- list(theta = 0.5)
  init_file <- tempfile(
    tmpdir = cmdstanr:::cmdstan_tempdir(),
    pattern = "testing-inits-",
    fileext = ".json"
  )
  write_stan_json(init_list, file = init_file)
  expect_pathfinder_output(mod$pathfinder(data = data_file_r, init = init_file))
})

test_that("pathfinder() method runs when all arguments specified", {
  expect_pathfinder_output(fit <- do.call(mod$pathfinder, ok_arg_values))
  expect_is(fit, "CmdStanPathfinder")
})

test_that("pathfinder() method runs when the stan file is removed", {
  stan_file_tmp <- tempfile(pattern = "tmp", fileext = ".stan")
  file.copy(stan_program, stan_file_tmp)
  mod_tmp <- cmdstan_model(stan_file_tmp)
  file.remove(stan_file_tmp)
  expect_pathfinder_output(
    mod_tmp$pathfinder(data = data_list)
  )
})

test_that("no error when checking estimates after failure", {
  fit <- cmdstanr_example("schools", method = "pathfinder", seed = 123) # optim always fails for this
  expect_silent(fit$summary()) # no error
})

