set_cmdstan_path()

data_list_schools <- testing_data("schools")
data_list_logistic <- testing_data("logistic")
test_inits <- function(mod, fit_init, data_list = NULL) {
 utils::capture.output({
  fit_sample <- mod$sample(data = data_list, chains = 1, init = fit_init,
    iter_sampling = 100, iter_warmup = 100, refresh = 0, seed = 1234)
  fit_sample_multi <- mod$sample(data = data_list, chains = 5, init = fit_init,
    iter_sampling = 100, iter_warmup = 100, refresh = 0, seed = 1234)
  fit_vb <- mod$variational(data = data_list, refresh = 0, seed = 1234,
    init = fit_init, algorithm = "fullrank")
  fit_path <- mod$pathfinder(data = data_list, seed=1234, refresh = 0,
    num_paths = 4, init = fit_init)
  fit_laplace <- mod$laplace(data = data_list, seed = 1234, refresh=0,
    init=fit_init)
  fit_ml <- mod$optimize(data = data_list, seed = 1234, refresh = 0,
    init = fit_init, history_size = 400, algorithm = "lbfgs")
  draws = posterior::as_draws_rvars(fit_init$draws())
  fit_sample_draws <- mod$sample(data = data_list, chains = 1, init = draws,
    iter_sampling = 100, iter_warmup = 100, refresh = 0, seed = 1234)
 })
  return(0)
}

test_that("Sample method works as init", {
  mod_params <- testing_model("parameter_types")
  utils::capture.output(fit_sample_init <- mod_params$sample(chains = 1,
    iter_warmup = 100, iter_sampling = 100, refresh = 0, seed = 1234))
  expect_no_error(test_inits(mod_params, fit_sample_init))
})

test_that("Multi chain Sample method works as init", {
  mod_params <- testing_model("parameter_types")
  utils::capture.output(fit_sample_multi_init <- mod_params$sample(chains = 4,
    iter_warmup = 100, iter_sampling = 100, refresh = 0, seed = 1234))
  expect_no_error(test_inits(mod_params, fit_sample_multi_init))
})

test_that("Subsets of parameters are allowed", {
  mod_logistic_simple <- testing_model("logistic_simple")
  utils::capture.output(fit_sample_init_simple <- mod_logistic_simple$sample(chains = 1,
    data = data_list_logistic, iter_warmup = 100, iter_sampling = 100,
    refresh = 0, seed = 1234))
  mod_logistic <- testing_model("logistic")
  expect_no_error(test_inits(mod_logistic, fit_sample_init_simple,
    data_list_logistic))
})

test_that("Pathfinder works as init", {
  mod_logistic <- testing_model("logistic")
  utils::capture.output(fit_path_init <- mod_logistic$pathfinder(
    seed=1234, data = data_list_logistic, refresh = 0, num_paths = 1))
  expect_no_error(test_inits(mod_logistic, fit_path_init, data_list_logistic))
})

test_that("Multi Pathfinder method works as init", {
  mod_logistic <- testing_model("logistic")
  utils::capture.output(fit_path_init <- mod_logistic$pathfinder(seed=1234,
    data = data_list_logistic, refresh = 0, num_paths = 4))
  expect_no_error(test_inits(mod_logistic, fit_path_init, data_list_logistic))
})

test_that("Pathfinder method with psis_resample as false works as init", {
  mod_logistic <- testing_model("logistic")
  utils::capture.output(fit_path_init <- mod_logistic$pathfinder(
    seed=1234, data = data_list_logistic, refresh = 0, num_paths = 1,
    psis_resample = FALSE))
  expect_no_error(test_inits(mod_logistic, fit_path_init, data_list_logistic))
})


test_that("Multi Pathfinder method with psis_resample as false works as init", {
  mod_logistic <- testing_model("logistic")
  utils::capture.output(fit_path_init <- mod_logistic$pathfinder(
    seed=1234, data = data_list_logistic, refresh = 0, num_paths = 4,
    psis_resample = FALSE))
  expect_no_error(test_inits(mod_logistic, fit_path_init, data_list_logistic))
})


test_that("Pathfinder method with calculate_lp as false works as init", {
  mod_logistic <- testing_model("logistic")
  fit_path_init <- mod_logistic$pathfinder(
    seed=1234, data = data_list_logistic, refresh = 0, num_paths = 1,
    psis_resample = FALSE, calculate_lp = FALSE)
  expect_no_error(test_inits(mod_logistic, fit_path_init, data_list_logistic))
})

test_that("Multi Pathfinder method with calculate_lp as false works as init", {
  mod_logistic <- testing_model("logistic")
  utils::capture.output(fit_path_init <- mod_logistic$pathfinder(
    seed=1234, data = data_list_logistic, refresh = 0, num_paths = 4,
    psis_resample = TRUE, calculate_lp = FALSE))
  expect_no_error(test_inits(mod_logistic, fit_path_init, data_list_logistic))
})

test_that("Pathfinder fit initializations use the intended weights", {
  # These draws have four different importance weights, so this test can focus
  # on whether uniform or Pareto-smoothed weights are used.
  draws <- posterior::as_draws_df(data.frame(
    theta = 1:4,
    lp__ = c(1, 2, 4, 8),
    lp_approx__ = 0
  ))
  # This fake Pathfinder fit supplies draws and metadata without running CmdStan.
  pathfinder_fit <- function(num_paths, psis_resample, calculate_lp) {
    structure(
      list(
        draws = function(format) draws,
        metadata = function() list(
          num_paths = num_paths,
          psis_resample = psis_resample,
          calculate_lp = calculate_lp
        ),
        return_codes = function() 0
      ),
      class = "CmdStanPathfinder"
    )
  }

  weights_seen <- list()
  pareto_smooth_calls <- 0L
  # Return known Pareto-smoothed weights and record which weights are used to
  # select the initialization draws.
  local_mocked_bindings(
    pareto_smooth = function(x, ...) {
      pareto_smooth_calls <<- pareto_smooth_calls + 1L
      seq_along(x)
    },
    resample_draws = function(x, ndraws, weights = NULL, ...) {
      weights_seen[[length(weights_seen) + 1L]] <<- weights
      x[seq_len(ndraws), , drop = FALSE]
    },
    .package = "posterior"
  )
  local_mocked_bindings(process_init = function(init, ...) init)

  # Test, in order: output resampled by CmdStan, output not resampled, a
  # single-path fit, and a fit without calculated log densities.
  process_init.CmdStanPathfinder(pathfinder_fit(4, TRUE, TRUE), 2)
  process_init.CmdStanPathfinder(pathfinder_fit(4, FALSE, TRUE), 2)
  process_init.CmdStanPathfinder(pathfinder_fit(1, TRUE, TRUE), 2)
  process_init.CmdStanPathfinder(pathfinder_fit(4, TRUE, FALSE), 2)

  expect_equal(weights_seen, list(rep(1, 4), 1:4, 1:4, rep(1, 4)))
  expect_equal(pareto_smooth_calls, 2L)
})

test_that("Pathfinder init candidates are distinct target parameter vectors", {
  draws <- posterior::as_draws_df(data.frame(
    theta = c(1, 1, 2, 3),
    unused = c(10, 20, 30, 40),
    lp__ = c(1, 1, 1, 1),
    lp_approx__ = 0
  ))
  model_variables <- list(
    parameters = list(theta = list(dimensions = 0L))
  )
  pathfinder_fit <- structure(
    list(
      draws = function(format) draws,
      metadata = function() list(
        num_paths = 4,
        psis_resample = TRUE,
        calculate_lp = TRUE,
        stan_variables = c("theta", "unused")
      ),
      return_codes = function() 0
    ),
    class = "CmdStanPathfinder"
  )

  candidates_seen <- NULL
  weights_seen <- NULL
  method_seen <- NULL
  local_mocked_bindings(
    pareto_smooth = function(...) stop("Pareto smoothing should not be used"),
    resample_draws = function(x, ndraws, weights = NULL, method, ...) {
      candidates_seen <<- x
      weights_seen <<- weights
      method_seen <<- method
      x[seq_len(ndraws), , drop = FALSE]
    },
    .package = "posterior"
  )
  local_mocked_bindings(process_init = function(init, ...) init)

  expect_no_error(
    process_init.CmdStanPathfinder(pathfinder_fit, 3, model_variables)
  )

  expect_equal(candidates_seen$theta, c(1, 2, 3))
  expect_equal(weights_seen, c(2, 1, 1))
  expect_equal(method_seen, "simple_no_replace")
  expect_snapshot(
    error = TRUE,
    process_init.CmdStanPathfinder(pathfinder_fit, 4, model_variables)
  )
})

test_that("draws inits are recycled in order when too few are supplied", {
  draws <- posterior::as_draws_df(data.frame(theta = c(10, 20)))
  model_variables <- list(
    parameters = list(theta = list(dimensions = 0L))
  )
  # Return the prepared init lists instead of writing them to JSON files so the
  # test can inspect which draw is assigned to each chain or path.
  local_mocked_bindings(process_init = function(init, ...) init)

  inits <- process_init.draws(
    draws,
    num_procs = 5,
    model_variables = model_variables
  )

  expect_equal(
    vapply(inits, `[[`, numeric(1), "theta"),
    c(10, 20, 10, 20, 10)
  )
})

test_that("Variational method works as init", {
  mod_logistic <- testing_model("logistic")
  utils::capture.output(fit_vb_init <- mod_logistic$variational(
    data = data_list_logistic, seed=1234, refresh = 0))
  expect_no_error(test_inits(mod_logistic, fit_vb_init, data_list_logistic))
})

test_that("Optimization method works as init", {
  mod_logistic <- testing_model("logistic")
  utils::capture.output(fit_ml_init <- mod_logistic$optimize(
    data = data_list_logistic, seed=1234, refresh = 0))
  expect_no_error(test_inits(mod_logistic, fit_ml_init, data_list_logistic))
})


test_that("Draws Object with NA or Inf throws error", {
  mod_logistic <- testing_model("logistic")
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
