set_cmdstan_path()
mod <- testing_model("bernoulli")

test_that("cmdstan_defaults() errors for uncompiled model", {
  mod_uncompiled <- cmdstan_model(
    stan_file = testing_stan_file("bernoulli"),
    compile = FALSE
  )
  expect_error(
    mod_uncompiled$cmdstan_defaults("sample"),
    "'$cmdstan_defaults()' requires a compiled model",
    fixed = TRUE
  )
})

test_that("cmdstan_defaults() errors for invalid method", {
  expect_error(
    mod$cmdstan_defaults("bogus"),
    "'arg' should be one of",
    fixed = TRUE
  )
})

test_that("cmdstan_defaults() returns named list for sample", {
  args <- mod$cmdstan_defaults("sample")
  expect_type(args, "list")
  expect_named(args)
  expected_names <- names(map_cmdstan_to_cmdstanr("sample"))
  for (nm in expected_names) {
    expect_true(nm %in% names(args), info = paste0("missing: ", nm))
  }
})

test_that("cmdstan_defaults() returns expected default types for sample", {
  args <- mod$cmdstan_defaults("sample")
  expect_type(args$iter_sampling, "integer")
  expect_type(args$iter_warmup, "integer")
  expect_type(args$thin, "integer")
  expect_type(args$adapt_delta, "double")
  expect_type(args$save_warmup, "logical")
  expect_type(args$max_treedepth, "integer")
})

test_that("cmdstan_defaults() works for optimize", {
  args <- mod$cmdstan_defaults("optimize")
  expect_type(args, "list")
  expect_named(args)
  expected_names <- names(map_cmdstan_to_cmdstanr("optimize"))
  for (nm in expected_names) {
    expect_true(nm %in% names(args), info = paste0("missing: ", nm))
  }
  expect_type(args$jacobian, "logical")
  expect_type(args$iter, "integer")
})

test_that("cmdstan_defaults() works for variational", {
  args <- mod$cmdstan_defaults("variational")
  expect_type(args, "list")
  expect_named(args)
  expected_names <- names(map_cmdstan_to_cmdstanr("variational"))
  for (nm in expected_names) {
    expect_true(nm %in% names(args), info = paste0("missing: ", nm))
  }
})

test_that("cmdstan_defaults() works for pathfinder", {
  args <- mod$cmdstan_defaults("pathfinder")
  expect_type(args, "list")
  expect_named(args)
  expected_names <- names(map_cmdstan_to_cmdstanr("pathfinder"))
  for (nm in expected_names) {
    expect_true(nm %in% names(args), info = paste0("missing: ", nm))
  }
})

test_that("cmdstan_defaults() works for laplace", {
  args <- mod$cmdstan_defaults("laplace")
  expect_type(args, "list")
  expect_named(args)
  expected_names <- names(map_cmdstan_to_cmdstanr("laplace"))
  for (nm in expected_names) {
    expect_true(nm %in% names(args), info = paste0("missing: ", nm))
  }
  expect_type(args$jacobian, "logical")
  expect_type(args$draws, "integer")
})

# default values ----------------------------------------------------------

test_that("cmdstan_defaults() returns expected default values for sample", {
  args <- mod$cmdstan_defaults("sample")
  expect_identical(args$iter_sampling, 1000L)
  expect_identical(args$iter_warmup, 1000L)
  expect_identical(args$save_warmup, FALSE)
  expect_identical(args$thin, 1L)
  expect_identical(args$adapt_engaged, TRUE)
  expect_identical(args$adapt_delta, 0.8)
  expect_identical(args$init_buffer, 75L)
  expect_identical(args$term_buffer, 50L)
  expect_identical(args$window, 25L)
  expect_identical(args$save_metric, FALSE)
  expect_identical(args$max_treedepth, 10L)
  expect_identical(args$metric, "diag_e")
  expect_identical(args$step_size, 1L)
  expect_identical(args$chains, 1L)
})

test_that("cmdstan_defaults() returns expected default values for optimize", {
  args <- mod$cmdstan_defaults("optimize")
  expect_identical(args$algorithm, "lbfgs")
  expect_identical(args$jacobian, FALSE)
  expect_identical(args$iter, 2000L)
  expect_identical(args$init_alpha, 0.001)
  expect_identical(args$tol_obj, 1e-12)
  expect_identical(args$tol_rel_obj, 10000L)
  expect_identical(args$tol_grad, 1e-08)
  expect_identical(args$tol_rel_grad, 1e+07)
  expect_identical(args$tol_param, 1e-08)
  expect_identical(args$history_size, 5L)
})

test_that("cmdstan_defaults() returns expected default values for variational", {
  args <- mod$cmdstan_defaults("variational")
  expect_identical(args$algorithm, "meanfield")
  expect_identical(args$iter, 10000L)
  expect_identical(args$grad_samples, 1L)
  expect_identical(args$elbo_samples, 100L)
  expect_identical(args$eta, 1L)
  expect_identical(args$adapt_engaged, TRUE)
  expect_identical(args$adapt_iter, 50L)
  expect_identical(args$tol_rel_obj, 0.01)
  expect_identical(args$eval_elbo, 100L)
  expect_identical(args$draws, 1000L)
})

test_that("cmdstan_defaults() returns expected default values for pathfinder", {
  args <- mod$cmdstan_defaults("pathfinder")
  expect_identical(args$init_alpha, 0.001)
  expect_identical(args$tol_obj, 1e-12)
  expect_identical(args$tol_rel_obj, 10000L)
  expect_identical(args$tol_grad, 1e-08)
  expect_identical(args$tol_rel_grad, 1e+07)
  expect_identical(args$tol_param, 1e-08)
  expect_identical(args$history_size, 5L)
  expect_identical(args$draws, 1000L)
  expect_identical(args$num_paths, 4L)
  expect_identical(args$save_single_paths, FALSE)
  expect_identical(args$psis_resample, TRUE)
  expect_identical(args$calculate_lp, TRUE)
  expect_identical(args$max_lbfgs_iters, 1000L)
  expect_identical(args$single_path_draws, 1000L)
  expect_identical(args$num_elbo_draws, 25L)
})

test_that("cmdstan_defaults() returns expected default values for laplace", {
  args <- mod$cmdstan_defaults("laplace")
  expect_identical(args$jacobian, TRUE)
  expect_identical(args$draws, 1000L)
})

# internal helpers --------------------------------------------------------

test_that("parse_default_value() parses booleans", {
  expect_identical(parse_default_value("Defaults to true"), TRUE)
  expect_identical(parse_default_value("Defaults to false"), FALSE)
})

test_that("parse_default_value() parses integers", {
  expect_identical(parse_default_value("Defaults to 1000"), 1000L)
  expect_identical(parse_default_value("Defaults to -1"), -1L)
  expect_identical(parse_default_value("Defaults to 0"), 0L)
})

test_that("parse_default_value() parses doubles", {
  expect_identical(parse_default_value("Defaults to 0.8"), 0.8)
  expect_identical(parse_default_value("Defaults to 1e-6"), 1e-6)
  expect_identical(parse_default_value("Defaults to -0.5"), -0.5)
})

test_that("parse_default_value() returns strings for non-numeric values", {
  expect_identical(parse_default_value("Defaults to lbfgs"), "lbfgs")
  expect_identical(parse_default_value("Defaults to diagonal_e"), "diagonal_e")
})

test_that("map_cmdstan_to_cmdstanr() returns named character for valid methods", {
  for (method in c("sample", "optimize", "variational", "pathfinder", "laplace")) {
    mapping <- map_cmdstan_to_cmdstanr(method)
    expect_type(mapping, "character")
    expect_true(length(mapping) > 0, info = method)
    expect_named(mapping)
  }
})

test_that("map_cmdstan_to_cmdstanr() returns empty for unknown method", {
  expect_length(map_cmdstan_to_cmdstanr("unknown"), 0)
})
