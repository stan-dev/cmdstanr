set_cmdstan_path()
mod <- testing_model("bernoulli")

test_that("get_cmdstan_args() errors for uncompiled model", {
  mod_uncompiled <- cmdstan_model(
    stan_file = testing_stan_file("bernoulli"),
    compile = FALSE
  )
  expect_error(
    mod_uncompiled$get_cmdstan_args("sample"),
    "'$get_cmdstan_args()' requires a compiled model",
    fixed = TRUE
  )
})

test_that("get_cmdstan_args() errors for invalid method", {
  expect_error(
    mod$get_cmdstan_args("bogus"),
    "'arg' should be one of",
    fixed = TRUE
  )
})

test_that("get_cmdstan_args() returns named list for sample", {
  args <- mod$get_cmdstan_args("sample")
  expect_type(args, "list")
  expect_named(args)
  expected_names <- names(map_cmdstan_to_cmdstanr("sample"))
  for (nm in expected_names) {
    expect_true(nm %in% names(args), info = paste0("missing: ", nm))
  }
})

test_that("get_cmdstan_args() returns expected default types for sample", {
  args <- mod$get_cmdstan_args("sample")
  expect_type(args$iter_sampling, "integer")
  expect_type(args$iter_warmup, "integer")
  expect_type(args$thin, "integer")
  expect_type(args$adapt_delta, "double")
  expect_type(args$save_warmup, "logical")
  expect_type(args$max_treedepth, "integer")
})

test_that("get_cmdstan_args() works for optimize", {
  args <- mod$get_cmdstan_args("optimize")
  expect_type(args, "list")
  expect_named(args)
  expected_names <- names(map_cmdstan_to_cmdstanr("optimize"))
  for (nm in expected_names) {
    expect_true(nm %in% names(args), info = paste0("missing: ", nm))
  }
  expect_type(args$jacobian, "logical")
  expect_type(args$iter, "integer")
})

test_that("get_cmdstan_args() works for variational", {
  args <- mod$get_cmdstan_args("variational")
  expect_type(args, "list")
  expect_named(args)
  expected_names <- names(map_cmdstan_to_cmdstanr("variational"))
  for (nm in expected_names) {
    expect_true(nm %in% names(args), info = paste0("missing: ", nm))
  }
})

test_that("get_cmdstan_args() works for pathfinder", {
  args <- mod$get_cmdstan_args("pathfinder")
  expect_type(args, "list")
  expect_named(args)
  expected_names <- names(map_cmdstan_to_cmdstanr("pathfinder"))
  for (nm in expected_names) {
    expect_true(nm %in% names(args), info = paste0("missing: ", nm))
  }
})

test_that("get_cmdstan_args() works for laplace", {
  args <- mod$get_cmdstan_args("laplace")
  expect_type(args, "list")
  expect_named(args)
  expected_names <- names(map_cmdstan_to_cmdstanr("laplace"))
  for (nm in expected_names) {
    expect_true(nm %in% names(args), info = paste0("missing: ", nm))
  }
  expect_type(args$jacobian, "logical")
  expect_type(args$draws, "integer")
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
