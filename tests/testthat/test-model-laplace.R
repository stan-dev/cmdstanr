context("model-laplace")

set_cmdstan_path()
mod <- testing_model("logistic")
data_list <- testing_data("logistic")

# these are all valid for laplace()
ok_arg_values <- list(
  data = data_list,
  refresh = 100,
  init = NULL,
  seed = 12345,
  mode = NULL,
  draws = 100,
  jacobian = TRUE,
  opt_args = list(
    algorithm = "lbfgs",
    iter = 100,
    init_alpha = 0.002,
    tol_obj = 1e-11
  )
)

# using any of these should cause optimize() to error
bad_arg_values <- list(
  data = "NOT_A_FILE",
  refresh = -20,
  init = "NOT_A_FILE",
  seed = "NOT_A_SEED",
  jacobian = 30,
  draws = -10,
  mode = 10
)

test_that("laplace() method errors for any invalid argument before calling cmdstan", {
  for (nm in names(bad_arg_values)) {
    args <- ok_arg_values
    args[[nm]] <- bad_arg_values[[nm]]
    expect_error(do.call(mod$laplace, args), regexp = nm, info = nm)
  }
  args <- ok_arg_values
  args$opt_args <- list(iter = "NOT_A_NUMBER")
  expect_error(do.call(mod$laplace, args), regexp = "Must be of type 'integerish'")
})

test_that("laplace() runs when all arguments specified validly", {
  # specifying all arguments validly
  expect_laplace_output(fit1 <- do.call(mod$laplace, ok_arg_values))
  expect_is(fit1, "CmdStanLaplace")

  # check that correct arguments were indeed passed to CmdStan
  expect_equal(fit1$metadata()$refresh, ok_arg_values$refresh)
  expect_equal(fit1$metadata()$jacobian, as.integer(ok_arg_values$jacobian))
  expect_equal(fit1$metadata()$draws, as.integer(ok_arg_values$draws))
  expect_equal(fit1$mode()$metadata()$jacobian, as.integer(ok_arg_values$jacobian))
  expect_equal(fit1$mode()$metadata()$init_alpha, ok_arg_values$opt_args$init_alpha)

  expect_equal(fit1$mode()$metadata()$tol_obj, ok_arg_values$opt_args$tol_obj, tolerance = 0)

  # leaving all at default (except 'data')
  expect_laplace_output(fit2 <- mod$laplace(data = data_list, seed = 123))
  expect_is(fit2, "CmdStanLaplace")
})

test_that("laplace() all valid 'mode' inputs give same results", {
  mode <- mod$optimize(data = data_list, jacobian = TRUE, seed = 100, refresh = 0)
  fit1 <- mod$laplace(data = data_list, mode = mode, seed = 100, refresh = 0)
  fit2 <- mod$laplace(data = data_list, mode = mode$output_files(), seed = 100, refresh = 0)
  fit3 <- mod$laplace(data = data_list, mode = NULL, seed = 100, refresh = 0)

  expect_is(fit1, "CmdStanLaplace")
  expect_is(fit2, "CmdStanLaplace")
  expect_is(fit3, "CmdStanLaplace")
  expect_is(fit1$mode(), "CmdStanMLE")
  expect_is(fit2$mode(), "CmdStanMLE")
  expect_is(fit3$mode(), "CmdStanMLE")
  expect_equal(fit1$mode()$mle(), fit2$mode()$mle())
  expect_equal(fit1$mode()$mle(), fit3$mode()$mle())
  expect_equal(fit1$lp(), fit2$lp())
  expect_equal(fit1$lp(), fit3$lp())
  expect_equal(fit1$lp_approx(), fit2$lp_approx())
  expect_equal(fit1$lp_approx(), fit3$lp_approx())
  expect_equal(fit1$draws(), fit2$draws())
  expect_equal(fit1$draws(), fit3$draws())
})

test_that("laplace() allows choosing number of draws", {
  fit <- mod$laplace(data = data_list, draws = 10, refresh = 0)
  expect_equal(fit$metadata()$draws, 10)
  expect_equal(posterior::ndraws(fit$draws()), 10)

  fit2 <- mod$laplace(data = data_list, draws = 100, refresh = 0)
  expect_equal(fit2$metadata()$draws, 100)
  expect_equal(posterior::ndraws(fit2$draws()), 100)
})

test_that("laplace() errors if jacobian arg doesn't match what optimize used", {
  fit <- mod$optimize(data = data_list, jacobian = FALSE, refresh = 0)
  expect_error(
    mod$laplace(data = data_list, mode = fit, jacobian = TRUE),
    "'jacobian' argument to optimize and laplace must match"
  )
  expect_error(
    mod$laplace(data = data_list, mode = fit, jacobian = TRUE),
    "laplace was called with jacobian=TRUE\noptimize was run with jacobian=FALSE"
  )
})

test_that("laplace() errors with bad combinations of arguments", {
  fit <- mod$optimize(data = data_list, jacobian = TRUE, refresh = 0)
  expect_error(
    mod$laplace(data = data_list, mode = mod, opt_args = list(iter = 10)),
    "Cannot specify both 'opt_args' and 'mode' arguments."
  )
  expect_error(
    mod$laplace(data = data_list, mode = rnorm(10)),
    "If not NULL or a CmdStanMLE object then 'mode' must be a path to a CSV file"
  )
})

test_that("laplace() errors if optimize() fails", {
  mod_schools <- testing_model("schools")
  expect_error(
    expect_message(
      mod_schools$laplace(data = testing_data("schools"), refresh = 0),
      "Line search failed to achieve a sufficient decrease"
    ),
    "Optimization failed"
  )

})
