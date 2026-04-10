set_cmdstan_path()
fit_vb <- testing_fit("logistic", method = "variational", seed = 123)
fit_vb_sci_not <- testing_fit("logistic", method = "variational", seed = 123, iter = 200000, adapt_iter = 100000)
mod <- testing_model("bernoulli")
data_list <- testing_data("bernoulli")
PARAM_NAMES <- c("alpha", "beta[1]", "beta[2]", "beta[3]")


test_that("summary() method works after vb", {
  x <- fit_vb$summary()
  expect_s3_class(x, "draws_summary")
  expect_equal(x$variable, c("lp__", "lp_approx__", PARAM_NAMES))

  x <- fit_vb$summary(variables = NULL, c("mean", "sd"))
  expect_s3_class(x, "draws_summary")
  expect_equal(x$variable, c("lp__", "lp_approx__", PARAM_NAMES))
  expect_equal(colnames(x), c("variable", "mean", "sd"))
})

test_that("print() method works after vb", {
  expect_snapshot(
    expect_s3_class(fit_vb$print(), "CmdStanVB"),
    transform = transform_print_snapshot,
    cran = FALSE
  )

  expect_snapshot(
    fit_vb$print(max_rows = 1),
    transform = transform_print_snapshot,
    cran = FALSE
  )

  # test on model with more parameters
  fit <- cmdstanr_example("schools_ncp", method = "variational", seed = 123)
  expect_snapshot(
    fit$print(),
    transform = transform_print_snapshot,
    cran = FALSE
  )
  expect_snapshot(
    fit$print(max_rows = 20),
    transform = transform_print_snapshot,
    cran = FALSE
  )
  expect_snapshot(
    fit$print(c("theta", "tau", "lp__", "lp_approx__")),
    transform = transform_print_snapshot,
    cran = FALSE
  )

  expect_error(
    fit$print(variable = "unknown", max_rows = 20),
    "Can't find the following variable(s): unknown",
    fixed = TRUE
  ) # unknown parameter
})

test_that("draws() method returns posterior sample (reading csv works)", {
  draws <- fit_vb$draws()
  expect_type(draws, "double")
  expect_s3_class(draws, "draws_matrix")
  expect_equal(posterior::variables(draws), c("lp__", "lp_approx__", PARAM_NAMES))
})

test_that("lp(), lp_approx() methods return vectors (reading csv works)", {
  lp <- fit_vb$lp()
  lg <- fit_vb$lp_approx()
  expect_type(lp, "double")
  expect_type(lg, "double")
  expect_equal(length(lp), nrow(fit_vb$draws()))
  expect_equal(length(lg), length(lp))
})

test_that("vb works with scientific notation args", {
  x <- fit_vb_sci_not$summary()
  expect_s3_class(x, "draws_summary")
  expect_equal(x$variable, c("lp__", "lp_approx__", PARAM_NAMES))

  x <- fit_vb_sci_not$summary(variables = NULL, c("mean", "sd"))
  expect_s3_class(x, "draws_summary")
  expect_equal(x$variable, c("lp__", "lp_approx__", PARAM_NAMES))
  expect_equal(colnames(x), c("variable", "mean", "sd"))
})

test_that("time() method works after vb", {
  run_times <- fit_vb$time()
  checkmate::expect_list(run_times, names = "strict", any.missing = FALSE)
  testthat::expect_named(run_times, c("total"))
  checkmate::expect_number(run_times$total, finite = TRUE)
})

test_that("output() works for vb", {
  expect_output(fit_vb$output(),
                "method = variational")
})

test_that("time is reported after vb", {
  expect_output(
    mod$variational(data = data_list,
                    seed = 123,
                    elbo_samples = 1000,
                    iter = 2000,
                    draws = 50
                    ),
    "Finished in"
  )
})

test_that("draws() works for different formats", {
  a <- fit_vb$draws()
  expect_true(posterior::is_draws_matrix(a))
  a <- fit_vb$draws(format = "list")
  expect_true(posterior::is_draws_list(a))
  a <- fit_vb$draws(format = "array")
  expect_true(posterior::is_draws_array(a))
  a <- fit_vb$draws(format = "df")
  expect_true(posterior::is_draws_df(a))
})

test_that("draws() errors if invalid format", {
  expect_error(
    fit_vb$draws(format = "bad_format"),
    "The supplied draws format is not valid"
  )
})
