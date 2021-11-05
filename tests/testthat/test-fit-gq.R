context("fitted-gq")

set_cmdstan_path()
fit <- testing_fit("bernoulli", method = "sample", seed = 123)
fit_gq <- testing_fit("bernoulli_ppc", method = "generate_quantities", seed = 123, fitted_params = fit)
PARAM_NAMES <- c("y_rep[1]", "y_rep[2]", "y_rep[3]", "y_rep[4]", "y_rep[5]",
                 "y_rep[6]", "y_rep[7]", "y_rep[8]", "y_rep[9]", "y_rep[10]",
                 "sum_y")

test_that("draws() stops for unkown variables", {
  expect_error(
    fit_gq$draws(variables = "ABCD"),
    "Can't find the following variable(s) in the output: ABCD",
    fixed = TRUE
  )
  fit_gq$draws()
  expect_error(
    fit_gq$draws(variables = c("ABCD", "EFGH")),
    "Can't find the following variable(s) in the output: ABCD, EFGH",
    fixed = TRUE
  )
})

test_that("draws() method returns draws_array (reading csv works)", {
  draws <- fit_gq$draws()
  draws_ys <- fit_gq$draws(variables = "y_rep")
  draws_y <- fit_gq$draws(variables = "y_rep[1]")
  draws_sum_y <- fit_gq$draws(variables = c("sum_y", "y_rep"))
  draws_y_sum <- fit_gq$draws(variables = c("y_rep", "sum_y"))
  draws_all_after <- fit_gq$draws()
  expect_type(draws, "integer")
  expect_s3_class(draws, "draws_array")
  expect_equal(posterior::variables(draws), PARAM_NAMES)
  expect_equal(posterior::nchains(draws), fit_gq$num_chains())
  expect_s3_class(draws_ys, "draws_array")
  expect_equal(posterior::nvariables(draws_ys), 10)
  expect_equal(posterior::nchains(draws_ys), fit_gq$num_chains())
  expect_s3_class(draws_y, "draws_array")
  expect_equal(posterior::nvariables(draws_y), 1)
  expect_equal(posterior::nchains(draws_y), fit_gq$num_chains())
  expect_s3_class(draws_sum_y, "draws_array")
  expect_equal(posterior::nvariables(draws_sum_y), 11)
  expect_equal(posterior::nchains(draws_sum_y), fit_gq$num_chains())
  expect_s3_class(draws_all_after, "draws_array")
  expect_equal(posterior::nvariables(draws_all_after), 11)
  expect_equal(posterior::nchains(draws_all_after), fit_gq$num_chains())

  expect_equal(dim(draws), c(1000, 4, 11))
  expect_equal(dim(draws_ys), c(1000, 4, 10))
  expect_equal(dim(draws_y), c(1000, 4, 1))
  expect_equal(dim(draws_all_after), c(1000, 4, 11))

  # check the order of the draws
  expect_equal(posterior::variables(draws_sum_y), c("sum_y", PARAM_NAMES[1:(length(PARAM_NAMES)-1)]))
  expect_equal(posterior::variables(draws_y_sum), PARAM_NAMES)
})

test_that("summary() method works after gq", {
  x <- fit_gq$summary()
  expect_s3_class(x, "draws_summary")
  expect_equal(x$variable, PARAM_NAMES)

  x <- fit_gq$summary("sum_y", c("median", "mad"))
  expect_equal(x$variable, "sum_y")
  expect_equal(colnames(x), c("variable", "median", "mad"))
})

test_that("print() method works after gq", {
  expect_output(expect_s3_class(fit_gq$print(), "CmdStanGQ"), "variable")
  expect_output(fit_gq$print(max_rows = 1), "# showing 1 of 11 rows")
  expect_output(fit_gq$print(NULL, c("mad")), "mad")

  expect_output(fit_gq$print(), "showing 10 of 11 rows")
  expect_output(fit_gq$print(max_rows = 2), "showing 2 of 11 rows")
  expect_output(fit_gq$print(max_rows = 11), "sum_y", fixed=TRUE) # last parameter
  expect_output(fit_gq$print("y_rep", max_rows = 2), "showing 2 of 10 rows")
  expect_error(
    fit_gq$print(variable = "unknown", max_rows = 20),
    "Can't find the following variable(s): unknown",
    fixed = TRUE
  ) # unknown parameter

  out <- capture.output(fit_gq$print("y_rep"))
  expect_length(out, 11) # columns names + 1 y_rep
  expect_match(out[1], "variable")
  expect_match(out[2], "y_rep[1]", fixed = TRUE)
  expect_match(out[9], "y_rep[8]", fixed = TRUE)
  expect_false(any(grepl("sum_y|theta", out)))

  # make sure the row order is correct
  out <- capture.output(fit_gq$print(c("y_rep[1]", "sum_y", "y_rep[3]")))
  expect_length(out, 4)
  expect_match(out[1], " variable", out[1])
  expect_match(out[2], " y_rep[1]", fixed = TRUE)
  expect_match(out[3], " sum_y")
  expect_match(out[4], " y_rep[3]", fixed = TRUE)
})

test_that("output() method works after gq", {
  checkmate::expect_list(
    fit_gq$output(),
    types = "character",
    any.missing = FALSE,
    len = fit_gq$runset$num_procs()
  )
  expect_output(fit_gq$output(id = 1), "method = generate_quantities")
})

test_that("time() works after gq", {
  run_times <- fit_gq$time()
  checkmate::expect_list(run_times, names = "strict", any.missing = FALSE)
  testthat::expect_named(run_times, c("total", "chains"))
  checkmate::expect_number(run_times$total, finite = TRUE)
  checkmate::expect_data_frame(
    run_times$chains,
    any.missing = FALSE,
    types = c("integer", "numeric"),
    nrows = fit_gq$runset$num_procs(),
    ncols = 2
  )
})

test_that("fitted_params_files() works", {
  expect_equal(
    fit_gq$fitted_params_files(),
    fit$output_files()
  )
})

test_that("draws() works for different formats", {
  a <- fit_gq$draws()
  expect_true(posterior::is_draws_array(a))
  a <- fit_gq$draws(format = "list")
  expect_true(posterior::is_draws_list(a))
  a <- fit_gq$draws(format = "array")
  expect_true(posterior::is_draws_array(a))
  a <- fit_gq$draws(format = "df")
  expect_true(posterior::is_draws_df(a))
})

test_that("draws() errors if invalid format", {
  expect_error(
    fit_gq$draws(format = "bad_format"),
    "The supplied draws format is not valid"
  )
})
