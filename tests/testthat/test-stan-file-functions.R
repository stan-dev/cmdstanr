skip_on_cran()

set_cmdstan_path()

# The standalone functions do what the model methods do, from a file alone.
# Each is tried on a program without #include and on one with, where the
# program's own directory stands in for include_paths when none are given.

test_that("check_syntax_stan_file() checks a program", {
  expect_message(
    expect_true(check_syntax_stan_file(testing_stan_file("bernoulli"))),
    "Stan program is syntactically correct"
  )
  expect_error(
    check_syntax_stan_file(testing_stan_file("fail"), quiet = TRUE),
    "Syntax error found!"
  )

  include_model <- local_include_model_with_spaces()
  expect_true(check_syntax_stan_file(
    include_model$stan_file,
    include_paths = include_model$include_paths,
    quiet = TRUE
  ))
  expect_true(check_syntax_stan_file(include_model$stan_file, quiet = TRUE))
})

test_that("check_syntax_stan_file() and variables_stan_file() leave nothing behind", {
  before <- list.files(tempdir())
  check_syntax_stan_file(testing_stan_file("bernoulli"), quiet = TRUE)
  variables_stan_file(testing_stan_file("bernoulli"))
  expect_equal(list.files(tempdir()), before)
})

test_that("format_stan_file() formats a program", {
  stan_file <- withr::local_tempfile(
    lines = "parameters {real y;} model {y ~ std_normal();}", fileext = ".stan"
  )
  expect_output(format_stan_file(stan_file), "  real y;", fixed = TRUE)
  expect_message(
    expect_true(format_stan_file(stan_file, overwrite_file = TRUE)),
    "Old version of the model stored to"
  )
  expect_true("  real y;" %in% readLines(stan_file))
  expect_length(
    list.files(
      dirname(stan_file), pattern = paste0(basename(stan_file), ".bak-")
    ),
    1
  )

  include_model <- local_include_model_with_spaces()
  expect_output(
    format_stan_file(
      include_model$stan_file, include_paths = include_model$include_paths
    ),
    "#include ", fixed = TRUE
  )
  expect_output(
    format_stan_file(include_model$stan_file, canonicalize = list("includes")),
    "real divide_real_by_two", fixed = TRUE
  )

  expect_output(
    format_stan_file(stan_file, canonicalize = TRUE), "  real y;", fixed = TRUE
  )
  long_line <- withr::local_tempfile(
    lines = paste0(
      "parameters {real y;} model {y ~ normal(0, ",
      paste(rep("1", 20), collapse = " + "), ");}"
    ),
    fileext = ".stan"
  )
  expect_gt(max(nchar(capture.output(format_stan_file(long_line)))), 30)
  format_stan_file(
    long_line, max_line_length = 30, overwrite_file = TRUE, backup = FALSE
  )
  expect_true(all(nchar(readLines(long_line)) <= 30))
})

test_that("variables_stan_file() reports a program's variables", {
  variables <- variables_stan_file(testing_stan_file("bernoulli"))
  expect_equal(variables$data$N, list(type = "int", dimensions = 0))
  expect_equal(variables$data$y, list(type = "int", dimensions = 1))
  expect_equal(variables$parameters$theta, list(type = "real", dimensions = 0))

  include_model <- local_include_model_with_spaces()
  with_paths <- variables_stan_file(
    include_model$stan_file, include_paths = include_model$include_paths
  )
  expect_equal(names(with_paths$parameters), "theta")
  expect_equal(variables_stan_file(include_model$stan_file), with_paths)
})

test_that("variables_stan_file() accepts a declared but undefined function", {
  stan_file <- withr::local_tempfile(
    lines = c(
      "functions {",
      "  real f(real x);",
      "}",
      "parameters {",
      "  real y;",
      "}"
    ),
    fileext = ".stan"
  )
  expect_no_error(variables_stan_file(stan_file))
})

test_that("compile_stan_file() builds or reuses the executable", {
  model_dir <- withr::local_tempdir()
  stan_file <- file.path(model_dir, "bernoulli.stan")
  file.copy(testing_stan_file("bernoulli"), stan_file)
  with_mocked_cli(compile_ret = list(status = 0), info_ret = default_info_ret, {
    expect_mock_compile(exe <- compile_stan_file(stan_file))
    expect_equal(exe, cmdstan_ext(strip_ext(resolve_path(stan_file))))
    expect_true(file.exists(exe))
    expect_no_mock_compile(expect_equal(compile_stan_file(stan_file), exe))
    expect_mock_compile(
      compile_stan_file(stan_file, cpp_options = list(stan_threads = TRUE))
    )
    # the record is beside it, so a model can adopt it without a launch
    mod <- cmdstan_model(exe_file = exe)
    expect_equal(mod$cpp_options(), list(STAN_THREADS = "TRUE"))

    exe_dir <- withr::local_tempdir()
    expect_mock_compile(exe_in_dir <- compile_stan_file(stan_file, dir = exe_dir))
    expect_equal(dirname(exe_in_dir), resolve_path(exe_dir))
  })

  include_model <- local_include_model_with_spaces()
  with_mocked_cli(compile_ret = list(status = 0), info_ret = default_info_ret, {
    expect_mock_compile(compile_stan_file(include_model$stan_file))
  })
})
