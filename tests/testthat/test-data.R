set_cmdstan_path()
fit <- testing_fit("bernoulli", method = "sample", seed = 123)
fit_vb <- testing_fit("bernoulli", method = "variational", seed = 123)
fit_optimize <- testing_fit("bernoulli", method = "optimize", seed = 123)

test_that("empty data list converted to NULL", {
  stan_file <- write_stan_file("
  parameters {
    real y;
  }
  model {
    y ~ std_normal();
  }
  ")
  expect_null(process_data(list()))
  mod <- cmdstan_model(stan_file, compile = FALSE)
  expect_null(process_data(list(), model_variables = mod$variables()))
})

test_that("process_data works for inputs of length one", {
  data <- list(val = 5)
  stan_file <- write_stan_file("
  data {
    real val;
  }
  ")
  mod <- cmdstan_model(stan_file, compile = FALSE)
  expect_equal(jsonlite::read_json(process_data(data, model_variables = mod$variables())), list(val = 5))
  stan_file <- write_stan_file("
  data {
    int val;
  }
  ")
  mod <- cmdstan_model(stan_file, compile = FALSE)
  expect_equal(jsonlite::read_json(process_data(data, model_variables = mod$variables())), list(val = 5))
  stan_file <- write_stan_file("
  data {
    vector[1] val;
  }
  ")
  mod <- cmdstan_model(stan_file, compile = FALSE)
  expect_equal(jsonlite::read_json(process_data(data, model_variables = mod$variables())), list(val = list(5)))
})

test_that("process_data errors on NULL data variables", {
  stan_file <- write_stan_file("
  data {
    int N;
  }
  ")
  mod <- cmdstan_model(stan_file, compile = FALSE)
  expect_error(
    process_data(list(N = NULL), model_variables = mod$variables()),
    "Variable 'N' is NULL"
  )
})

test_that("process_fitted_params() works with basic input types", {
  temp_file <- tempfile()
  temp_files <- c(tempfile(),
                  tempfile(),
                  tempfile())
  expect_equal(process_fitted_params(temp_file), temp_file)
  expect_equal(process_fitted_params(temp_files), temp_files)
  expect_equal(process_fitted_params(fit), fit$output_files())
})

test_that("process_fitted_params() errors with bad args", {
  error_msg <- paste0(
    "'fitted_params' must be a list of paths to CSV files, a CmdStanMCMC, ",
    "CmdStanMLE, CmdStanLaplace, CmdStanVB, or CmdStanPathfinder object, ",
    "a posterior::draws_array or a posterior::draws_matrix."
  )
  expect_error(
    process_fitted_params(5),
    error_msg
  )
  expect_error(
    process_fitted_params(NULL),
    error_msg
  )

  # WSL Tempdir not cleaned up with R gc
  if (!os_is_wsl()) {
    fit_tmp <- testing_fit("bernoulli", method = "sample", seed = 123)
    temp_file <- tempfile(fileext = ".rds")
    saveRDS(fit_tmp, file = temp_file)
    rm(fit_tmp)
    gc()
    fit_tmp_null <- readRDS(temp_file)
    expect_error(
      process_fitted_params(fit_tmp_null),
      "Unable to obtain draws from the fit object."
    )

    fit_tmp <- testing_fit("bernoulli", method = "variational", seed = 123)
    temp_file <- tempfile(fileext = ".rds")
    saveRDS(fit_tmp, file = temp_file)
    rm(fit_tmp)
    gc()
    fit_tmp_null <- readRDS(temp_file)
    expect_error(
      process_fitted_params(fit_tmp_null),
      "Unable to obtain draws from the fit object."
    )
  }
})

test_that("process_fitted_params() works with CmdStanMLE", {
  file <- process_fitted_params(fit_optimize)
  fit_params <- data.table::fread(file, skip = "lp__")
  expect_equal(nrow(fit_params), 1)
  expect_equal(
    fit_params$theta,
    as.numeric(fit_optimize$draws(variables = "theta"))
  )
})

test_that("process_fitted_params() works if output_files in fit do not exist", {
  fit_ref <- testing_fit("bernoulli", method = "sample", seed = 123)
  fit_tmp <- testing_fit("bernoulli", method = "sample", seed = 123)
  temp_file <- tempfile(fileext = ".rds")
  fit_tmp$save_object(temp_file)
  rm(fit_tmp)
  gc()
  fit_tmp <- readRDS(temp_file)
  expect_false(any(file.exists(fit_tmp$output_files())))
  new_files <- process_fitted_params(fit_tmp)
  expect_true(all(file.exists(new_files)))
  chain <- 1
  for(file in new_files) {
    if (os_is_windows()) {
      grep_path <- repair_path(Sys.which("grep.exe"))
      fread_cmd <- paste0(grep_path, " -v '^#' --color=never ", file)
    } else {
      fread_cmd <- paste0("grep -v '^#' --color=never ", file)
    }
    suppressWarnings(
      tmp_file_gq <- data.table::fread(
        cmd = fread_cmd
      )
    )
    tmp_file_gq <- posterior::as_draws_array(tmp_file_gq)
    expect_equal(
      posterior::subset_draws(fit_ref$draws(), variable = "lp__", chain = chain),
      posterior::subset_draws(tmp_file_gq, variable = "lp__")
    )
    expect_equal(
      posterior::subset_draws(fit_ref$draws(), variable = "theta", chain = chain),
      posterior::subset_draws(tmp_file_gq, variable = "theta")
    )
    expect_equal(
      posterior::subset_draws(fit_ref$sampler_diagnostics(), chain = chain),
      posterior::subset_draws(tmp_file_gq, variable = posterior::variables(fit_ref$sampler_diagnostics()))
    )
    chain <- chain + 1
  }
})

test_that("process_fitted_params() works with CmdStanMCMC", {
  fit <- testing_fit("logistic", method = "sample", seed = 123)
  fit_params_files <- process_fitted_params(fit)
  expect_true(all(file.exists(fit_params_files)))
  chain <- 1
  for(file in fit_params_files) {
    if (os_is_windows()) {
      grep_path <- repair_path(Sys.which("grep.exe"))
      fread_cmd <- paste0(grep_path, " -v '^#' --color=never ", file)
    } else {
      fread_cmd <- paste0("grep -v '^#' --color=never ", file)
    }
    suppressWarnings(
      fit_params_tmp <- data.table::fread(
        cmd = fread_cmd
      )
    )
    fit_params_tmp <- posterior::as_draws_array(fit_params_tmp)
    posterior::variables(fit_params_tmp) <- repair_variable_names(posterior::variables(fit_params_tmp))
    expect_equal(
      posterior::subset_draws(fit$draws(), variable = "lp__", chain = chain),
      posterior::subset_draws(fit_params_tmp, variable = "lp__")
    )
    expect_equal(
      posterior::subset_draws(fit$draws(), variable = c("alpha", "beta[1]", "beta[2]", "beta[3]"), chain = chain),
      posterior::subset_draws(fit_params_tmp, variable = c("alpha", "beta[1]", "beta[2]", "beta[3]"),)
    )
    chain <- chain + 1
  }
})

test_that("process_fitted_params() works with draws_array", {
  fit <- testing_fit("logistic", method = "sample", seed = 123)
  fit_params_files <- process_fitted_params(fit$draws())
  expect_true(all(file.exists(fit_params_files)))
  chain <- 1
  for(file in fit_params_files) {
    if (os_is_windows()) {
      grep_path <- repair_path(Sys.which("grep.exe"))
      fread_cmd <- paste0(grep_path, " -v '^#' --color=never ", file)
    } else {
      fread_cmd <- paste0("grep -v '^#' --color=never ", file)
    }
    suppressWarnings(
      fit_params_tmp <- data.table::fread(
        cmd = fread_cmd
      )
    )
    fit_params_tmp <- posterior::as_draws_array(fit_params_tmp)
    posterior::variables(fit_params_tmp) <- repair_variable_names(posterior::variables(fit_params_tmp))
    expect_equal(
      posterior::subset_draws(fit$draws(), variable = "lp__", chain = chain),
      posterior::subset_draws(fit_params_tmp, variable = "lp__")
    )
    expect_equal(
      posterior::subset_draws(fit$draws(), variable = c("alpha", "beta[1]", "beta[2]", "beta[3]"), chain = chain),
      posterior::subset_draws(fit_params_tmp, variable = c("alpha", "beta[1]", "beta[2]", "beta[3]"),)
    )
    chain <- chain + 1
  }
})

test_that("process_fitted_params() works with draws_array without lp__", {
  fit <- testing_fit("logistic", method = "sample", seed = 123)
  fit_params_files <- process_fitted_params(posterior::subset_draws(fit$draws(), variables = c("alpha", "beta[1]", "beta[2]", "beta[3]")))
  expect_true(all(file.exists(fit_params_files)))
  chain <- 1
  for(file in fit_params_files) {
    if (os_is_windows()) {
      grep_path <- repair_path(Sys.which("grep.exe"))
      fread_cmd <- paste0(grep_path, " -v '^#' --color=never ", file)
    } else {
      fread_cmd <- paste0("grep -v '^#' --color=never ", file)
    }
    suppressWarnings(
      fit_params_tmp <- data.table::fread(
        cmd = fread_cmd
      )
    )
    fit_params_tmp <- posterior::as_draws_array(fit_params_tmp)
    posterior::variables(fit_params_tmp) <- repair_variable_names(posterior::variables(fit_params_tmp))
    expect_equal(
      posterior::subset_draws(fit$draws(), variable = c("alpha", "beta[1]", "beta[2]", "beta[3]"), chain = chain),
      posterior::subset_draws(fit_params_tmp, variable = c("alpha", "beta[1]", "beta[2]", "beta[3]"),)
    )
    chain <- chain + 1
  }
})

test_that("process_fitted_params() works with CmdStanVB", {
  fit <- testing_fit("logistic", method = "variational", seed = 123)
  file <- process_fitted_params(fit)
  expect_true(file.exists(file))
  if (os_is_windows()) {
    grep_path <- repair_path(Sys.which("grep.exe"))
    fread_cmd <- paste0(grep_path, " -v '^#' --color=never ", file)
  } else {
    fread_cmd <- paste0("grep -v '^#' --color=never ", file)
  }
  suppressWarnings(
    fit_params_tmp <- data.table::fread(
      cmd = fread_cmd
    )
  )
  fit_params_tmp <- posterior::as_draws_array(fit_params_tmp)
  posterior::variables(fit_params_tmp) <- repair_variable_names(posterior::variables(fit_params_tmp))
  expect_equal(
    posterior::subset_draws(posterior::as_draws_array(fit$draws()), variable = "lp__"),
    posterior::subset_draws(fit_params_tmp, variable = "lp__")
  )
  expect_equal(
    posterior::subset_draws(posterior::as_draws_array(fit$draws()), variable = c("alpha", "beta[1]", "beta[2]", "beta[3]")),
    posterior::subset_draws(fit_params_tmp, variable = c("alpha", "beta[1]", "beta[2]", "beta[3]"))
  )
})

test_that("process_fitted_params() works with draws_matrix", {
  fit <- testing_fit("logistic", method = "variational", seed = 123)
  file <- process_fitted_params(fit$draws())
  expect_true(file.exists(file))
  if (os_is_windows()) {
    grep_path <- repair_path(Sys.which("grep.exe"))
    fread_cmd <- paste0(grep_path, " -v '^#' --color=never ", file)
  } else {
    fread_cmd <- paste0("grep -v '^#' --color=never ", file)
  }
  suppressWarnings(
    fit_params_tmp <- data.table::fread(
      cmd = fread_cmd
    )
  )
  fit_params_tmp <- posterior::as_draws_array(fit_params_tmp)
  posterior::variables(fit_params_tmp) <- repair_variable_names(posterior::variables(fit_params_tmp))
  expect_equal(
    posterior::subset_draws(posterior::as_draws_array(fit$draws()), variable = "lp__"),
    posterior::subset_draws(fit_params_tmp, variable = "lp__")
  )
  expect_equal(
    posterior::subset_draws(posterior::as_draws_array(fit$draws()), variable = c("alpha", "beta[1]", "beta[2]", "beta[3]")),
    posterior::subset_draws(fit_params_tmp, variable = c("alpha", "beta[1]", "beta[2]", "beta[3]"))
  )
})

test_that("process_data() errors on missing variables", {
  stan_file <- write_stan_file("
  data {
    real val1;
    real val2;
  }
  ")
  mod <- cmdstan_model(stan_file, compile = FALSE)
  expect_error(
    process_data(data = list(val1 = 5), model_variables = mod$variables()),
    "Missing input data for the following data variables: val2."
  )
  expect_error(
    process_data(data = list(val = 1), model_variables = mod$variables()),
    "Missing input data for the following data variables: val1, val2."
  )
  stan_file_no_data <- write_stan_file("
  transformed data {
    real val1 = 1;
    real val2 = 2;
  }
  ")
  mod <- cmdstan_model(stan_file_no_data, compile = FALSE)
  v <- process_data(data = list(val1 = 5), model_variables = mod$variables())
  expect_type(v, "character")
})

test_that("process_data() corrrectly casts integers and floating point numbers", {
  stan_file <- write_stan_file("
  data {
    int a;
    real b;
  }
  ")
  mod <- cmdstan_model(stan_file, compile = FALSE)
  test_file <- process_data(list(a = 1, b = 2), model_variables = mod$variables())
  expect_match(
    "  \"a\": 1,",
    readLines(test_file)[2],
    fixed = TRUE
  )
  expect_match(
    "  \"b\": 2.0",
    readLines(test_file)[3],
    fixed = TRUE
  )
  test_file <- process_data(list(a = 1L, b = 1774000000), model_variables = mod$variables())
  expect_match(
    "  \"a\": 1,",
    readLines(test_file)[2],
    fixed = TRUE
  )
  expect_match(
    "  \"b\": 1774000000.0",
    readLines(test_file)[3],
    fixed = TRUE
  )

  stan_file <- write_stan_file("
  data {
    array[3,3] int<lower=0> k;
  }
  ")
  mod <- cmdstan_model(stan_file, compile = FALSE)
  test_file <- process_data(list(k = matrix(c(18, 18, 16, 13, 9, 6, 4, 4, 4), nrow=3, ncol=3, byrow=T)), model_variables = mod$variables())
  expect_match(
    "  \"k\": [",
    readLines(test_file)[2],
    fixed = TRUE
  )
  expect_match(
    "    [18, 18, 16],",
    readLines(test_file)[3],
    fixed = TRUE
  )
})

test_that("process_data warns on int coercion", {
  stan_file <- write_stan_file("
  data {
    int a;
    real b;
  }
  ")
  mod <- cmdstan_model(stan_file, compile = FALSE)
  expect_warning(
    process_data(list(a = 1.1, b = 2.1), model_variables = mod$variables()),
    "A non-integer value was supplied for 'a'! It will be truncated to an integer."
  )

  stan_file <- write_stan_file("
  data {
    array[3] int a;
  }
  ")
  mod <- cmdstan_model(stan_file, compile = FALSE)
  expect_warning(
    process_data(list(a = c(1, 2.1, 3)), model_variables = mod$variables()),
    "A non-integer value was supplied for 'a'! It will be truncated to an integer."
  )

  expect_no_warning(
    process_data(list(a = c(1, 2, 3)), model_variables = mod$variables())
  )
  expect_no_warning(
    process_data(list(a = factor(c("a", "b", "c"))), model_variables = mod$variables())
  )
})

test_that("process_data accepts lists of matrices/vectors for int variables", {
  stan_file <- write_stan_file("
  data {
    array[4,3,2] int x;
  }
  ")
  mod <- cmdstan_model(stan_file, compile = FALSE)
  model_variables <- mod$variables()

  a <- matrix(1:6, nrow = 3, ncol = 2)
  arr <- array(dim = c(4, 3, 2))
  for (i in 1:4) arr[i, , ] <- a

  from_array <- readLines(process_data(list(x = arr), model_variables = model_variables))
  from_int_list <- readLines(process_data(list(x = list(a, a, a, a)), model_variables = model_variables))
  expect_identical(from_int_list, from_array)

  # a list of doubles must give the same result as a list of integers (#817)
  storage.mode(a) <- "double"
  from_dbl_list <- readLines(process_data(list(x = list(a, a, a, a)), model_variables = model_variables))
  expect_identical(from_dbl_list, from_array)

  # values are written as integers, not as decimals
  expect_false(any(grepl(".", from_dbl_list, fixed = TRUE)))

  stan_file <- write_stan_file("
  data {
    array[2,3] int x;
  }
  ")
  mod <- cmdstan_model(stan_file, compile = FALSE)
  test_file <- process_data(list(x = list(c(1, 2, 3), c(4, 5, 6))), model_variables = mod$variables())
  expect_equal(
    jsonlite::read_json(test_file, simplifyVector = TRUE),
    list(x = matrix(1:6, nrow = 2, ncol = 3, byrow = TRUE))
  )
})

test_that("process_data accepts data frames for int variables", {
  stan_file <- write_stan_file("
  data {
    array[2,2] int x;
  }
  ")
  mod <- cmdstan_model(stan_file, compile = FALSE)
  model_variables <- mod$variables()

  df <- data.frame(a = c(1, 2), b = c(3, 4))
  from_df <- readLines(process_data(list(x = df), model_variables = model_variables))
  from_matrix <- readLines(process_data(list(x = data.matrix(df)), model_variables = model_variables))
  expect_identical(from_df, from_matrix)
  expect_false(any(grepl(".", from_df, fixed = TRUE)))
})

test_that("process_data errors on invalid types", {
  stan_file <- write_stan_file("
  data {
    array[2,2] int x;
  }
  ")
  mod <- cmdstan_model(stan_file, compile = FALSE)
  model_variables <- mod$variables()

  expect_error(
    process_data(list(x = c("v", "w")), model_variables = model_variables),
    "Variable 'x' is of invalid type."
  )
  # NAs inside a list are reported as NAs rather than as a coercion failure
  expect_error(
    process_data(list(x = list(c(1, NA), c(3, 4))), model_variables = model_variables),
    "Variable 'x' has NA values"
  )
})

test_that("Floating-point differences do not cause truncation towards 0", {
  stan_file <- write_stan_file("
  data {
    int a;
    real b;
  }
  ")
  mod <- cmdstan_model(stan_file, compile = FALSE)
  a <- 10*(3-2.7)
  expect_false(is.integer(a))
  test_file <- process_data(list(a = a, b = 2.0), model_variables = mod$variables())
  expect_match(
    "  \"a\": 3,",
    readLines(test_file)[2],
    fixed = TRUE
  )
})
