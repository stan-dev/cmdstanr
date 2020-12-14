context("data-utils")

if (not_on_cran()) {
  set_cmdstan_path()
  fit <- testing_fit("bernoulli", method = "sample", seed = 123)
  fit_vb <- testing_fit("bernoulli", method = "variational", seed = 123)
  fit_optimize <- testing_fit("bernoulli", method = "optimize", seed = 123)
}

test_that("empty data list converted to NULL", {
  expect_null(process_data(list()))
})

test_that("NAs detected in data list", {
  expect_false(any_na_elements(list(y = 1)))
  expect_true(any_na_elements(list(y = 1, N = NA)))
  expect_true(any_na_elements(list(x = matrix(NA, 1, 1))))
  expect_true(any_na_elements(list(x = list(1, NA))))
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
  error_msg <- "'fitted_params' must be a list of paths to CSV files, a CmdStanMCMC/CmdStanVB object, a posterior::draws_array or a posterior::draws_matrix."
  expect_error(
    process_fitted_params(5),
    error_msg
  )
  expect_error(
    process_fitted_params(NULL),
    error_msg
  )
  expect_error(
    process_fitted_params(fit_optimize),
    error_msg
  )

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
