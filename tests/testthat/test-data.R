context("data-utils")

if (not_on_cran()) {
  set_cmdstan_path()
  fit <- testing_fit("bernoulli", method = "sample", seed = 123)
  fit_vb <- testing_fit("bernoulli", method = "variational", seed = 123)
  fit_optimize <- testing_fit("bernoulli", method = "optimize", seed = 123)
}

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
  expect_error(
    process_fitted_params(5),
    "'fitted_params' should be a vector of paths or a CmdStanMCMC object."
  )
  expect_error(
    process_fitted_params(NULL),
    "'fitted_params' should be a vector of paths or a CmdStanMCMC object."
  )
  expect_error(
    process_fitted_params(fit_vb),
    "'fitted_params' should be a vector of paths or a CmdStanMCMC object."
  )
  expect_error(
    process_fitted_params(fit_optimize),
    "'fitted_params' should be a vector of paths or a CmdStanMCMC object."
  )

  fit_tmp <- testing_fit("bernoulli", method = "sample", seed = 123)
  temp_file <- tempfile(fileext = ".rds")
  saveRDS(fit_tmp, file = temp_file)
  rm(fit_tmp)
  gc()
  fit_tmp_null <- readRDS(temp_file)
  expect_error(
    process_fitted_params(fit_tmp_null),
    "Unable to obtain draws from the fit \\(CmdStanMCMC\\) object."
  )

  fit_tmp <- testing_fit("bernoulli", method = "sample", seed = 123)
  fit_tmp$draws()
  temp_file <- tempfile(fileext = ".rds")
  saveRDS(fit_tmp, file = temp_file)
  rm(fit_tmp)
  gc()
  fit_tmp_null <- readRDS(temp_file)
  expect_error(
    process_fitted_params(fit_tmp_null),
    "Unable to obtain sampler diagnostics from the fit \\(CmdStanMCMC\\) object."
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
      fread_cmd <- paste0(grep_path, " -v '^#' ", file)
    } else {
      fread_cmd <- paste0("grep -v '^#' ", file)
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


