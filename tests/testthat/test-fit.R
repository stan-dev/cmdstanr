if (not_on_cran()) {
  set_cmdstan_path()
  stan_program <- test_path("resources/stan/logistic.stan")
  data_file_json <- test_path("resources/data/logistic.data.json")
  data_list <- jsonlite::read_json(data_file_json, simplifyVector = TRUE)
  mod <- cmdstan_model(stan_file = stan_program)
  utils::capture.output(
    fit_mcmc <- mod$sample(data = data_list, num_chains = 2,
                           save_diagnostics = TRUE)
  )
  utils::capture.output(
    fit_mcmc_0 <- mod$sample(data = data_file_json, num_chains = 2,
                             refresh = 0)
  )
  utils::capture.output(suppressWarnings(
    fit_mle <- mod$optimize(data = data_list, save_diagnostics = FALSE)
  ))
  utils::capture.output(suppressWarnings(
    fit_vb <- mod$variational(data = data_list, save_diagnostics = TRUE)
  ))

  PARAM_NAMES <- c("alpha", "beta[1]", "beta[2]", "beta[3]")

  # cleanup
  file.remove(paste0(strip_ext(mod$exe_file()), delete_extensions()))
}


# CmdStanMCMC -------------------------------------------------------------
context("fit-objects-mcmc")

test_that("saving csv mcmc output works", {
  skip_on_cran()
  old_paths <- fit_mcmc$output_files()
  checkmate::expect_file_exists(old_paths, extension = "csv")

  expect_message(
    paths <- fit_mcmc$save_output_files(tempdir(), basename = "testing-mcmc-output"),
    "Moved 2 output files and set internal paths to new locations"
  )
  checkmate::expect_file_exists(paths, extension = "csv")

  should_match <- paste0("testing-mcmc-output-",
                         format(Sys.time(), "%Y%m%d%H%M"),
                         "-",
                         1:2)
  expect_match(paths[1], should_match[1])
  expect_match(paths[2], should_match[2])

  expect_false(any(file.exists(old_paths)))
  expect_equal(fit_mcmc$output_files(), paths)
})

test_that("saving data file after mcmc works", {
  skip_on_cran()
  old_path <- fit_mcmc$data_file()
  checkmate::expect_file_exists(old_path, extension = "json")

  expect_message(
    path <- fit_mcmc$save_data_file(tempdir(), basename = "testing-mcmc-data"),
    "Moved data file and set internal path to new location"
  )
  checkmate::expect_file_exists(path, extension = "json")
  expect_match(path, "testing-mcmc-data-")

  expect_false(file.exists(old_path))
  expect_equal(fit_mcmc$data_file(), path)
})

test_that("saving diagnostic csv mcmc output works", {
  skip_on_cran()
  old_paths <- fit_mcmc$diagnostic_files()
  checkmate::expect_file_exists(old_paths, extension = "csv")

  expect_message(
    paths <- fit_mcmc$save_diagnostic_files(tempdir(), basename = "testing-mcmc"),
    "Moved 2 diagnostic files and set internal paths to new locations"
  )
  checkmate::expect_file_exists(paths, extension = "csv")
  expect_true(all(file.size(paths) > 0))

  should_match <- paste0("testing-mcmc-diagnostic-",
                         format(Sys.time(), "%Y%m%d%H%M"),
                         "-",
                         1:2)

  expect_match(paths[1], should_match[1])
  expect_match(paths[2], should_match[2])

  expect_false(any(file.exists(old_paths)))
  expect_equal(fit_mcmc$diagnostic_files(), paths)
})

test_that("draws() method returns posterior sample (reading csv works)", {
  skip_on_cran()
  draws <- fit_mcmc$draws()
  expect_type(draws, "double")
  expect_s3_class(draws, "draws_array")
  expect_equal(posterior::variables(draws), c(PARAM_NAMES, "lp__"))
})

test_that("summary() method works after mcmc", {
  skip_on_cran()
  x <- fit_mcmc$summary()
  expect_s3_class(x, "data.frame")
  expect_equal(x$variable, c(PARAM_NAMES, "lp__"))
})

test_that("cmdstan_summary() method succesfully calls bin/stansummary after mcmc", {
  skip_on_cran()
  expect_output(
    fit_mcmc$cmdstan_summary(),
    "Inference for Stan model: logistic_model",
    fixed = TRUE
  )
})

test_that("cmdstan_diagnose() method succesfully calls bin/diagnose after mcmc", {
  skip_on_cran()
  expect_output(fit_mcmc$cmdstan_diagnose(), "Checking sampler transitions for divergences")
})

test_that("output() method works after mcmc", {
  skip_on_cran()
  checkmate::expect_list(
    fit_mcmc$output(),
    types = "character",
    any.missing = FALSE,
    len = fit_mcmc$runset$num_runs()
  )
  expect_output(fit_mcmc$output(id = 1), "Gradient evaluation took")
})

test_that("time() method works after mcmc", {
  skip_on_cran()
  run_times <- fit_mcmc$time()
  checkmate::expect_list(run_times, names = "strict", any.missing = FALSE)
  testthat::expect_named(run_times, c("total", "chains"))
  checkmate::expect_number(run_times$total, finite = TRUE)
  checkmate::expect_data_frame(
    run_times$chains,
    any.missing = FALSE,
    types = c("integer", "numeric"),
    nrows = fit_mcmc$runset$num_runs(),
    ncols = 4
  )

  # after refresh=0 warmup and sampling times should be NA
  testthat::expect_warning(
    run_times_0 <- fit_mcmc_0$time(),
    "Separate warmup and sampling times are not available"
  )
  checkmate::expect_number(run_times_0$total, finite = TRUE)
  checkmate::expect_data_frame(run_times_0$chains,
                               any.missing = TRUE,
                               types = c("integer", "numeric"),
                               nrows = fit_mcmc_0$runset$num_runs(),
                               ncols = 4)
  for (j in 1:nrow(run_times_0$chains)) {
    checkmate::expect_scalar_na(run_times_0$chains$warmup[j])
    checkmate::expect_scalar_na(run_times_0$chains$sampling[j])
  }
})


# CmdStanMLE --------------------------------------------------------------
context("fit-objects-mle")

test_that("saving csv optimzation output works", {
  skip_on_cran()
  checkmate::expect_file_exists(fit_mle$output_files(), extension = "csv")

  expect_message(
    path <- fit_mle$save_output_files(tempdir(), basename = "testing-mle-output",
                                      timestamp = FALSE, random = FALSE),
    "Moved 1 output files and set internal paths to new locations"
  )
  expect_length(path, 1)
  checkmate::expect_file_exists(path, extension = "csv")
  expect_equal(basename(path), "testing-mle-output-1.csv")
})

test_that("saving data file after optimization works", {
  skip_on_cran()

  checkmate::expect_file_exists(fit_mle$data_file(), extension = "json")

  expect_message(
    path <- fit_mle$save_data_file(tempdir(), basename = "testing-mle-data",
                                   timestamp = FALSE, random = TRUE),
    "Moved data file and set internal path to new location"
  )
  checkmate::expect_file_exists(path, extension = "json")
  expect_match(basename(path), "testing-mle-data-")
})

test_that("saving diagnostics files errors if don't exist", {
  skip_on_cran()
  expect_error(fit_mle$save_diagnostic_files(), "No diagnostic files found")
  expect_error(fit_mle$diagnostic_files(), "No diagnostic files found")
})

test_that("reading in csv optimization output works", {
  skip_on_cran()
  expect_named(fit_mle$mle(), PARAM_NAMES)
  expect_named(fit_mle$lp(), "lp__")
})

test_that("summary method doesn't error for optimization", {
  skip_on_cran()
  # summary method for optimization isn't fully designed yet
  expect_output(fit_mle$summary(), "Estimates from optimization")
})

# CmdStanVB -------------------------------------------------------------
context("fit-objects-vb")

test_that("saving csv variational output works", {
  skip_on_cran()

  old_path <- fit_vb$output_files()
  checkmate::expect_file_exists(old_path, extension = "csv")

  expect_message(
    path <- fit_vb$save_output_files(tempdir(), basename = "testing-vb-output",
                                      timestamp = FALSE, random = TRUE),
    "Moved 1 output files and set internal paths to new locations"
  )
  expect_false(file.exists(old_path))
  expect_length(path, 1)
  checkmate::expect_file_exists(path, extension = "csv")
  expect_match(path, "testing-vb-output-1-")
})

test_that("saving data file after variational works", {
  skip_on_cran()
  checkmate::expect_file_exists(fit_vb$data_file(), extension = "json")

  path <- fit_vb$save_data_file(tempdir(), basename = "testing-vb-data")
  checkmate::expect_file_exists(path, extension = "json")
  expect_match(path, "testing-vb-data-")
})

test_that("saving diagnostic csv from variational works", {
  skip_on_cran()
  checkmate::expect_file_exists(fit_vb$diagnostic_files(), extension = "csv")

  paths <- fit_vb$save_diagnostic_files(tempdir(), basename = "testing-vb",
                                        timestamp = FALSE)
  expect_true(all(file.exists(paths)))
  expect_true(all(file.size(paths) > 0))
  checkmate::expect_file_exists(paths, extension = "csv")
  expect_match(paths[1], "testing-vb-diagnostic-1")
})

test_that("summary() method works after vb", {
  skip_on_cran()
  x <- fit_vb$summary()
  expect_s3_class(x, "data.frame")
  expect_equal(x$variable, PARAM_NAMES)

  x <- fit_vb$summary(measures = c("mean", "sd"))
  expect_s3_class(x, "data.frame")
  expect_equal(x$variable, PARAM_NAMES)
  expect_equal(colnames(x), c("variable", "mean", "sd"))
})

test_that("cmdstan_summary() method after variational works", {
  skip_on_cran()
  expect_output(fit_vb$cmdstan_summary(), "Inference for Stan model")
})

test_that("draws() method returns posterior sample (reading csv works)", {
  skip_on_cran()
  draws <- fit_vb$draws()
  expect_type(draws, "double")
  expect_s3_class(draws, "draws_matrix")
  expect_equal(posterior::variables(draws), PARAM_NAMES)
})

test_that("log_p(), log_g() methods return vectors (reading csv works)", {
  skip_on_cran()
  lp <- fit_vb$log_p()
  lg <- fit_vb$log_g()
  expect_type(lp, "double")
  expect_type(lg, "double")
  expect_equal(length(lp), nrow(fit_vb$draws()))
  expect_equal(length(lg), nrow(fit_vb$draws()))
})



# CmdStanRun ------------------------------------------------------------------

context("runset (CmdStanRun)")
test_that("CmdStanRun (runset) methods return valid output", {
  skip_on_cran()

  runset <- fit_mcmc$runset
  checkmate::expect_r6(runset$args, "CmdStanArgs")
  checkmate::expect_r6(runset$procs, "CmdStanProcs")
  checkmate::expect_list(
    runset$command_args(),
    types = "character",
    len = runset$num_runs(),
    unique = TRUE
  )
  expect_equal(runset$model_name(), "logistic")
  expect_equal(runset$method(), "sample")
  expect_equal(runset$run_ids(), seq_len(runset$num_runs()))
  expect_equal(runset$run_ids(), seq_len(runset$num_chains()))
  checkmate::expect_file_exists(runset$data_file())
  checkmate::expect_file_exists(runset$output_files())
  checkmate::expect_file_exists(runset$diagnostic_files())
})

