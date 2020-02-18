context("fitted-shared-methods")

if (not_on_cran()) {
  set_cmdstan_path()

  fits <- list()
  fits[["sample"]] <- testing_fit("logistic", method = "sample",
                                  seed = 123, save_diagnostics = TRUE)
  fits[["variational"]] <- testing_fit("logistic", method = "variational",
                                       seed = 123, save_diagnostics = TRUE)
  fits[["optimize"]] <- testing_fit("logistic", method = "optimize", seed = 123)
  all_methods <- c("sample", "optimize", "variational")
}


test_that("*_files() methods return the right number of paths", {
  skip_on_cran()
  for (method in all_methods) {
    expect_length(fits[[method]]$output_files(), fits[[method]]$num_runs())
    expect_length(fits[[method]]$data_file(), 1)
    if (method != "optimize") {
      expect_length(fits[[method]]$diagnostic_files(), fits[[method]]$num_runs())
    }
  }
})

test_that("saving csv output files works", {
  skip_on_cran()

  for (method in all_methods) {
    fit <- fits[[method]]
    old_paths <- fit$output_files()
    checkmate::expect_file_exists(old_paths, extension = "csv")

    expect_message(
      paths <- fit$save_output_files(tempdir(), basename = "testing-output"),
      paste("Moved", fit$num_runs(), "output files and set internal paths")
    )
    checkmate::expect_file_exists(paths, extension = "csv")
    expect_true(all(file.size(paths) > 0))

    should_match <- paste0("testing-output-",
                           format(Sys.time(), "%Y%m%d%H%M"),
                           "-",
                           seq_len(fit$num_runs()))
    for (j in seq_along(paths)) {
      expect_match(paths[j], should_match[j])
    }

    expect_false(any(file.exists(old_paths)))
    expect_equal(fit$output_files(), paths)
  }
})

test_that("saving diagnostic csv output works", {
  skip_on_cran()

  for (method in all_methods) {
    fit <- fits[[method]]
    if (method == "optimize") {
      expect_error(
        fit$save_diagnostic_files(),
        "No diagnostic files found. Set 'save_diagnostics=TRUE' when fitting the model",
        fixed = TRUE
      )
      next
    }

    old_paths <- fit$diagnostic_files()
    checkmate::expect_file_exists(old_paths, extension = "csv")

    expect_message(
      paths <- fit$save_diagnostic_files(tempdir(), basename = "testing-output"),
      paste("Moved", fit$num_runs(), "diagnostic files and set internal paths")
    )
    checkmate::expect_file_exists(paths, extension = "csv")
    expect_true(all(file.size(paths) > 0))

    should_match <- paste0("testing-output-diagnostic-",
                           format(Sys.time(), "%Y%m%d%H%M"),
                           "-",
                           seq_len(fit$num_runs()))

    for (j in seq_along(paths)) {
      expect_match(paths[j], should_match[j])
    }

    expect_false(any(file.exists(old_paths)))
    expect_equal(fit$diagnostic_files(), paths)
  }
})

test_that("saving data file works", {
  skip_on_cran()

  for (method in all_methods) {
    fit <- fits[[method]]
    old_path <- fit$data_file()
    checkmate::expect_file_exists(old_path, extension = "json")

    expect_message(
      path <- fit$save_data_file(tempdir(), basename = NULL,
                                 timestamp = FALSE, random = FALSE),
      "Moved data file and set internal path"
    )
    checkmate::expect_file_exists(path, extension = "json")
    expect_true(file.size(path) > 0)
    expect_equal(basename(path), "logistic.json")

    expect_false(file.exists(old_path))
    expect_equal(fit$data_file(), path)
  }
})

test_that("cmdstan_summary() and cmdstan_diagnose() work correctly", {
  skip_on_cran()
  for (method in all_methods) {
    fit <- fits[[method]]
    if (method == "optimize") {
      expect_error(fit$cmdstan_summary(), "Not available for optimize method")
      expect_error(fit$cmdstan_diagnose(), "Not available for optimize method")
    } else {
      expect_output(fit$cmdstan_summary(), "Inference for Stan model")
      expect_output(fit$cmdstan_diagnose(), "Processing complete")
    }
  }
})

test_that("draws() method returns a 'draws' object", {
  skip_on_cran()
  for (method in all_methods) {
    fit <- fits[[method]]
    draws <- fit$draws()
    expect_type(draws, "double")
    expect_s3_class(draws, "draws")
  }
})

test_that("time() method errors for optimization and variational", {
  skip_on_cran()

  # further testing for time() for mcmc in test-fit-mcmc.R
  expect_silent(fits[["sample"]]$time())

  # error for now until this method is implemented
  expect_error(fits[["optimize"]]$time(), "Not yet implemented")
  expect_error(fits[["variational"]]$time(), "Not yet implemented")
})
