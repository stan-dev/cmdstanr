test_that("profiling works if profiling data is present", {
  skip_on_cran()
  mod <- testing_model("logistic_profiling")
  utils::capture.output(
    fit <- mod$sample(data = testing_data("logistic"), refresh = 0)
  )
  expect_equal(length(fit$profile_files()), 4)
  profiles <- fit$profiles()
  expect_equal(length(profiles), 4)
  expect_equal(dim(profiles[[1]]), c(3,9))
  expect_equal(profiles[[1]][,"name"], c("glm", "priors", "udf"))
})

test_that("profiling errors if no profiling files are present", {
  skip_on_cran()
  mod <- testing_model("logistic")
  suppressWarnings(
    utils::capture.output(
      fit <- mod$sample(data = testing_data("logistic"), refresh = 0)
    )
  )
  expect_error(
    fit$profile_files(),
    "No profile files found. The model that produced the fit did not use any profiling.",
    fixed = TRUE
  )
  expect_error(fit$profiles(),
               "No profile files found. The model that produced the fit did not use any profiling.")
  expect_error(fit$save_profile_files(),
               "No profile files found. The model that produced the fit did not use any profiling.")
})

test_that("saving diagnostic csv output works", {
  skip_on_cran()
  mod <- testing_model("logistic_profiling")
  utils::capture.output(
    fit <- mod$sample(data = testing_data("logistic"), refresh = 0)
  )
  old_paths <- fit$profile_files()
  checkmate::expect_file_exists(old_paths, extension = "csv")

  expect_message(
    paths <- fit$save_profile_files(tempdir(), basename = "testing-output"),
    paste("Moved", fit$num_procs(), "files and set internal paths")
  )
  checkmate::expect_file_exists(paths, extension = "csv")
  expect_true(all(file.size(paths) > 0))

  expect_false(any(file.exists(old_paths)))
})

