context("model-output_dir")

if (not_on_cran()) {
  set_cmdstan_path()
  if (getRversion() < '3.5.0') {
    sandbox <- file.path(tempdir(), "sandbox")
  } else {
    sandbox <- file.path(tempdir(check = TRUE), "sandbox")
  }
  if (!dir.exists(sandbox)) {
    dir.create(sandbox)
  }
}

test_that("all fitting methods work with output_dir", {
  skip_on_cran()
  for (method in c("sample", "optimize", "variational")) {
    method_dir <- file.path(sandbox, method)
    if (!dir.exists(method_dir)) {
      dir.create(method_dir)
    }

    # no output_dir means should use tempdir
    fit <- testing_fit("bernoulli", method = method, seed = 123)
    expect_equal(fit$runset$args$output_dir, absolute_path(tempdir()))

    # specifying output_dir
    fit <- testing_fit("bernoulli", method = method, seed = 123,
                        output_dir = method_dir)
    expect_equal(fit$runset$args$output_dir, absolute_path(method_dir))
    expect_equal(length(list.files(method_dir)), fit$num_runs())
  }

  # specifying output_dir and save_diagnostics
  fit <- testing_fit("bernoulli", method = "sample", seed = 123,
                     output_dir = file.path(sandbox, "sample"),
                     save_diagnostics = TRUE)

  files <- list.files(file.path(sandbox, "sample"))
  expect_equal(
    sum(grepl("diagnostic", files)),
    fit$num_runs()
  )
})

test_that("error if output_dir is invalid", {
  skip_on_cran()

  expect_error(
    testing_fit("bernoulli", output_dir = "NOT_A_DIR"),
    "Directory 'NOT_A_DIR' does not exist",
    fixed = TRUE
  )
  expect_error(
    testing_fit("bernoulli", output_dir = TRUE),
    "No directory provided"
  )

  if (!os_is_windows()) {
    # FIXME: how do I create an unreadable file on windows?
    not_readable <- file.path(sandbox, "locked")
    dir.create(not_readable, mode = "220")
    expect_error(
      testing_fit("bernoulli", output_dir = not_readable),
      "not readable"
    )
  }
  file.remove(list.files(sandbox, full.names = TRUE, recursive = TRUE))
})


