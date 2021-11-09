context("model-output_dir-output-basename")

set_cmdstan_path()
if (getRversion() < '3.5.0') {
  sandbox <- file.path(tempdir(), "sandbox")
} else {
  sandbox <- file.path(tempdir(check = TRUE), "sandbox")
}
if (!dir.exists(sandbox)) {
  dir.create(sandbox)
}

test_that("all fitting methods work with output_dir", {
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
    expect_equal(length(list.files(method_dir)), fit$num_procs())


    # specifying output_dir
    fit <- testing_fit("bernoulli", method = method, seed = 123,
                       output_basename = "custom")
    n_files <- length(fit$output_files())
    files <- paste0("custom-", 1:n_files, ".csv")
    expect_equal(basename(fit$output_files()), files)
  }

  # specifying output_dir and save_latent_dynamics
  fit <- testing_fit("bernoulli", method = "sample", seed = 123,
                     output_dir = file.path(sandbox, "sample"),
                     save_latent_dynamics = TRUE)

  files <- list.files(file.path(sandbox, "sample"))
  expect_equal(
    sum(grepl("diagnostic", files)),
    fit$num_procs()
  )
})

test_that("error if output_dir is invalid", {
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

test_that("output_dir works with trailing /", {
  test_dir <- file.path(sandbox, "trailing")
  dir.create(test_dir)
  fit <- testing_fit(
    "bernoulli",
    method = "sample",
    seed = 123,
    output_dir = paste0(test_dir,"/")
  )
  expect_equal(fit$runset$args$output_dir, absolute_path(test_dir))
  expect_equal(length(list.files(test_dir)), fit$num_procs())
})
