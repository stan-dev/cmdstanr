# Setup -------------------------------------------------------------------
NOT_CRAN <- identical(Sys.getenv("NOT_CRAN"), "true")

context("Installation")

test_that("test cmdstan installation", {
  skip_if_offline()
  skip_if(identical(Sys.getenv("R_COVR"), "true"), message = "R_COVR")

  if (NOT_CRAN) {
    dir <- dirname(cmdstan_default_path())
  } else {
    dir <- tempdir()
  }
  expect_message(
    expect_output(
      install_log <- install_cmdstan(dir = dir, cores = 2, quiet = FALSE, overwrite = TRUE),
      "Finished installing CmdStan"
    ),
    "CmdStan path set"
  )
  expect_equal(install_log$status, 0)
})

test_that("Test cmdstan installation error", {
  skip_if_offline()
  if (NOT_CRAN) {
    dir <- dirname(cmdstan_default_path())
  } else {
    dir <- tempdir()
  }
  if (dir.exists(file.path(dir, "cmdstan"))) {
    expect_output(
      install_cmdstan(dir = dir, cores = 1, quiet = FALSE, overwrite = FALSE),
      "Please remove or rename the 'cmdstan' folder"
    )
  }
})
