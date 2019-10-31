# Setup -------------------------------------------------------------------
NOT_CRAN <-
  identical(Sys.getenv("NOT_CRAN"), "true") ||
  identical(Sys.getenv("TRAVIS"), "true")

context("Installation")

test_that("test cmdstan installation", {
  skip_if_offline()

  if (NOT_CRAN) {
    dir <- dirname(cmdstan_default_path())
  } else {
    dir <- tempdir()
  }
  expect_message(
    expect_output(
      install_log <- install_cmdstan(dir = dir, cores = 2, quiet = FALSE, overwrite = TRUE),
      "CmdStan installation location"
    ),
    paste0("Use set_cmdstan_path('", dir, "/cmdstan", "')"),
    fixed = TRUE
  )
  expect_equal(install_log$status, 0)
})
