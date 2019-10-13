# Setup -------------------------------------------------------------------
NOT_CRAN <-
  identical(Sys.getenv("NOT_CRAN"), "true") ||
  identical(Sys.getenv("TRAVIS"), "true")


context("Installation")

test_that("test cmdstan installation", {
  skip_if_offline()

  if (NOT_CRAN) {
    dir <- NULL
  } else {
    dir <- tempdir()
  }
  expect_output(
    install_log <- install_cmdstan(dir = dir, cores = 2, quiet = FALSE),
    "CmdStan installation location:"
  )
  expect_equal(install_log$status, 0)
})
