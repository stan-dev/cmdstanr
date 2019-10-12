context("Installation")
test_that("test cmdstan installation", {
  skip_on_cran()

  expect_output(
    log <- install_cmdstan(dir = NULL, cores = 2, quiet = FALSE),
    "CmdStan installation location:"
  )
  expect_equal(log$status, 0)
})
