context("install")

test_that("test cmdstan installation", {
  skip_if_offline()
  skip_on_covr()

  if (not_on_cran()) {
    dir <- dirname(cmdstan_default_path())
  } else {
    dir <- tempdir()
  }
  expect_message(
    expect_output(
      install_cmdstan(dir = dir, cores = 2, quiet = FALSE, overwrite = TRUE),
      "Compiling, linking C++ code",
      fixed = TRUE
    ),
    "CmdStan path set"
  )
})

test_that("Test cmdstan installation error", {
  skip_if_offline()
  if (not_on_cran()) {
    dir <- dirname(cmdstan_default_path())
  } else {
    dir <- tempdir()
  }
  if (dir.exists(file.path(dir, "cmdstan"))) {
    expect_warning(
      install_cmdstan(dir = dir, overwrite = FALSE),
      "An installation already exists"
    )
  }
})
