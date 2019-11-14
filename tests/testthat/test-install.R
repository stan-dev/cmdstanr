context("install")

test_that("test cmdstan installation", {
  skip_if_offline()
  skip_if(on_codecov(), message = "R_COVR")

  if (not_on_cran()) {
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
  if (not_on_cran()) {
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
