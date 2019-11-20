context("install")

test_that("install_cmdstan() successfully installs cmdstan", {
  skip_if_offline()
  dir <- tempdir(check = TRUE)
  expect_message(
    expect_output(
      install_cmdstan(dir = dir, cores = 2, quiet = FALSE, overwrite = TRUE),
      "Compiling, linking C++ code",
      fixed = TRUE
    ),
    "CmdStan path set"
  )
})

test_that("install_cmdstan() errors if installation already exists", {
  skip_if_offline()
  if (not_on_cran()) {
    dir <- NULL
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

test_that("internal clone_repo() function clones the repo", {
  skip_on_covr() 
  skip_on_cran()
  skip_if_offline()
  clone_dir <- tempfile(tmpdir = tempdir(check=TRUE))
  if (!dir.exists(clone_dir)) {
    dir.create(clone_dir)
  }

  clone_log <- clone_repo(
    dir = clone_dir,
    repo_url = "https://github.com/stan-dev/cmdstan.git",
    repo_branch = "develop",
    quiet = FALSE
  )
  expect_equal(clone_log$status, 0)
  checkmate::expect_directory_exists(file.path(clone_dir, "stan"))
  checkmate::expect_directory_exists(file.path(clone_dir, "stan", "lib", "stan_math"))
})


