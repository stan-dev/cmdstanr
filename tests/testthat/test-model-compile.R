context("model-compile")

if (not_on_cran()) {
  set_cmdstan_path()
  stan_program <- beroulli_example_file()
  mod <- cmdstan_model(stan_file = stan_program, compile = FALSE)

  stan_program_different_dir <- test_path("resources", "stan", "bernoulli.stan")
  stan_program_w_include <- test_path("resources", "stan", "bernoulli_include.stan")
}

test_that("object initialized correctly", {
  skip_on_cran()
  expect_equal(mod$stan_file(), stan_program)
  expect_equal(mod$exe_file(), character(0))
})

test_that("error if no compile() before model fitting", {
  skip_on_cran()
  expect_error(
    mod$sample(),
    "Model not compiled. Try running the compile() method first.",
    fixed = TRUE
  )
})

test_that("compile() method works", {
  skip_on_cran()
  expected <- if (!file.exists(cmdstan_ext(strip_ext(mod$stan_file()))))
    "Translating Stan model" else "is up to date"
  out <- utils::capture.output(mod$compile(quiet = FALSE))
  expect_output(print(out), expected)
  expect_equal(mod$exe_file(), cmdstan_ext(strip_ext(stan_program)))
})

test_that("compile() method forces recompilation if changes in flags", {
  skip_on_cran()
  skip_on_travis() # FIXME should work on travis

  expect_message(
    mod$compile(threads=TRUE),
    "change in the compiler flags was found"
  )

  # change it back
  expect_message(
    mod$compile(threads=FALSE),
    "change in the compiler flags was found"
  )
})

test_that("compilation works when stan program not in cmdstan dir", {
  skip_on_cran()

  expect_message(
    mod_2 <- cmdstan_model(stan_file = stan_program_different_dir, quiet = TRUE),
    "Compiling Stan program..."
  )
  expect_equal(mod_2$exe_file(), cmdstan_ext(strip_ext(absolute_path(stan_program_different_dir))))

  out <- utils::capture.output(
    mod_2 <- suppressMessages(cmdstan_model(stan_file = stan_program_different_dir, quiet = FALSE))
  )
  expect_output(print(out), "is up to date")

  # cleanup
  file.remove(paste0(strip_ext(mod_2$exe_file()), delete_extensions()))
})

test_that("compilation works with include_paths", {
  skip_on_cran()

  expect_error(
    cmdstan_model(stan_file = stan_program_w_include, include_paths = "NOT_A_DIR",
                  quiet = TRUE),
    "Directory 'NOT_A_DIR' does not exist"
  )

  expect_error(
    expect_output(
      cmdstan_model(stan_file = stan_program_w_include, quiet = TRUE),
      "could not find include file"
    )
  )

  expect_message(
    mod_w_include <- cmdstan_model(stan_file = stan_program_w_include, quiet = TRUE,
                                   include_paths = test_path("resources", "stan")),
    "Compiling Stan program"
  )
  expect_equal(mod_w_include$exe_file(), cmdstan_ext(strip_ext(absolute_path(stan_program_w_include))))

  # cleanup
  file.remove(paste0(strip_ext(mod_w_include$exe_file()), delete_extensions()))
})
