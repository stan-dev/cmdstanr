context("model-compile")

if (not_on_cran()) {
  set_cmdstan_path()
  stan_program <- cmdstan_example_file()
  mod <- cmdstan_model(stan_file = stan_program, compile = FALSE)
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
  # remove executable if exists
  exe <- cmdstan_ext(strip_ext(mod$stan_file()))
  if (file.exists(exe)) {
    file.remove(exe)
  }
  expect_message(mod$compile(quiet = TRUE), "Compiling Stan program...")
  expect_message(mod$compile(quiet = TRUE), "Model executable is up to date!")

  if (file.exists(exe)) {
    file.remove(exe)
  }
  out <- utils::capture.output(mod$compile(quiet = FALSE))
  expect_output(print(out), "Translating Stan model")

})

test_that("compile() method forces recompilation force_recompile = TRUE", {
  skip_on_cran()
  mod$compile(quiet = TRUE)
  expect_message(mod$compile(quiet = TRUE, force_recompile = TRUE), "Compiling Stan program...")
})

test_that("compile() method forces recompilation if model modified", {
  skip_on_cran()
  # remove executable if exists
  exe <- cmdstan_ext(strip_ext(mod$stan_file()))
  if (!file.exists(exe)) {
    mod$compile(quiet = TRUE)
  }
  Sys.setFileTime(mod$stan_file(), Sys.time()) #touch file to trigger recompile
  expect_message(mod$compile(quiet = TRUE), "Compiling Stan program...")
})

test_that("compile() method works with spaces in path", {
  skip_on_cran()
  stan_file <- testing_stan_file("bernoulli")
  stan_model_with_spaces <- testing_stan_file("folder spaces/bernoulli spaces")

  dir_with_spaces <- test_path("resources", "stan", "folder spaces")
  if (!file.exists(dir_with_spaces)) {
    dir.create(dir_with_spaces)
  }
  file.copy(stan_file, stan_model_with_spaces)

  mod_spaces <- cmdstan_model(stan_file = stan_model_with_spaces, compile = FALSE)
  exe <- cmdstan_ext(strip_ext(mod_spaces$stan_file()))
  if (file.exists(exe)) {
    file.remove(exe)
  }
  expect_message(mod_spaces$compile(), "Compiling Stan program...")
  file.remove(stan_model_with_spaces)
  file.remove(exe)
  file.remove(dir_with_spaces)
})

test_that("compile() method overwrites binaries", {
  skip_on_cran()
  mod$compile(quiet = TRUE)
  old_time = file.mtime(mod$exe_file())
  mod$compile(quiet = TRUE, force_recompile = TRUE)
  new_time =
  expect_gt(file.mtime(mod$exe_file()), old_time)
})

test_that("compilation works with include_paths", {
  skip_on_cran()

  stan_program_w_include <- testing_stan_file("bernoulli_include")
  exe <- cmdstan_ext(strip_ext(stan_program_w_include))
  if(file.exists(exe)) {
    file.remove(exe)
  }
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
  expect_equal(
    mod_w_include$exe_file(),
    cmdstan_ext(strip_ext(absolute_path(stan_program_w_include)))
  )
})

test_that("name in STANCFLAGS is set correctly", {
  skip_on_cran()
  out <- utils::capture.output(mod$compile(quiet = FALSE, force_recompile = TRUE))
  if(os_is_windows()) {
    out_no_name <- "bin/stanc.exe --name='bernoulli_model' --o"
    out_name <- "bin/stanc.exe --name='bernoulli2_model' --o"
  } else {
    out_no_name <- "bin/stanc --name='bernoulli_model' --o"
    out_name <- "bin/stanc --name='bernoulli2_model' --o"
  }
  expect_output(print(out), out_no_name)
  out <- utils::capture.output(mod$compile(quiet = FALSE, force_recompile = TRUE, stanc_options = list(name = "bernoulli2_model")))
  expect_output(print(out), out_name)
})

test_that("name in STANCFLAGS is set correctly", {
  skip_on_cran()
  out <- utils::capture.output(mod$compile(quiet = FALSE, force_recompile = TRUE))
  if(os_is_windows()) {
    out_no_name <- "bin/stanc.exe --name='bernoulli_model' --o"
    out_name <- "bin/stanc.exe --name='bernoulli2_model' --o"
  } else {
    out_no_name <- "bin/stanc --name='bernoulli_model' --o"
    out_name <- "bin/stanc --name='bernoulli2_model' --o"
  }
  expect_output(print(out), out_no_name)
  out <- utils::capture.output(mod$compile(quiet = FALSE, force_recompile = TRUE, stanc_options = list(name = "bernoulli2_model")))
  expect_output(print(out), out_name)
})

test_that("recompiles on change of make flags", {
  skip_on_cran()
  #remove all flags
  set_cmdstan_cpp_options(cpp_options = list())

  expect_true(set_cmdstan_cpp_options(cpp_options = list(stan_threads = TRUE)))
  expect_false(set_cmdstan_cpp_options(cpp_options = list(stan_threads = TRUE)))
  expect_true(set_cmdstan_cpp_options(cpp_options = list()))
})
