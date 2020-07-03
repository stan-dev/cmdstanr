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
  expect_equal(mod$hpp_file(), character(0))
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
  checkmate::expect_file_exists(mod$hpp_file())
  checkmate::expect_file_exists(exe)
  file.remove(exe)
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
  Sys.setFileTime(mod$stan_file(), Sys.time() + 1) #touch file to trigger recompile
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


test_that("switching threads on and off works without rebuild", {
  skip_on_cran()
  main_path_o <- file.path(cmdstan_path(), "src", "cmdstan", "main.o")
  mod$compile(force_recompile = TRUE)

  before_mtime <- file.mtime(main_path_o)
  mod$compile(force_recompile = TRUE)
  after_mtime <- file.mtime(main_path_o)
  expect_equal(before_mtime, after_mtime)

  before_mtime <- file.mtime(main_path_o)
  mod$compile(force_recompile = TRUE, cpp_options = list(stan_threads = TRUE))
  after_mtime <- file.mtime(main_path_o)
  time_diff <- as.double((after_mtime - before_mtime), units = "secs")
  expect_gt(time_diff, 0)

  before_mtime <- file.mtime(main_path_o)
  mod$compile(force_recompile = TRUE, cpp_options = list(stan_threads = TRUE))
  after_mtime <- file.mtime(main_path_o)
  expect_equal(before_mtime, after_mtime)

  before_mtime <- file.mtime(main_path_o)
  mod$compile(force_recompile = TRUE)
  after_mtime <- file.mtime(main_path_o)
  time_diff <- as.double((after_mtime - before_mtime), units = "secs")
  expect_gt(time_diff, 0)
})

test_that("message is shown on building main.o", {
  skip_on_cran()
  main_o_files <- c(
    file.path(cmdstan_path(), "src", "cmdstan", "main.o"),
    file.path(cmdstan_path(), "src", "cmdstan", "main_noflags.o")
  )
  for (f in main_o_files) {
    if (file.exists(f))
      file.remove(f)
  }
  expect_message(
    mod$compile(force_recompile = TRUE),
    "Compiling the main object file and precompiled headers (may take up to a few minutes). ",
    "This is only necessary for the first compilation after installation or when ",
    "threading, MPI or OpenCL are used for the first time.",
    fixed = TRUE
  )
})

test_that("compile errors are shown", {
  skip_on_cran()
  stan_file <- testing_stan_file("fail")
  expect_error(
    cmdstan_model(stan_file),
    "An error occured during compilation! See the message above for more information."
  )
})

test_that("dir arg works for cmdstan_model and $compile()", {
  skip_on_cran()
  tmp_dir <- tempdir()
  tmp_dir_2 <- tempdir()

  mod_dir <- cmdstan_model(stan_program, dir = tmp_dir)
  expect_equal(repair_path(dirname(mod_dir$exe_file())), repair_path(tmp_dir))
  checkmate::expect_file_exists(mod_dir$exe_file())
  file.remove(mod_dir$exe_file())

  mod_dir_1 <- cmdstan_model(stan_program, dir = tmp_dir, compile = FALSE)
  mod_dir_1$compile()
  expect_equal(repair_path(dirname(mod_dir_1$exe_file())), repair_path(tmp_dir))
  checkmate::expect_file_exists(mod_dir_1$exe_file())
  file.remove(mod_dir_1$exe_file())

  mod_dir_1$compile(dir = tmp_dir_2) #dir in compile overwrites dir in cmdstan_model
  expect_equal(repair_path(dirname(mod_dir_1$exe_file())), repair_path(tmp_dir))
  checkmate::expect_file_exists(mod_dir_1$exe_file())
  file.remove(mod_dir_1$exe_file())

  mod_dir_2 <- cmdstan_model(stan_program, compile = FALSE)
  mod_dir_2$compile(dir = tmp_dir)
  expect_equal(repair_path(dirname(mod_dir_2$exe_file())), repair_path(tmp_dir))
  checkmate::expect_file_exists(mod_dir_2$exe_file())
  file.remove(mod_dir_2$exe_file())

  mod_dir_3 <- cmdstan_model(stan_program)
  mod_dir_3$compile(dir = tmp_dir) #dir in compile overwrites dir in cmdstan_model
  expect_equal(repair_path(dirname(mod_dir_3$exe_file())), repair_path(tmp_dir))
  checkmate::expect_file_exists(mod_dir_3$exe_file())
  file.remove(mod_dir_3$exe_file())

  expect_error(
    cmdstan_model(stan_program, dir = "ABCD"),
    "Assertion on 'dir' failed"
  )
})

test_that("*hpp_file() functions work", {
  skip_on_cran()
  tmp_dir <- tempdir()
  stan_file <- testing_stan_file("bernoulli")
  mod <- cmdstan_model(stan_file, force_recompile = TRUE)
  checkmate::expect_file_exists(mod$hpp_file())
  expect_match(paste0(readLines(mod$hpp_file()), collapse = "\n"), "Code generated by stanc", fixed = TRUE)
  mod$save_hpp_file()
  expect_equal(mod$hpp_file(), file.path(dirname(mod$stan_file()), "bernoulli.hpp"))
  mod$save_hpp_file(tmp_dir)
  expect_equal(mod$hpp_file(), file.path(tmp_dir, "bernoulli.hpp"))
  mod$compile(force_recompile = TRUE)
  expect_false(isTRUE(all.equal(mod$hpp_file(), file.path(tmp_dir, "bernoulli.hpp"))))
  expect_false(isTRUE(all.equal(mod$hpp_file(), file.path(dirname(mod$stan_file()), "bernoulli.hpp"))))
})

