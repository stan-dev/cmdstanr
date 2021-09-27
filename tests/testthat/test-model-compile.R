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
  expect_error(
    mod$hpp_file(),
    "The .hpp file does not exists. Please (re)compile the model.",
    fixed = TRUE
  )
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
  expect_interactive_message(mod$compile(quiet = TRUE), "Compiling Stan program...")
  expect_interactive_message(mod$compile(quiet = TRUE), "Model executable is up to date!")
  checkmate::expect_file_exists(mod$hpp_file())
  checkmate::expect_file_exists(exe)
  file.remove(exe)
  out <- utils::capture.output(mod$compile(quiet = FALSE))
  expect_output(print(out), "Translating Stan model")
})

test_that("compile() method forces recompilation force_recompile = TRUE", {
  skip_on_cran()
  mod$compile(quiet = TRUE)
  expect_interactive_message(
    mod$compile(quiet = TRUE, force_recompile = TRUE),
    "Compiling Stan program..."
  )
})

test_that("compile() method forces recompilation if model modified", {
  skip_on_cran()
  # remove executable if exists
  exe <- cmdstan_ext(strip_ext(mod$stan_file()))
  if (!file.exists(exe)) {
    mod$compile(quiet = TRUE)
  }
  Sys.setFileTime(mod$stan_file(), Sys.time() + 1) #touch file to trigger recompile
  expect_interactive_message(mod$compile(quiet = TRUE), "Compiling Stan program...")
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
  expect_interactive_message(mod_spaces$compile(), "Compiling Stan program...")
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

  expect_interactive_message(
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
  main_path_threads_o <- file.path(cmdstan_path(), "src", "cmdstan", "main_threads.o")
  if (file.exists(main_path_threads_o)) {
    file.remove(main_path_threads_o)
  }
  mod$compile(force_recompile = TRUE)

  before_mtime <- file.mtime(main_path_o)
  mod$compile(force_recompile = TRUE)
  after_mtime <- file.mtime(main_path_o)
  expect_equal(before_mtime, after_mtime)
  expect_false(file.exists(main_path_threads_o))

  mod$compile(force_recompile = TRUE, cpp_options = list(stan_threads = TRUE))
  checkmate::expect_file_exists(main_path_threads_o)

  before_mtime <- file.mtime(main_path_o)
  mod$compile(force_recompile = TRUE, cpp_options = list(stan_threads = TRUE))
  after_mtime <- file.mtime(main_path_o)
  expect_equal(before_mtime, after_mtime)

  before_mtime <- file.mtime(main_path_o)
  mod$compile(force_recompile = TRUE)
  after_mtime <- file.mtime(main_path_o)
  expect_equal(before_mtime, after_mtime)
})

test_that("multiple cpp_options work", {
  skip_on_cran()
  stan_file <- testing_stan_file("bernoulli")
  expect_interactive_message(
    mod <- cmdstan_model(stan_file, cpp_options = list("DUMMY_TEST2"="1", "DUMMY_TEST2"="1",  "DUMMY_TEST3"="1"), force_recompile = TRUE),
    "Compiling Stan program..."
  )
  expect_interactive_message(
    mod$compile(cpp_options = list("DUMMY_TEST2"="1", "DUMMY_TEST2"="1",  "DUMMY_TEST3"="1"), force_recompile = TRUE),
    "Compiling Stan program..."
  )
  expect_interactive_message(
    mod$compile(cpp_options = list(), force_recompile = TRUE),
    "Compiling Stan program..."
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

test_that("compiling stops on hyphens in stanc_options", {
  skip_on_cran()
  hyphens <- list("--allow-undefined")
  hyphens2 <- list("--allow-undefined" = TRUE)
  hyphens3 <- list("--o" = "something")
  stan_file <- testing_stan_file("bernoulli")
  expect_error(
    cmdstan_model(stan_file, stanc_options = hyphens, compile = FALSE),
    "No leading hyphens allowed in stanc options (--allow-undefined). Use options without leading hyphens, for example `stanc_options = list('allow-undefined')`",
    fixed = TRUE
  )
  expect_error(
    cmdstan_model(stan_file, stanc_options = hyphens2, compile = FALSE),
    "No leading hyphens allowed in stanc options (--allow-undefined). Use options without leading hyphens, for example `stanc_options = list('allow-undefined')`",
    fixed = TRUE
  )
  expect_error(
    cmdstan_model(stan_file, stanc_options = hyphens3, compile = FALSE),
    "No leading hyphens allowed in stanc options (--o). Use options without leading hyphens, for example `stanc_options = list('allow-undefined')`",
    fixed = TRUE
  )
  mod <- cmdstan_model(stan_file, compile = FALSE)
  expect_error(
    mod$compile(stanc_options = hyphens),
    "No leading hyphens allowed in stanc options (--allow-undefined). Use options without leading hyphens, for example `stanc_options = list('allow-undefined')`",
    fixed = TRUE
  )
  expect_error(
    mod$compile(stanc_options = hyphens2),
    "No leading hyphens allowed in stanc options (--allow-undefined). Use options without leading hyphens, for example `stanc_options = list('allow-undefined')`",
    fixed = TRUE
  )
  expect_error(
    mod$compile(stanc_options = hyphens3),
    "No leading hyphens allowed in stanc options (--o). Use options without leading hyphens, for example `stanc_options = list('allow-undefined')`",
    fixed = TRUE
  )
})

test_that("compiling works with only names in list", {
  skip_on_cran()
  stan_file <- testing_stan_file("bernoulli")
  mod <- cmdstan_model(stan_file, stanc_options = list("warn-pedantic"), force_recompile = TRUE)
  checkmate::expect_r6(
    mod,
    "CmdStanModel"
  )
})

test_that("compile() works with pedantic=TRUE", {
  skip_on_cran()
  stan_file <- write_stan_file("
  parameters {
    real y;
    real x;
  }
  model {
    y ~ std_normal();
  }
  ")
  expect_message(
    mod_pedantic_warn <- cmdstan_model(stan_file, pedantic = TRUE),
    "The parameter x was declared but was not used in the density calculation."
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


test_that("check_syntax() works", {
  skip_on_cran()
  stan_file <- testing_stan_file("fail")
  mod_fail <- cmdstan_model(stan_file, compile = FALSE)
  expect_error(
    expect_message(
      mod_fail$check_syntax(),
      "Ill-typed arguments supplied to assignment operator"
    ),
    "Syntax error found! See the message above for more information."
  )

  stan_file <- testing_stan_file("bernoulli")
  mod_ok <- cmdstan_model(stan_file, compile = FALSE)
  expect_true(
    expect_message(
      mod_ok$check_syntax(),
      "Stan program is syntactically correct"
    )
  )
  expect_message(
    mod_ok$check_syntax(quiet = TRUE),
    regexp = NA
  )
  expect_message(
    mod_ok$check_syntax(stanc_options = list("allow-undefined", "warn-pedantic")),
    "Stan program is syntactically correct"
  )
  expect_message(
    mod_ok$check_syntax(stanc_options = list("allow-undefined", "warn-pedantic"), quiet = TRUE),
    regexp = NA
  )

  code <- "
  parameters {
    real y;
  }
  model {
    y ~ std_normal();
  }
  "
  stan_file_tmp <- write_stan_file(code)
  mod_removed_stan_file <- cmdstan_model(stan_file_tmp)
  file.remove(stan_file_tmp)
  expect_error(
    mod_removed_stan_file$check_syntax(),
    "The Stan file used to create the `CmdStanModel` object does not exist."
  )
})

test_that("check_syntax() works with pedantic=TRUE", {
  skip_on_cran()
  model_code <- "
  parameters {
    real y;
    real x;
  }
  model {
    y ~ std_normal();
  }
  "
  stan_file <- write_stan_file(model_code)
  mod_pedantic_warn <- cmdstan_model(stan_file, compile = FALSE)
  expect_message(
    mod_pedantic_warn$check_syntax(),
    "Stan program is syntactically correct"
  )

  expect_message(
    mod_pedantic_warn$check_syntax(pedantic = TRUE),
    "The parameter x was declared but was not used in the density calculation."
  )

  # should also still work if specified via stanc_options
  expect_message(
    mod_pedantic_warn$check_syntax(stanc_options = list("warn-pedantic" = TRUE)),
    "The parameter x was declared but was not used in the density calculation."
  )

  expect_output(
    expect_message(
      mod_pedantic_warn$check_syntax(pedantic = TRUE),
      "The parameter x was declared but was not used in the density calculation."
    ),
    regexp = NA
  )
})

test_that("check_syntax() works with include_paths", {
  skip_on_cran()

  stan_program_w_include <- testing_stan_file("bernoulli_include")

  mod_w_include <- cmdstan_model(stan_file = stan_program_w_include, compile=FALSE,
                                   include_paths = test_path("resources", "stan"))
  expect_true(mod_w_include$check_syntax())

})

test_that("check_syntax() works with pedantic=TRUE", {
  skip_on_cran()
  model_code <- "
  transformed data {
    real a;
    a <- 3;
  }
  "
  stan_file <- write_stan_file(model_code)
  mod_dep_warning <- cmdstan_model(stan_file, compile = FALSE)
  expect_message(
    mod_dep_warning$compile(),
    "deprecated in the Stan language",
    fixed = TRUE
  )
  expect_message(
    mod_dep_warning$check_syntax(),
    "deprecated in the Stan language",
    fixed = TRUE
  )
})

test_that("compiliation errors if folder with the model name exists", {
  skip_on_cran()
  skip_if(os_is_windows())
  model_code <- "
  parameters {
    real y;
  }
  model {
    y ~ std_normal();
  }
  "
  stan_file <- write_stan_file(model_code)
  exe <- strip_ext(stan_file)
  if (!dir.exists(exe)) {
    if (file.exists(exe)) {
      file.remove(exe)
    }
    dir.create(exe)
  }
  expect_error(
    cmdstan_model(stan_file),
    "There is a subfolder matching the model name in the same folder as the model! Please remove or rename the subfolder and try again."
  )
  file.remove(exe)
})

test_that("cpp_options_to_compile_flags() works", {
  options = list(
    stan_threads = TRUE
  )
  expect_equal(cpp_options_to_compile_flags(options), "STAN_THREADS=TRUE")
  options = list(
    stan_threads = TRUE,
    stanc2 = TRUE
  )
  expect_equal(cpp_options_to_compile_flags(options), c("STAN_THREADS=TRUE", "STANC2=TRUE"))
  options = list()
  expect_equal(cpp_options_to_compile_flags(options), NULL)
})

test_that("include_paths_stanc3_args() works", {
  expect_equal(include_paths_stanc3_args(), NULL)
  path_1 <- file.path(tempdir(), "folder1")
  if (!dir.exists(path_1)) {
    dir.create(path_1)
  }
  path_1 <- repair_path(path_1)
  expect_equal(include_paths_stanc3_args(path_1), paste0("--include-paths=", path_1))
  path_2 <- file.path(tempdir(), "folder2")
  if (!dir.exists(path_2)) {
    dir.create(path_2)
  }
  path_2 <- repair_path(path_2)
  expect_equal(
    include_paths_stanc3_args(c(path_1, path_2)),
    c(
      paste0("--include-paths=", path_1, ",", path_2)
    )
  )
})

test_that("cpp_options work with settings in make/local", {
  backup <- cmdstan_make_local()

  if (length(mod$exe_file()) > 0 && file.exists(mod$exe_file())) {
    file.remove(mod$exe_file())
  }

  cmdstan_make_local(cpp_options = list(), append = FALSE)

  mod <- cmdstan_model(stan_file = stan_program)
  expect_null(mod$cpp_options()$STAN_THREADS)

  file.remove(mod$exe_file())

  cmdstan_make_local(cpp_options = list(stan_threads = TRUE))

  file <- file.path(cmdstan_path(), "examples", "bernoulli", "bernoulli.stan")
  mod <- cmdstan_model(file)
  expect_true(mod$cpp_options()$STAN_THREADS)

  file.remove(mod$exe_file())

  # restore
  cmdstan_make_local(cpp_options = backup, append = FALSE)
})
