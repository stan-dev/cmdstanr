context("model-compile")

set_cmdstan_path()
stan_program <- cmdstan_example_file()
mod <- cmdstan_model(stan_file = stan_program, compile = FALSE)

test_that("object initialized correctly", {
  expect_equal(mod$stan_file(), stan_program)
  expect_equal(mod$exe_file(), character(0))
  expect_error(
    mod$hpp_file(),
    "The .hpp file does not exists. Please (re)compile the model.",
    fixed = TRUE
  )
})

test_that("error if no compile() before model fitting", {
  expect_error(
    mod$sample(),
    "Model not compiled. Try running the compile() method first.",
    fixed = TRUE
  )
})

test_that("compile() method works", {
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
  mod$compile(quiet = TRUE)
  expect_interactive_message(
    mod$compile(quiet = TRUE, force_recompile = TRUE),
    "Compiling Stan program..."
  )
})

test_that("compile() method forces recompilation if model modified", {
  # remove executable if exists
  exe <- cmdstan_ext(strip_ext(mod$stan_file()))
  if (!file.exists(exe)) {
    mod$compile(quiet = TRUE)
  }
  Sys.setFileTime(mod$stan_file(), Sys.time() + 1) #touch file to trigger recompile
  expect_interactive_message(mod$compile(quiet = TRUE), "Compiling Stan program...")
})

test_that("compile() method works with spaces in path", {
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
  unlink(dir_with_spaces, recursive = TRUE)
})

test_that("compile() method overwrites binaries", {
  mod$compile(quiet = TRUE)
  old_time = file.mtime(mod$exe_file())
  mod$compile(quiet = TRUE, force_recompile = TRUE)
  new_time =
  expect_gt(file.mtime(mod$exe_file()), old_time)
})

test_that("compilation works with include_paths", {
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
                                   include_paths = test_path("resources", "stan"),
                                   force_recompile = TRUE),
    "Compiling Stan program"
  )
  expect_equal(
    mod_w_include$exe_file(),
    cmdstan_ext(strip_ext(absolute_path(stan_program_w_include)))
  )
})

test_that("name in STANCFLAGS is set correctly", {
  out <- utils::capture.output(mod$compile(quiet = FALSE, force_recompile = TRUE))
  if(os_is_windows() && !os_is_wsl()) {
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
  stan_file <- testing_stan_file("fail")
  expect_error(
    cmdstan_model(stan_file),
    "An error occured during compilation! See the message above for more information."
  )
})

test_that("dir arg works for cmdstan_model and $compile()", {
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
  stan_file <- testing_stan_file("bernoulli")
  mod <- cmdstan_model(stan_file, stanc_options = list("warn-pedantic"), force_recompile = TRUE)
  checkmate::expect_r6(
    mod,
    "CmdStanModel"
  )
})

test_that("compile() works with pedantic=TRUE", {
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
    "The parameter x was declared but was not used",
    fixed = TRUE
  )
})

test_that("*hpp_file() functions work", {
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
    "Stan program is syntactically correct",
    fixed = TRUE
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
    "The Stan file used to create the `CmdStanModel` object does not exist.",
    fixed = TRUE
  )
  mod_exe <- cmdstan_model(exe_file = mod_removed_stan_file$exe_file())
  expect_error(
    mod_exe$check_syntax(),
    "'$check_syntax()' cannot be used because the 'CmdStanModel' was not created with a Stan file.",
    fixed = TRUE
  )

})

test_that("check_syntax() works with pedantic=TRUE", {
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
    "The parameter x was declared but was not used",
    fixed = TRUE
  )

  # should also still work if specified via stanc_options
  expect_message(
    mod_pedantic_warn$check_syntax(stanc_options = list("warn-pedantic" = TRUE)),
    "The parameter x was declared but was not used",
    fixed = TRUE
  )

  expect_output(
    expect_message(
      mod_pedantic_warn$check_syntax(pedantic = TRUE),
      "The parameter x was declared but was not used",
      fixed = TRUE
    ),
    regexp = NA
  )
})

test_that("check_syntax() works with include_paths", {
  stan_program_w_include <- testing_stan_file("bernoulli_include")

  mod_w_include <- cmdstan_model(stan_file = stan_program_w_include, compile=FALSE,
                                   include_paths = test_path("resources", "stan"))
  expect_true(mod_w_include$check_syntax())

})

test_that("check_syntax() works with pedantic=TRUE", {
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
  skip_if(os_is_windows() && !os_is_wsl())
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
  unlink(exe, recursive = TRUE)
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
  path_1_compare <- ifelse(os_is_wsl(), wsl_safe_path(path_1), path_1)
  expect_equal(
    include_paths_stanc3_args(path_1),
    paste0("--include-paths=", path_1_compare))
  path_2 <- file.path(tempdir(), "folder2")
  if (!dir.exists(path_2)) {
    dir.create(path_2)
  }
  path_2 <- repair_path(path_2)
  path_2_compare <- ifelse(os_is_wsl(), wsl_safe_path(path_2), path_2)
  expect_equal(
    include_paths_stanc3_args(c(path_1, path_2)),
    c(
      paste0("--include-paths=", path_1_compare, ",", path_2_compare)
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

test_that("cmdstan_model works with exe_file", {
  stan_file <- testing_stan_file("bernoulli")
  mod <- cmdstan_model(stan_file)
  expect_true(file.exists(mod$exe_file()))
  default_exe_file <- mod$exe_file()
  file.remove(mod$exe_file())

  tmp_exe_file <- tempfile(fileext = cmdstan_ext())
  mod <- cmdstan_model(
    stan_file = stan_file,
    exe_file = tmp_exe_file
  )
  expect_match(
    mod$exe_file(),
    repair_path(tmp_exe_file)
  )
  expect_true(file.exists(mod$exe_file()))
  expect_false(file.exists(default_exe_file))

  mod <- cmdstan_model(
    exe_file = tmp_exe_file
  )
  expect_match(
    mod$exe_file(),
    repair_path(tmp_exe_file)
  )
  expect_true(file.exists(mod$exe_file()))
  expect_false(file.exists(default_exe_file))
})

test_that("cmdstan_model errors if exe_file specified and version less than 2.27", {
  fake_cmdstan_version("2.26.0")
  expect_error(
    cmdstan_model(exe_file = "foo"),
    "'exe_file' argument is only supported with CmdStan 2.27 and newer"
  )
  reset_cmdstan_version()
})

test_that("cmdstan_model created only with exe_file errors for check_syntax, code, ... ", {
  mod <- testing_model("bernoulli")
  mod_exe <- cmdstan_model(exe_file = mod$exe_file())
  expect_error(
    mod_exe$check_syntax(),
    "'$check_syntax()' cannot be used because the 'CmdStanModel' was not created with a Stan file.",
    fixed = TRUE
  )
  expect_error(
    mod_exe$variables(),
    "'$variables()' cannot be used because the 'CmdStanModel' was not created with a Stan file.",
    fixed = TRUE
  )
  expect_error(
    mod_exe$compile(),
    "'$compile()' cannot be used because the 'CmdStanModel' was not created with a Stan file.",
    fixed = TRUE
  )
})

test_that("cmdstan_model errors with no args ", {
  expect_error(
    cmdstan_model(),
    "Unable to create a `CmdStanModel` object. Both 'stan_file' and 'exe_file' are undefined.",
    fixed = TRUE
  )
})

test_that("cmdstan_model works with user_header", {
  skip_if(os_is_macos() | (os_is_windows() && !os_is_wsl()))
  tmpfile <- tempfile(fileext = ".hpp")
  hpp <-
  "
  #include <stan/math.hpp>
  #include <boost/math/tools/promotion.hpp>
  #include <ostream>

  namespace bernoulli_external_model_namespace
  {
      template <typename T0__,
            stan::require_all_t<stan::is_stan_scalar<T0__>>* = nullptr>
      inline typename boost::math::tools::promote_args<T0__>::type make_odds(const T0__ &
                                                                                 theta,
                                                                             std::ostream *pstream__)
      {
          return theta / (1 - theta);
      }
  }"
  cat(hpp, file = tmpfile, sep = "\n")
  mod <- cmdstan_model(
    stan_file = testing_stan_file("bernoulli_external"),
    user_header = tmpfile
  )
  expect_true(file.exists(mod$exe_file()))
  file.remove(mod$exe_file())
  mod_2 <- cmdstan_model(
    stan_file = testing_stan_file("bernoulli_external"),
    cpp_options=list(USER_HEADER=tmpfile),
    stanc_options = list("allow-undefined")
  )
  expect_true(file.exists(mod_2$exe_file()))
})

test_that("cmdstan_model cpp_options dont captialize cxxflags ", {
  file <- file.path(cmdstan_path(), "examples", "bernoulli", "bernoulli.stan")
  cpp_options <- list(
    "CXXFLAGS_OPTIM += -Dsomething_not_used"
  )
  options("cmdstanr_verbose" = TRUE)
  out <- utils::capture.output(
    mod <- cmdstan_model(file, cpp_options = cpp_options, force_recompile = TRUE)
  )
  expect_output(print(out), "-Dsomething_not_used")
})

test_that("format() works", {
  code <- "
  parameters {
    real y;
  }
  model {
  target +=         normal_log(y, 0, 1);
  }
  "
  stan_file_tmp <- write_stan_file(code)
  mod_1 <- cmdstan_model(stan_file_tmp, compile = FALSE)

  expect_output(
    expect_message(
      mod_1$format(),
      "is deprecated",
      fixed = TRUE
    ),
    "target += normal_log(y, 0, 1);",
    fixed = TRUE
  )

  expect_output(
    mod_1$format(canonicalize = TRUE),
    "target += normal_lpdf(y | 0, 1);",
    fixed = TRUE
  )
  expect_output(
    mod_1$format(canonicalize = list("deprecations")),
    "target += normal_lpdf(y | 0, 1);",
    fixed = TRUE
  )
  expect_output(
    expect_message(
      mod_1$format(canonicalize = list("includes")),
      "is deprecated",
      fixed = TRUE
    ),
    "target += normal_log(y, 0, 1);",
    fixed = TRUE
  )

  stan_file <- testing_stan_file("bernoulli_external")
  mod_2 <- cmdstan_model(stan_file, compile = FALSE, stanc_options = list("allow-undefined"))
  expect_output(
    mod_2$format(),
    "make_odds(theta);",
    fixed = TRUE
  )
  mod_3 <- cmdstan_model(
    stan_file,
    compile = FALSE,
    stanc_options = list("allow-undefined", "warn-pedantic")
  )
  expect_output(
    expect_message(
      mod_2$format(),
      regexp = NA
    ),
    "make_odds(theta);",
    fixed = TRUE
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
    mod_removed_stan_file$format(),
    "The Stan file used to create the `CmdStanModel` object does not exist.",
    fixed = TRUE
  )
  mod_exe <- cmdstan_model(exe_file = mod_removed_stan_file$exe_file())
  expect_error(
    mod_exe$format(),
    "'$format()' cannot be used because the 'CmdStanModel' was not created with a Stan file.",
    fixed = TRUE
  )
})

test_that("format() works with include_paths", {
  stan_program_w_include <- testing_stan_file("bernoulli_include")

  mod_w_include <- cmdstan_model(stan_file = stan_program_w_include, compile=FALSE,
                                   include_paths = test_path("resources", "stan"))
  expect_output(
    mod_w_include$format(),
    "#include ",
    fixed = TRUE
  )
  expect_output(
    mod_w_include$format(canonicalize = list('deprecations', 'parentheses', 'braces')),
    "#include ",
    fixed = TRUE
  )
    expect_output(
    mod_w_include$format(canonicalize = list('includes')),
    "real divide_real_by_two",
    fixed = TRUE
  )
})

test_that("overwrite_file works with format()", {
  code <- "
  parameters {
    real y;
  }
  model {
  target +=         normal_lpdf(y| 0, 5);
  }
  "
  stan_file_tmp <- write_stan_file(code)
  mod_1 <- cmdstan_model(stan_file_tmp, compile = FALSE)
  expect_false(
    any(
      grepl(paste0(basename(mod_1$stan_file()), ".bak"),
            list.files(dirname(mod_1$stan_file()))
      )
    )
  )
  mod_1$format(overwrite_file = TRUE, backup = FALSE)
  expect_false(
    any(
      grepl(paste0(basename(mod_1$stan_file()), ".bak"),
            list.files(dirname(mod_1$stan_file()))
      )
    )
  )
  mod_1$format(overwrite_file = TRUE, backup = TRUE)
  expect_true(
    any(
      grepl(paste0(basename(mod_1$stan_file()), ".bak"),
            list.files(dirname(mod_1$stan_file()))
      )
    )
  )
})

test_that("dirname of stan_file is used as include path if no other paths supplied", {
  data_code <- "
  data {
    int N;
  }
  "

  model_code <- "
  #include separate_file.stan
  parameters {
    vector[N] y;
  }
  model {
    y ~ std_normal();
  }
  "
  tmpdir <- tempdir()
  stan_data_file <- write_stan_file(data_code, basename = "separate_file.stan", dir = tmpdir)
  stan_file <- write_stan_file(model_code, dir = tmpdir)

  mod_tmp <- cmdstan_model(stan_file, compile = FALSE)
  expect_true(mod_tmp$check_syntax())
  expect_true(mod_tmp$format())
  expect_s3_class(mod_tmp$compile(), "CmdStanModel")
})
