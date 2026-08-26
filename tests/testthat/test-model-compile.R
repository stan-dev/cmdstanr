set_cmdstan_path()
stan_program <- cmdstan_example_file()
mod <- cmdstan_model(stan_file = stan_program, compile = FALSE)
local_cmdstan_make_local(cpp_options = list("PRECOMPILED_HEADERS"="false"))

test_that("object initialized correctly", {
  expect_equal(mod$stan_file(), stan_program)
  expect_equal(mod$exe_file(), character(0))
  expect_error(
    mod$hpp_file(),
    "The .hpp file does not exist. Please (re)compile the model.",
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
  expect_compilation(mod, quiet = TRUE)
  expect_no_recompilation(mod, quiet = TRUE)
  checkmate::expect_file_exists(mod$hpp_file())
  checkmate::expect_file_exists(exe)
  file.remove(exe)
  out <- utils::capture.output(mod$compile(quiet = FALSE))
  expect_output(print(out), "Translating Stan model")
})

test_that("compile() method forces recompilation force_recompile = TRUE", {
  mod$compile(quiet = TRUE)
  expect_compilation(mod, quiet = TRUE, force_recompile = TRUE)
})

test_that("compile() method forces recompilation if model modified", {
  # remove executable if exists
  exe <- cmdstan_ext(strip_ext(mod$stan_file()))
  if (!file.exists(exe)) {
    mod$compile(quiet = TRUE)
  }
  Sys.setFileTime(mod$stan_file(), Sys.time() + 1) #touch file to trigger recompile
  expect_compilation(mod, quiet = TRUE)
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
  expect_compilation(mod_spaces)
  file.remove(stan_model_with_spaces)
  file.remove(exe)
  unlink(dir_with_spaces, recursive = TRUE)
})

test_that("compile() method overwrites binaries", {
  mod$compile(quiet = TRUE)
  old_time = file.mtime(mod$exe_file())
  mod$compile(quiet = TRUE, force_recompile = TRUE)
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
    paste0(
      "Directory '",
      repair_path(absolute_path("NOT_A_DIR")),
      "' does not exist"
    ),
    fixed = TRUE
  )

  expect_error(
    expect_output(
      cmdstan_model(stan_file = stan_program_w_include, quiet = TRUE),
      "could not find include file"
    )
  )

  expect_call_compilation(
    mod_w_include <- cmdstan_model(stan_file = stan_program_w_include, quiet = TRUE,
                                   include_paths = test_path("resources", "stan"),
                                   force_recompile = TRUE)
  )
  expect_equal(
    mod_w_include$exe_file(),
    cmdstan_ext(strip_ext(absolute_path(stan_program_w_include)))
  )
})

test_that("precompiled models retain include paths", {
  model_dir <- withr::local_tempdir()
  write_stan_file(
    "
    functions {
      real silly_logit(real x) {
        return logit(x);
      }
    }
    ",
    dir = file.path(model_dir, "utils"),
    basename = "silly.stan"
  )
  stan_file <- write_stan_file(
    "
    #include utils/silly.stan
    data {
      int<lower=0> N;
      array[N] int<lower=0, upper=1> y;
    }
    parameters {
      real<lower=0, upper=1> theta;
    }
    model {
      theta ~ beta(1, 1);
      y ~ bernoulli(theta);
    }
    generated quantities {
      real theta_lin = silly_logit(theta);
    }
    ",
    dir = model_dir,
    basename = "bernoulli.stan"
  )
  compiled_model <- cmdstan_model(
    stan_file,
    include_paths = model_dir,
    quiet = TRUE
  )

  model_with_explicit_path <- cmdstan_model(
    stan_file,
    exe_file = compiled_model$exe_file(),
    compile = FALSE,
    include_paths = model_dir
  )
  expect_equal(model_with_explicit_path$include_paths(), repair_path(model_dir))
  expect_no_error(model_with_explicit_path$variables())

  model_with_automatic_path <- cmdstan_model(
    stan_file,
    exe_file = compiled_model$exe_file(),
    compile = FALSE
  )
  expect_equal(model_with_automatic_path$include_paths(), repair_path(dirname(stan_file)))
  expect_no_error(model_with_automatic_path$variables())
})

test_that("include paths are resolved when the model is created", {
  model_dir <- withr::local_tempdir()
  file.copy(
    c(testing_stan_file("bernoulli_include"), testing_stan_file("divide_real_by_two")),
    model_dir
  )
  mod <- withr::with_dir(
    model_dir,
    cmdstan_model("bernoulli_include.stan", compile = FALSE)
  )
  # the working directory no longer contains the included file
  expect_true(mod$check_syntax(quiet = TRUE))
})

test_that("relative include_paths are resolved when the model is created", {
  model_dir <- withr::local_tempdir()
  include_dir <- file.path(model_dir, "includes")
  dir.create(include_dir)
  file.copy(testing_stan_file("bernoulli_include"), model_dir)
  file.copy(testing_stan_file("divide_real_by_two"), include_dir)

  mod <- withr::with_dir(
    model_dir,
    cmdstan_model(
      "bernoulli_include.stan",
      include_paths = "includes",
      compile = FALSE
    )
  )
  # "includes" no longer resolves relative to the working directory
  expect_true(mod$check_syntax(quiet = TRUE))
})

test_that("relative include_paths given to $compile() are resolved when it is called", {
  model_dir <- withr::local_tempdir()
  include_dir <- file.path(model_dir, "includes")
  dir.create(include_dir)
  file.copy(testing_stan_file("bernoulli_include"), model_dir)
  file.copy(testing_stan_file("divide_real_by_two"), include_dir)

  mod <- withr::with_dir(model_dir, {
    mod <- cmdstan_model("bernoulli_include.stan", compile = FALSE)
    mod$compile(include_paths = "includes", quiet = TRUE)
    mod
  })
  expect_true(mod$check_syntax(quiet = TRUE))
})

test_that("$compile() reuses include paths from the previous compilation", {
  model_dir <- withr::local_tempdir()
  include_dir <- file.path(model_dir, "includes")
  dir.create(include_dir)
  file.copy(testing_stan_file("bernoulli_include"), model_dir)
  file.copy(testing_stan_file("divide_real_by_two"), include_dir)

  received_stancflags <- list()
  local_mocked_bindings(
    get_cmdstan_flags = function(flag_name) character(),
    get_standalone_hpp = function(stan_file, stancflags) {
      received_stancflags <<- append(received_stancflags, list(stancflags))
      ""
    }
  )

  mod <- cmdstan_model(
    file.path(model_dir, "bernoulli_include.stan"),
    include_paths = include_dir,
    compile = FALSE
  )
  # Use a successful compile to move the paths out of precompile state.
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = mod$compile(force_recompile = TRUE, quiet = TRUE)
  )
  expect_null(mod$.__enclos_env__$private$precompile_include_paths_)

  received_stancflags <- list()
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = expect_no_error(mod$compile(force_recompile = TRUE, quiet = TRUE))
  )
  expect_equal(mod$include_paths(), resolve_path(include_dir))
  # Compare stanc arguments because WSL converts stored Windows paths.
  include_args <- include_paths_stanc3_args(mod$include_paths(), direct_call = TRUE)
  expect_true(all(vapply(
    received_stancflags,
    function(x) all(include_args %in% x),
    logical(1)
  )))
})

test_that("$compile() doesn't reuse cpp and stanc options from the previous compilation", {
  # Use a temporary copy because mocked compiles install executables.
  model_dir <- withr::local_tempdir()
  stan_file <- file.path(model_dir, "bernoulli.stan")
  file.copy(testing_stan_file("bernoulli"), stan_file)
  model <- cmdstan_model(stan_file, compile = FALSE)
  received_stancflags <- list()
  local_mocked_bindings(
    get_cmdstan_flags = function(flag_name) character(),
    get_standalone_hpp = function(stan_file, stancflags) {
      received_stancflags <<- append(received_stancflags, list(stancflags))
      ""
    }
  )

  # Successful compiles clear one-shot cpp and stanc options.
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = model$compile(
      cpp_options = list(stan_threads = TRUE),
      stanc_options = list("warn-pedantic" = TRUE),
      force_recompile = TRUE
    )
  )
  expect_true(model$cpp_options()[["stan_threads"]])
  expect_equal(
    vapply(received_stancflags, function(x) "--warn-pedantic" %in% x, logical(1)),
    rep(TRUE, 2)
  )

  received_stancflags <- list()
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = model$compile(force_recompile = TRUE)
  )

  expect_null(model$cpp_options()[["stan_threads"]])
  expect_equal(
    vapply(received_stancflags, function(x) "--warn-pedantic" %in% x, logical(1)),
    rep(FALSE, 2)
  )
})

test_that("$compile() doesn't reuse cpp and stanc options supplied to cmdstan_model()", {
  model_dir <- withr::local_tempdir()
  stan_file <- file.path(model_dir, "bernoulli.stan")
  file.copy(testing_stan_file("bernoulli"), stan_file)
  # Options given to the constructor are held until the first compilation
  # consumes them, unlike the include paths and user header, which persist.
  model <- cmdstan_model(
    stan_file,
    compile = FALSE,
    cpp_options = list(stan_threads = TRUE),
    stanc_options = list("warn-pedantic" = TRUE)
  )
  received_stancflags <- list()
  local_mocked_bindings(
    get_cmdstan_flags = function(flag_name) character(),
    get_standalone_hpp = function(stan_file, stancflags) {
      received_stancflags <<- append(received_stancflags, list(stancflags))
      ""
    }
  )

  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = model$compile(force_recompile = TRUE)
  )
  expect_true(model$cpp_options()[["stan_threads"]])
  expect_equal(
    vapply(received_stancflags, function(x) "--warn-pedantic" %in% x, logical(1)),
    rep(TRUE, 2)
  )

  received_stancflags <- list()
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = model$compile(force_recompile = TRUE)
  )

  expect_null(model$cpp_options()[["stan_threads"]])
  expect_equal(
    vapply(received_stancflags, function(x) "--warn-pedantic" %in% x, logical(1)),
    rep(FALSE, 2)
  )
})

test_that("name in STANCFLAGS is set correctly", {
  local_reproducible_output()
  out <- utils::capture.output(mod$compile(quiet = FALSE, force_recompile = TRUE))
  if(os_is_windows() && !os_is_wsl()) {
    out_no_name <- "bin/stanc.exe --name=bernoulli_model --o"
    out_name <- "bin/stanc.exe --name=bernoulli2_model --o"
  } else {
    out_no_name <- "bin/stanc --name=bernoulli_model --o"
    out_name <- "bin/stanc --name=bernoulli2_model --o"
  }
  expect_output(print(out), out_no_name)

  out <- utils::capture.output(
    mod$compile(
      quiet = FALSE,
      force_recompile = TRUE,
      stanc_options = list(name = "bernoulli2_model")
    )
  )
  expect_output(print(out), out_name)
})


test_that("switching threads on and off works without rebuild", {
  main_path_o <- file.path(cmdstan_path(), "src", "cmdstan", "main.o")
  main_path_threads_o <- file.path(cmdstan_path(), "src", "cmdstan", "main_threads.o")
  backup <- cmdstan_make_local()
  no_threads <- grep("STAN_THREADS", backup, invert = TRUE, value = TRUE)
  cmdstan_make_local(cpp_options = list(no_threads), append = FALSE)
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

  cmdstan_make_local(cpp_options = backup, append = FALSE)
})

test_that("multiple cpp_options work", {
  stan_file <- testing_stan_file("bernoulli")
  expect_call_compilation(
    mod <- cmdstan_model(stan_file, cpp_options = list("DUMMY_TEST2"="1", "DUMMY_TEST2"="1",  "DUMMY_TEST3"="1"), force_recompile = TRUE)
  )
  expect_compilation(mod, cpp_options = list("DUMMY_TEST2"="1", "DUMMY_TEST2"="1",  "DUMMY_TEST3"="1"), force_recompile = TRUE)
  expect_compilation(mod, cpp_options = list(), force_recompile = TRUE)
})

test_that("compile errors are shown", {
  stan_file <- testing_stan_file("fail")
  expect_error(
    cmdstan_model(stan_file),
    "An error occurred during compilation! See the message above for more information. (stanc exited with status 1)",
    fixed = TRUE
  )
})

test_that("compile() performs stanc checks during dry runs", {
  stan_file <- testing_stan_file("fail")
  model <- cmdstan_model(stan_file, compile = FALSE)
  expect_error(
    model$compile(force_recompile = TRUE, dry_run = TRUE),
    "An error occurred during compilation! See the message above for more information. (stanc exited with status 1)",
    fixed = TRUE
  )
})

test_that("compile() with dry_run = TRUE doesn't refresh cached model state", {
  model_dir <- withr::local_tempdir()
  stan_file <- write_stan_file(
    "parameters { real alpha; } model { alpha ~ std_normal(); }",
    dir = model_dir,
    basename = "issue1228-dry-run.stan"
  )
  model <- cmdstan_model(stan_file, compile = FALSE)
  code_before <- model$code()
  variables_before <- model$variables()
  local_mocked_bindings(
    get_cmdstan_flags = function(flag_name) character(),
    get_standalone_hpp = function(stan_file, stancflags) ""
  )

  write_stan_file(
    "parameters { real beta; } model { beta ~ std_normal(); }",
    dir = model_dir,
    basename = "issue1228-dry-run.stan"
  )
  model$compile(force_recompile = TRUE, dry_run = TRUE)

  expect_identical(model$code(), code_before)
  expect_identical(model$variables(), variables_before)
  expect_equal(ls(model$functions), c("compiled", "existing_exe"))
  expect_false(model$functions$compiled)
})

test_that("a failed compile() doesn't refresh cached model state", {
  model_dir <- withr::local_tempdir()
  stan_file <- write_stan_file(
    "parameters { real alpha; } model { alpha ~ std_normal(); }",
    dir = model_dir,
    basename = "issue1228-failed-compile.stan"
  )
  model <- cmdstan_model(stan_file, compile = FALSE)
  code_before <- model$code()
  variables_before <- model$variables()

  file.copy(testing_stan_file("fail"), stan_file, overwrite = TRUE)
  expect_error(
    model$compile(force_recompile = TRUE),
    "An error occurred during compilation!",
    fixed = TRUE
  )

  expect_identical(model$code(), code_before)
  expect_identical(model$variables(), variables_before)
  expect_equal(ls(model$functions), c("compiled", "existing_exe"))
  expect_false(model$functions$compiled)
})

# Run stanc normally but mock the C++ compiler on a temporary model copy.
local_mocked_bernoulli_model <- function(.local_envir = parent.frame()) {
  stan_file <- file.path(
    withr::local_tempdir(.local_envir = .local_envir),
    "bernoulli.stan"
  )
  file.copy(cmdstan_example_file(), stan_file)
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = cmdstan_model(stan_file)
  )
}

test_that("a failed C++ compile doesn't refresh generated-code state", {
  model <- local_mocked_bernoulli_model()
  private <- model$.__enclos_env__$private

  code_before <- model$code()
  variables_before <- model$variables()
  functions_before <- as.list(model$functions)
  hpp_file_before <- model$hpp_file()
  hpp_code_before <- private$model_methods_env_$hpp_code_
  exe_before <- model$exe_file()
  other_dir <- withr::local_tempdir()
  expect_true(any(nzchar(hpp_code_before)))

  # model_methods_env_ must describe the same program as the executable.
  writeLines(
    "parameters { real beta; } model { beta ~ std_normal(); }",
    model$stan_file()
  )
  with_mocked_cli(
    compile_ret = list(status = 1),
    info_ret = list(status = 1),
    code = expect_error(
      model$compile(dir = other_dir, force_recompile = TRUE),
      "An error occurred during compilation!",
      fixed = TRUE
    )
  )

  expect_identical(model$code(), code_before)
  expect_identical(model$variables(), variables_before)
  expect_identical(as.list(model$functions), functions_before)
  expect_identical(model$hpp_file(), hpp_file_before)
  expect_identical(private$model_methods_env_$hpp_code_, hpp_code_before)
  expect_identical(model$exe_file(), exe_before)
  expect_true(file.exists(exe_before))
})

# Build a distinct replacement whose old backup cannot be removed.
local_leftover_backup_model <- function(.local_envir = parent.frame()) {
  model <- local_mocked_bernoulli_model(.local_envir = .local_envir)
  writeLines("old executable", model$exe_file())
  writeLines(
    "parameters { real beta; } model { beta ~ std_normal(); }",
    model$stan_file()
  )
  local_mocked_bindings(
    unlink = function(...) 1L,
    .package = "base",
    .env = .local_envir
  )
  model
}

expect_describes_new_program <- function(model) {
  private <- model$.__enclos_env__$private
  expect_identical(
    model$code(),
    "parameters { real beta; } model { beta ~ std_normal(); }"
  )
  expect_equal(model$variables()$parameters$beta$dimensions, 0)
  expect_match(paste(private$model_methods_env_$hpp_code_, collapse = "\n"), "beta")
  expect_match(paste(readLines(model$hpp_file()), collapse = "\n"), "beta")
  expect_true(model$cpp_options()$stan_threads)
  expect_match(readLines(model$exe_file()), "^mock executable ")
}

test_that("a leftover backup doesn't unwind a compile when warnings are errors", {
  model <- local_leftover_backup_model()
  model_dir <- dirname(model$exe_file())

  # The warning must come after the new executable state is committed.
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = expect_snapshot(
      error = TRUE,
      withr::with_options(
        list(warn = 2),
        model$compile(cpp_options = list(stan_threads = TRUE), force_recompile = TRUE)
      ),
      # Normalize Windows separators and the random backup name.
      transform = function(lines) {
        for (dir in unique(c(model_dir, repair_path(model_dir)))) {
          lines <- gsub(dir, "<dir>", lines, fixed = TRUE)
        }
        gsub("exe-old-[0-9a-f]+", "exe-old-<random>", lines)
      }
    )
  )

  expect_describes_new_program(model)
})

test_that("dir arg works for cmdstan_model and $compile()", {
  tmp_dir <- tempdir()
  tmp_dir_2 <- tempdir()

  mod_dir <- cmdstan_model(stan_program, dir = tmp_dir)
  expect_equal(repair_path(dirname(mod_dir$exe_file())), repair_path(tmp_dir))
  checkmate::expect_file_exists(mod_dir$exe_file())
  file.remove(mod_dir$exe_file())

  mod_dir_1 <- cmdstan_model(stan_program, dir = tmp_dir, compile = FALSE)
  mod_dir_1$compile(dry_run = TRUE)
  expect_equal(repair_path(dirname(mod_dir_1$exe_file())), repair_path(tmp_dir))

  mod_dir_1$compile(dir = tmp_dir_2, dry_run = TRUE) #dir in compile overwrites dir in cmdstan_model
  expect_equal(repair_path(dirname(mod_dir_1$exe_file())), repair_path(tmp_dir))

  mod_dir_2 <- cmdstan_model(stan_program, compile = FALSE)
  mod_dir_2$compile(dir = tmp_dir, dry_run = TRUE)
  expect_equal(repair_path(dirname(mod_dir_2$exe_file())), repair_path(tmp_dir))

  mod_dir_3 <- cmdstan_model(stan_program)
  mod_dir_3$compile(dir = tmp_dir, dry_run = TRUE) #dir in compile overwrites dir in cmdstan_model
  expect_equal(repair_path(dirname(mod_dir_3$exe_file())), repair_path(tmp_dir))

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
  expect_call_compilation(mod <- cmdstan_model(stan_file, stanc_options = list("warn-pedantic"), force_recompile = TRUE))
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
    mod_pedantic_warn <- cmdstan_model(stan_file, pedantic = TRUE, force_recompile = TRUE),
    "The parameter x was declared but was not used",
    fixed = TRUE
  )
})

test_that("*hpp_file() functions work", {
  tmp_dir <- tempdir()
  stan_file <- testing_stan_file("bernoulli")
  expect_call_compilation(mod <- cmdstan_model(stan_file, force_recompile = TRUE))
  checkmate::expect_file_exists(mod$hpp_file())
  expect_match(paste0(readLines(mod$hpp_file(), warn = FALSE), collapse = "\n"), "Code generated by stanc", fixed = TRUE)
  mod$save_hpp_file()
  expect_equal(mod$hpp_file(), file.path(dirname(mod$stan_file()), "bernoulli.hpp"))
  mod$save_hpp_file(tmp_dir)
  expect_equal(mod$hpp_file(), file.path(tmp_dir, "bernoulli.hpp"))
  # A dry run leaves the saved header location unchanged.
  mod$compile(force_recompile = TRUE, dry_run = TRUE)
  expect_equal(mod$hpp_file(), file.path(tmp_dir, "bernoulli.hpp"))
  # A real recompilation uses a fresh temporary header.
  expect_call_compilation(mod$compile(force_recompile = TRUE))
  expect_false(isTRUE(all.equal(mod$hpp_file(), file.path(tmp_dir, "bernoulli.hpp"))))
  expect_false(isTRUE(all.equal(mod$hpp_file(), file.path(dirname(mod$stan_file()), "bernoulli.hpp"))))
  checkmate::expect_file_exists(mod$hpp_file())
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
  expect_message(
    mod_ok$check_syntax(),
    "Stan program is syntactically correct"
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

  expect_message(
    mod_pedantic_warn$check_syntax(pedantic = TRUE),
    "The parameter x was declared but was not used",
    fixed = TRUE
  )
})

test_that("check_syntax() works with include_paths", {
  include_model <- local_include_model_with_spaces()

  mod_w_include <- cmdstan_model(
    stan_file = include_model$stan_file,
    compile = FALSE,
    include_paths = include_model$include_paths
  )
  expect_true(mod_w_include$check_syntax())

})

test_that("check_syntax() works with include_paths on compiled model", {
  stan_program_w_include <- testing_stan_file("bernoulli_include")

  mod_w_include <- cmdstan_model(stan_file = stan_program_w_include, compile=TRUE,
                                 include_paths = test_path("resources", "stan"))
  expect_true(mod_w_include$check_syntax())

})

test_that("check_syntax() and format() allow undefined functions with a user header", {
  stan_file <- testing_stan_file("bernoulli_external")
  # Stanc does not read the header, so an empty one is enough.
  user_header <- withr::local_tempfile(lines = "", fileext = ".hpp")
  mod <- cmdstan_model(stan_file, user_header = user_header, compile = FALSE)

  expect_true(mod$check_syntax(quiet = TRUE))
  expect_output(mod$format(), "make_odds", fixed = TRUE)

  # A compile that failed because the header is missing still counts as using one.
  mod_missing <- cmdstan_model(stan_file, compile = FALSE)
  expect_error(
    mod_missing$compile(user_header = "not_a_real_header.hpp"),
    "does not exist"
  )
  expect_true(mod_missing$check_syntax(quiet = TRUE))
})

test_that("compile() and check_syntax() error on removed syntax", {
  model_code <- "
  transformed data {
    real a;
    a <- 3;
  }
  "
  stan_file <- write_stan_file(model_code)
  mod_dep_warning <- cmdstan_model(stan_file, compile = FALSE)
  expect_error(
    mod_dep_warning$compile(),
    "An error occurred during compilation! See the message above for more information. (stanc exited with status 1)",
    fixed = TRUE
  )
  expect_error(
    mod_dep_warning$check_syntax(),
    "Syntax error found! See the message above for more information.",
    fixed = TRUE
  )
})

test_that("compilation errors if folder with the model name exists", {
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
  path_1_make <- if (grepl(" ", path_1_compare, fixed = TRUE)) {
    paste0("'", path_1_compare, "'")
  } else {
    path_1_compare
  }
  expect_equal(
    include_paths_stanc3_args(path_1),
    paste0("--include-paths=", path_1_make))
  path_2 <- file.path(tempdir(), "folder 2")
  if (!dir.exists(path_2)) {
    dir.create(path_2)
  }
  path_2 <- repair_path(path_2)
  path_2_compare <- ifelse(os_is_wsl(), wsl_safe_path(path_2), path_2)
  path_2_make <- paste0("'", path_2_compare, "'")
  expect_equal(
    include_paths_stanc3_args(c(path_1, path_2)),
    paste0("--include-paths=", path_1_make, ",", path_2_make)
  )
  expect_equal(
    include_paths_stanc3_args(
      c(path_1, path_2),
      direct_call = TRUE
    ),
    c("--include-paths", paste0(path_1_compare, ",", path_2_compare))
  )
})

test_that("cpp_options work with settings in make/local", {
  backup <- cmdstan_make_local()
  no_threads <- grep("STAN_THREADS", backup, invert = TRUE, value = TRUE)
  cmdstan_make_local(cpp_options = list(no_threads), append = FALSE)

  if (length(mod$exe_file()) > 0 && file.exists(mod$exe_file())) {
    file.remove(mod$exe_file())
  }

  rebuild_cmdstan()
  mod <- cmdstan_model(stan_file = stan_program)
  expect_null(mod$cpp_options()$STAN_THREADS)

  file.remove(mod$exe_file())

  cmdstan_make_local(cpp_options = list(stan_threads = TRUE), append = TRUE)

  file <- file.path(cmdstan_path(), "examples", "bernoulli", "bernoulli.stan")
  mod <- cmdstan_model(file)
  expect_true(mod$cpp_options()$STAN_THREADS)

  file.remove(mod$exe_file())

  # restore
  cmdstan_make_local(cpp_options = backup, append = FALSE)
})

test_that("cpp_options() excludes the Stan version reported by the executable", {
  mod <- cmdstan_model(stan_file = stan_program)
  expect_null(mod$cpp_options()$STAN_VERSION)
  expect_equal(mod$cmdstan_version(), cmdstan_version())
})

test_that("cmdstan_model works with exe_file", {
  stan_file <- testing_stan_file("bernoulli")
  mod <- cmdstan_model(stan_file, dry_run = TRUE)
  default_exe_file <- mod$exe_file()
  if(file.exists(mod$exe_file())) {
    file.remove(mod$exe_file())
  }

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
    exe_file = tmp_exe_file,
    dry_run = TRUE
  )
  expect_match(
    mod$exe_file(),
    repair_path(tmp_exe_file)
  )
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
  skip_if(os_is_macos())
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
  expect_call_compilation(
    mod <- cmdstan_model(
      stan_file = testing_stan_file("bernoulli_external"),
      user_header = tmpfile
  ))
  file.remove(mod$exe_file())

  # No stanc_options here: a user header supplied via cpp_options must enable
  # allow-undefined on its own (#1227)
  expect_call_compilation(
    mod_2 <- cmdstan_model(
      stan_file = testing_stan_file("bernoulli_external"),
      cpp_options=list(USER_HEADER=tmpfile)
    )
  )

  # Check recompilation upon changing header
  expect_no_recompilation(mod, quiet = TRUE, user_header = tmpfile)

  Sys.setFileTime(tmpfile, Sys.time() + 1) #touch file to trigger recompile
  expect_compilation(mod, quiet = TRUE, user_header = tmpfile)

  # Alternative spec of user header
  expect_no_recompilation(mod,
    quiet = TRUE,
    cpp_options = list(user_header = tmpfile),
    dry_run = TRUE
  )

  # Error/warning messages
  expect_error(
    cmdstan_model(
      stan_file = testing_stan_file("bernoulli_external"),
      cpp_options = list(USER_HEADER = "non_existent.hpp"),
      stanc_options = list("allow-undefined")
    ),
    "header file '[^']*' does not exist"
  )

  expect_warning(cmdstan_model(
    stan_file = testing_stan_file("bernoulli_external"),
    cpp_options = list(USER_HEADER = tmpfile, user_header = tmpfile),
    dry_run = TRUE),
    "User header specified both"
  )
  expect_warning(cmdstan_model(
    stan_file = testing_stan_file("bernoulli_external"),
    user_header = tmpfile,
    cpp_options = list(USER_HEADER = tmpfile),
    dry_run = TRUE),
    "User header specified both"
  )
})

test_that("cmdstan_model cpp_options dont capitalize cxxflags ", {
  file <- file.path(cmdstan_path(), "examples", "bernoulli", "bernoulli.stan")
  cpp_options <- list(
    "CXXFLAGS_OPTIM += -Dsomething_not_used"
  )
  withr::with_options(list("cmdstanr_verbose" = TRUE),
    out <- utils::capture.output(
      mod <- cmdstan_model(file, cpp_options = cpp_options, force_recompile = TRUE)
    )
  )
  expect_output(print(out), "-Dsomething_not_used")
})

test_that("format(overwrite_file = TRUE) refreshes cached variables", {
  model_dir <- withr::local_tempdir()
  stan_file <- write_stan_file(
    "parameters { real alpha; } model { alpha ~ std_normal(); }",
    dir = model_dir,
    basename = "reformat.stan"
  )
  model <- cmdstan_model(stan_file, compile = FALSE)
  expect_equal(names(model$variables()$parameters), "alpha")

  # Formatting in place must refresh variables along with the cached code.
  writeLines(
    "parameters { real beta; } model { beta ~ std_normal(); }",
    stan_file
  )
  model$format(overwrite_file = TRUE, quiet = TRUE)

  expect_equal(names(model$variables()$parameters), "beta")
  expect_match(paste(model$code(), collapse = " "), "beta")
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

  expect_error(
    mod_1$format(),
    "Syntax error found! See the message above for more information.",
    fixed = TRUE
  )

  expect_error(
    mod_1$format(),
    "Syntax error found! See the message above for more information.",
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
  include_model <- local_include_model_with_spaces()

  mod_w_include <- cmdstan_model(
    stan_file = include_model$stan_file,
    compile = FALSE,
    include_paths = include_model$include_paths
  )
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

test_that("format() works with include_paths on compiled model", {
  stan_program_w_include <- testing_stan_file("bernoulli_include")

  mod_w_include <- cmdstan_model(stan_file = stan_program_w_include, compile=TRUE,
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
  tmpdir <- withr::local_tempdir(pattern = "include path")
  stan_data_file <- write_stan_file(data_code, basename = "separate_file.stan", dir = tmpdir)
  stan_file <- write_stan_file(model_code, dir = tmpdir)

  mod_tmp <- cmdstan_model(stan_file, compile = FALSE)
  expect_true(mod_tmp$check_syntax())
  utils::capture.output(expect_true(mod_tmp$format()))
  expect_s3_class(mod_tmp$compile(), "CmdStanModel")
})

test_that("STANCFLAGS from get_cmdstan_flags() are included in compile output", {
  local_reproducible_output()
  real_get_cmdstan_flags <- get_cmdstan_flags
  local_mocked_bindings(
    get_cmdstan_flags = function(flag_name) {
      if (identical(flag_name, "STANCFLAGS")) {
        c("--O1", "--warn-pedantic")
      } else {
        real_get_cmdstan_flags(flag_name)
      }
    }
  )
  out <- utils::capture.output(mod$compile(quiet = FALSE, force_recompile = TRUE))
  if(os_is_windows() && !os_is_wsl()) {
    out_w_flags <- "bin/stanc.exe --name=bernoulli_model[[:space:]]+--O1[[:space:]]+--warn-pedantic[[:space:]]+--o"
  } else {
    out_w_flags <- "bin/stanc --name=bernoulli_model[[:space:]]+--O1[[:space:]]+--warn-pedantic[[:space:]]+--o"
  }
  expect_output(print(out), out_w_flags)
})

test_that("stanc_options_to_args() builds direct and Make-quoted arguments", {
  # Unnamed options are already flag names and are never quoted
  expect_equal(stanc_options_to_args(list("allow-undefined")), "--allow-undefined")
  expect_equal(
    stanc_options_to_args(list("allow-undefined"), quote_values = TRUE),
    "--allow-undefined"
  )

  # Logical values mark boolean flags
  expect_equal(stanc_options_to_args(list("warn-pedantic" = TRUE)), "--warn-pedantic")
  expect_equal(stanc_options_to_args(list("warn-pedantic" = FALSE)), NULL)

  # Values are quoted only for Make (#1227)
  expect_equal(
    stanc_options_to_args(list(canonicalize = "deprecations")),
    "--canonicalize=deprecations"
  )
  expect_equal(
    stanc_options_to_args(list(canonicalize = "deprecations"), quote_values = TRUE),
    "--canonicalize='deprecations'"
  )

  # Quoting the model name mangles the generated namespace
  expect_equal(
    stanc_options_to_args(list(name = "m_model"), quote_values = TRUE),
    "--name=m_model"
  )

  # Numeric values are kept rather than collapsed to a bare flag (#1233)
  expect_equal(
    stanc_options_to_args(list("max-line-length" = 78)),
    "--max-line-length=78"
  )

  expect_equal(stanc_options_to_args(list()), NULL)
  expect_equal(stanc_options_to_args(NULL), NULL)
})

test_that("compile() passes unquoted named stanc options to direct calls", {
  stan_file <- testing_stan_file("bernoulli")
  model <- cmdstan_model(stan_file, compile = FALSE)
  received_stancflags <- list()
  local_mocked_bindings(
    get_cmdstan_flags = function(flag_name) character(),
    get_standalone_hpp = function(stan_file, stancflags) {
      received_stancflags <<- append(received_stancflags, list(stancflags))
      ""
    }
  )

  model$compile(
    stanc_options = list(
      canonicalize = "deprecations",
      "filename-in-msg" = "model filename with spaces.stan"
    ),
    force_recompile = TRUE,
    dry_run = TRUE
  )

  expected <- c(
    "--canonicalize=deprecations",
    "--filename-in-msg=model filename with spaces.stan"
  )
  direct_options <- lapply(received_stancflags, function(x) {
    grep("^--(canonicalize|filename-in-msg)=", x, value = TRUE)
  })
  expect_length(received_stancflags, 2)
  expect_equal(direct_options, rep(list(expected), 2))
  expect_equal(
    grep("'", unlist(received_stancflags), fixed = TRUE, value = TRUE),
    character()
  )
})

test_that("compile() works with named stanc option values", {
  stan_file <- write_stan_file(
    "
    functions {
      real half(real x) {
        return x / 2;
      }
    }
    parameters {
      real y;
    }
    model {
      y ~ std_normal();
    }
    ",
    dir = withr::local_tempdir(),
    basename = "issue1227.stan"
  )

  expect_call_compilation(
    model <- cmdstan_model(
      stan_file,
      stanc_options = list(
        canonicalize = "deprecations",
        "filename-in-msg" = "model filename with spaces.stan"
      )
    )
  )
})

test_that("compile() detects stan_opencl without case or partial matching", {
  stan_file <- testing_stan_file("bernoulli")
  model <- cmdstan_model(stan_file, compile = FALSE)
  received_stancflags <- list()
  local_mocked_bindings(
    get_cmdstan_flags = function(flag_name) character(),
    get_standalone_hpp = function(stan_file, stancflags) {
      received_stancflags <<- append(received_stancflags, list(stancflags))
      ""
    }
  )

  model$compile(
    cpp_options = list(STAN_OPENCL = TRUE),
    force_recompile = TRUE,
    dry_run = TRUE
  )
  expect_length(received_stancflags, 2)
  expect_equal(
    vapply(
      received_stancflags,
      function(x) "--use-opencl" %in% x,
      logical(1)
    ),
    rep(TRUE, length(received_stancflags))
  )

  received_stancflags <- list()
  model$compile(
    cpp_options = list(stan_opencl_x = TRUE),
    force_recompile = TRUE,
    dry_run = TRUE
  )
  expect_length(received_stancflags, 2)
  expect_equal(
    vapply(
      received_stancflags,
      function(x) "--use-opencl" %in% x,
      logical(1)
    ),
    rep(FALSE, length(received_stancflags))
  )
})

test_that("compile() ignores directory chatter from MAKEFLAGS when reading STANCFLAGS", {
  withr::local_envvar(MAKEFLAGS = "-w -j 4")
  expect_compilation(mod, quiet = TRUE, force_recompile = TRUE)
})

test_that("compile() checks it can commit before replacing the executable", {
  model_dir <- withr::local_tempdir()
  stan_file <- file.path(model_dir, "bernoulli.stan")
  file.copy(testing_stan_file("bernoulli"), stan_file)
  model <- cmdstan_model(stan_file, compile = FALSE)
  exe <- cmdstan_ext(strip_ext(stan_file))

  lockEnvironment(model$functions, bindings = FALSE)

  # Clearing a locked environment would fail during the state commit.
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = expect_error(
      model$compile(force_recompile = TRUE),
      "missing or locked",
      fixed = TRUE
    )
  )
  expect_false(file.exists(exe))
  expect_length(model$exe_file(), 0)
})

test_that("compile() refuses an executable destination that is a directory", {
  model_dir <- withr::local_tempdir()
  stan_file <- file.path(model_dir, "bernoulli.stan")
  file.copy(testing_stan_file("bernoulli"), stan_file)
  destination <- file.path(model_dir, "target-dir")
  dir.create(destination)
  writeLines("important", file.path(destination, "data.txt"))

  model <- cmdstan_model(stan_file, compile = FALSE)
  model$exe_file(destination)

  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = expect_error(
      model$compile(force_recompile = TRUE),
      "is a directory",
      fixed = TRUE
    )
  )
  expect_true(dir.exists(destination))
  expect_identical(readLines(file.path(destination, "data.txt")), "important")
})

test_that("compile() installs the artifact it just built, not the previous one", {
  model_dir <- withr::local_tempdir()
  stan_file <- file.path(model_dir, "bernoulli.stan")
  file.copy(testing_stan_file("bernoulli"), stan_file)
  model <- cmdstan_model(stan_file, compile = FALSE)
  exe <- cmdstan_ext(strip_ext(stan_file))

  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = {
      model$compile(force_recompile = TRUE)
      first <- readLines(exe)
      model$compile(force_recompile = TRUE)
      second <- readLines(exe)
    }
  )
  expect_false(identical(first, second))
})
