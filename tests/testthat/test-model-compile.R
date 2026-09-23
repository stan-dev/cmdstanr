set_cmdstan_path()
stan_program <- cmdstan_example_file()
local_cmdstan_make_local(cpp_options = list("PRECOMPILED_HEADERS"="false"))
mod <- cmdstan_model(stan_file = stan_program)

test_that("object initialized correctly", {
  expect_equal(mod$stan_file(), stan_program)
  expect_equal(mod$exe_file(), cmdstan_ext(strip_ext(stan_program)))
  checkmate::expect_file_exists(mod$exe_file())
  checkmate::expect_file_exists(mod$hpp_file())
})

test_that("cmdstan_model() builds an executable and then reuses it", {
  exe <- cmdstan_ext(strip_ext(stan_program))
  if (file.exists(exe)) {
    file.remove(exe)
  }
  built <- expect_compilation(cmdstan_model(stan_program, quiet = TRUE))
  checkmate::expect_file_exists(exe)
  checkmate::expect_file_exists(built$hpp_file())
  expect_no_recompilation(cmdstan_model(stan_program, quiet = TRUE))

  out <- utils::capture.output(
    cmdstan_model(stan_program, quiet = FALSE, force_recompile = TRUE)
  )
  expect_output(print(out), "Translating Stan model")
})

test_that("force_recompile = TRUE rebuilds", {
  expect_compilation(
    cmdstan_model(stan_program, quiet = TRUE, force_recompile = TRUE)
  )
})

test_that("compiling works with spaces in path", {
  stan_file <- testing_stan_file("bernoulli")
  stan_model_with_spaces <- testing_stan_file("folder spaces/bernoulli spaces")

  dir_with_spaces <- test_path("resources", "stan", "folder spaces")
  if (!file.exists(dir_with_spaces)) {
    dir.create(dir_with_spaces)
  }
  file.copy(stan_file, stan_model_with_spaces)

  expect_compilation(cmdstan_model(stan_model_with_spaces))
  unlink(dir_with_spaces, recursive = TRUE)
})

test_that("compilation works with include_paths", {
  stan_program_w_include <- testing_stan_file("bernoulli_include")
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

  mod_w_include <- expect_compilation(
    cmdstan_model(stan_file = stan_program_w_include, quiet = TRUE,
                  include_paths = test_path("resources", "stan"),
                  force_recompile = TRUE)
  )
  expect_equal(
    mod_w_include$exe_file(),
    cmdstan_ext(strip_ext(absolute_path(stan_program_w_include)))
  )
})

test_that("include paths are resolved when the model is created", {
  model_dir <- withr::local_tempdir()
  file.copy(
    c(testing_stan_file("bernoulli_include"), testing_stan_file("divide_real_by_two")),
    model_dir
  )
  mod <- withr::with_dir(
    model_dir,
    mock_cmdstan_model("bernoulli_include.stan")
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
    mock_cmdstan_model("bernoulli_include.stan", include_paths = "includes")
  )
  # "includes" no longer resolves relative to the working directory
  expect_true(mod$check_syntax(quiet = TRUE))
})

test_that("the model name stanc receives comes from the file name", {
  local_reproducible_output()
  out <- utils::capture.output(
    cmdstan_model(stan_program, quiet = FALSE, force_recompile = TRUE)
  )
  if(os_is_windows() && !os_is_wsl()) {
    out_no_name <- "bin/stanc.exe --name=bernoulli_model[[:space:]]+--filename-in-msg=[^[:space:]]+[[:space:]]+--o"
  } else {
    out_no_name <- "bin/stanc --name=bernoulli_model[[:space:]]+--filename-in-msg=[^[:space:]]+[[:space:]]+--o"
  }
  expect_output(print(out), out_no_name)

  expect_error(
    cmdstan_model(stan_program, force_recompile = TRUE,
                  stanc_options = list(name = "bernoulli2_model")),
    "The model name comes from the name of the Stan file.",
    fixed = TRUE
  )
})

test_that("multiple cpp_options work", {
  stan_file <- testing_stan_file("bernoulli")
  mod_options <- expect_compilation(
    cmdstan_model(
      stan_file,
      cpp_options = list("DUMMY_TEST2"="1", "DUMMY_TEST2"="1", "DUMMY_TEST3"="1"),
      force_recompile = TRUE
    )
  )
  expect_equal(
    mod_options$cpp_options(),
    list(DUMMY_TEST2 = "1", DUMMY_TEST3 = "1")
  )
})

test_that("a program stanc rejects errors with stanc's message", {
  stan_file <- testing_stan_file("fail")
  expect_error(
    cmdstan_model(stan_file),
    "fail.stan(', line 7|:7:)"
  )
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

# A model for a program that cannot be built, such as one with a syntax error
# or an undefined function. The object is built from a valid program and the
# program under test takes its place afterwards, which is all the methods that
# read the source ever look at.
local_source_only_model <- function(stan_file, .local_envir = parent.frame()) {
  path <- file.path(
    withr::local_tempdir(.local_envir = .local_envir),
    basename(stan_file)
  )
  file.copy(testing_stan_file("bernoulli"), path)
  model <- mock_cmdstan_model(path, .local_envir = .local_envir)
  file.copy(stan_file, path, overwrite = TRUE)
  model
}

test_that("a failed build leaves the previous executable and its record alone", {
  model <- local_mocked_bernoulli_model()
  exe_before <- readLines(model$exe_file())
  record_before <- readLines(build_record_path(model$exe_file()))
  hpp_before <- readLines(model$hpp_file())

  writeLines(
    "parameters { real beta; } model { beta ~ std_normal(); }",
    model$stan_file()
  )
  with_mocked_cli(
    compile_ret = list(status = 1),
    info_ret = list(status = 1),
    code = expect_error(
      cmdstan_model(model$stan_file()),
      "An error occurred during compilation!",
      fixed = TRUE
    )
  )

  expect_identical(readLines(model$exe_file()), exe_before)
  expect_identical(
    readLines(build_record_path(model$exe_file())), record_before
  )
  expect_identical(readLines(model$hpp_file()), hpp_before)
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
  expect_identical(
    model$code(),
    "parameters { real beta; } model { beta ~ std_normal(); }"
  )
  expect_equal(model$variables()$parameters$beta$dimensions, 0)
  expect_match(paste(readLines(model$hpp_file()), collapse = "\n"), "beta")
  expect_equal(model$cpp_options()$STAN_THREADS, "TRUE")
  expect_match(readLines(model$exe_file()), "^mock executable ")
}

test_that("a leftover backup doesn't unwind a build when warnings are errors", {
  model <- local_leftover_backup_model()
  model_dir <- dirname(model$exe_file())
  stan_file <- model$stan_file()

  # The warning must come after the new executable is installed.
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = expect_snapshot(
      error = TRUE,
      withr::with_options(
        list(warn = 2),
        cmdstan_model(stan_file, cpp_options = list(stan_threads = TRUE))
      ),
      # Normalize Windows separators and the random backup names.
      transform = function(lines) {
        for (dir in unique(c(model_dir, repair_path(model_dir)))) {
          lines <- gsub(dir, "<dir>", lines, fixed = TRUE)
        }
        lines <- gsub("exe-old-[0-9a-f]+", "exe-old-<random>", lines)
        gsub("record-old-[0-9a-f]+", "record-old-<random>", lines)
      }
    )
  )

  # The install stands, so the next cmdstan_model() reuses what it left behind.
  rebuilt <- with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = expect_no_mock_compile(
      cmdstan_model(stan_file, cpp_options = list(stan_threads = TRUE))
    )
  )
  expect_describes_new_program(rebuilt)
})

test_that("dir arg works for cmdstan_model", {
  tmp_dir <- withr::local_tempdir()

  mod_dir <- cmdstan_model(stan_program, dir = tmp_dir)
  expect_equal(repair_path(dirname(mod_dir$exe_file())), repair_path(tmp_dir))
  checkmate::expect_file_exists(mod_dir$exe_file())

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
    cmdstan_model(stan_file, stanc_options = hyphens),
    "No leading hyphens allowed in stanc options (--allow-undefined). Use options without leading hyphens, for example `stanc_options = list('warn-uninitialized')`",
    fixed = TRUE
  )
  expect_error(
    cmdstan_model(stan_file, stanc_options = hyphens2),
    "No leading hyphens allowed in stanc options (--allow-undefined). Use options without leading hyphens, for example `stanc_options = list('warn-uninitialized')`",
    fixed = TRUE
  )
  expect_error(
    cmdstan_model(stan_file, stanc_options = hyphens3),
    "No leading hyphens allowed in stanc options (--o). Use options without leading hyphens, for example `stanc_options = list('warn-uninitialized')`",
    fixed = TRUE
  )
})

test_that("compiling stops on stanc options cmdstanr sets itself", {
  stan_file <- testing_stan_file("bernoulli")
  fragments <- list(
    "include-paths" = "Pass the directories with the `include_paths` argument.",
    "warn-pedantic" = "Use `pedantic = TRUE`.",
    "allow-undefined" = "Builds turn it on when a `user_header` is supplied",
    "use-opencl" = "Use `cpp_options = list(stan_opencl = TRUE)`, which turns it on.",
    "name" = "The model name comes from the name of the Stan file."
  )
  for (flag in names(fragments)) {
    fragment <- fragments[[flag]]
    spellings <- list(
      list(flag),
      list(paste0(flag, "=yes")),
      setNames(list(TRUE), flag),
      setNames(list(FALSE), flag),
      setNames(list(NA), flag),
      setNames(list(NULL), flag),
      setNames(list("yes"), flag)
    )
    for (spelling in spellings) {
      expect_error(
        cmdstan_model(stan_file, stanc_options = spelling),
        fragment,
        fixed = TRUE
      )
    }
  }

  # A named entry carrying its own value still matches on the flag name.
  expect_error(
    cmdstan_model(stan_file, stanc_options = list("include-paths=/b" = TRUE)),
    "Pass the directories with the `include_paths` argument.",
    fixed = TRUE
  )
})

test_that("stanc_options names cannot carry their own value", {
  expect_error(
    cmdstan_model(
      testing_stan_file("bernoulli"),
      stanc_options = list("max-line-length=78" = TRUE)
    ),
    "`list(\"max-line-length\" = \"78\")`",
    fixed = TRUE
  )
})

test_that("compiling works with only names in list", {
  stan_file <- testing_stan_file("bernoulli")
  mod <- expect_compilation(
    cmdstan_model(stan_file, stanc_options = list("warn-uninitialized"),
                  force_recompile = TRUE)
  )
  checkmate::expect_r6(
    mod,
    "CmdStanModel"
  )
})

test_that("pedantic = TRUE warns on a build and on a reuse", {
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
    cmdstan_model(stan_file, pedantic = TRUE, force_recompile = TRUE),
    "The parameter x was declared but was not used",
    fixed = TRUE
  )
  # The executable is current, so the warning comes from stanc alone.
  expect_message(
    expect_no_recompilation(cmdstan_model(stan_file, pedantic = TRUE)),
    "The parameter x was declared but was not used",
    fixed = TRUE
  )
})

test_that("*hpp_file() functions work", {
  tmp_dir <- withr::local_tempdir()
  stan_file <- testing_stan_file("bernoulli")
  mod <- cmdstan_model(stan_file)
  checkmate::expect_file_exists(mod$hpp_file())
  expect_match(paste0(readLines(mod$hpp_file(), warn = FALSE), collapse = "\n"), "Code generated by stanc", fixed = TRUE)
  mod$save_hpp_file()
  expect_equal(mod$hpp_file(), file.path(dirname(mod$stan_file()), "bernoulli.hpp"))
  mod$save_hpp_file(tmp_dir)
  expect_equal(mod$hpp_file(), file.path(tmp_dir, "bernoulli.hpp"))

  # A model on a reused executable holds generated C++ of its own.
  reused <- cmdstan_model(stan_file)
  checkmate::expect_file_exists(reused$hpp_file())
  expect_false(isTRUE(all.equal(reused$hpp_file(), mod$hpp_file())))
})

test_that("check_syntax() works", {
  mod_fail <- local_source_only_model(testing_stan_file("fail"))
  expect_error(
    expect_message(
      mod_fail$check_syntax(),
      "Ill-typed arguments supplied to assignment operator"
    ),
    "Syntax error found! See the message above for more information."
  )

  stan_file <- testing_stan_file("bernoulli")
  mod_ok <- cmdstan_model(stan_file)
  expect_message(
    mod_ok$check_syntax(),
    "Stan program is syntactically correct"
  )
  expect_message(
    mod_ok$check_syntax(quiet = TRUE),
    regexp = NA
  )
  expect_message(
    mod_ok$check_syntax(stanc_options = list("warn-uninitialized")),
    "Stan program is syntactically correct",
    fixed = TRUE
  )
  expect_message(
    mod_ok$check_syntax(stanc_options = list("warn-uninitialized"), quiet = TRUE),
    regexp = NA
  )
  expect_error(
    mod_ok$check_syntax(stanc_options = list("warn-pedantic")),
    "Use `pedantic = TRUE`.",
    fixed = TRUE
  )
  expect_error(
    mod_ok$check_syntax(stanc_options = list("allow-undefined")),
    "Builds turn it on when a `user_header` is supplied",
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
  mod_removed_stan_file <- mock_cmdstan_model(stan_file_tmp)
  file.remove(stan_file_tmp)
  expect_error(
    mod_removed_stan_file$check_syntax(),
    "this model was created from no longer exists", fixed = TRUE
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
  mod_pedantic_warn <- mock_cmdstan_model(stan_file)
  expect_message(
    mod_pedantic_warn$check_syntax(),
    "Stan program is syntactically correct"
  )

  expect_message(
    mod_pedantic_warn$check_syntax(pedantic = TRUE),
    "The parameter x was declared but was not used",
    fixed = TRUE
  )

  # pedantic mode has one channel here as well
  expect_error(
    mod_pedantic_warn$check_syntax(stanc_options = list("warn-pedantic" = TRUE)),
    "Use `pedantic = TRUE`.",
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

  mod_w_include <- mock_cmdstan_model(
    stan_file = include_model$stan_file,
    include_paths = include_model$include_paths
  )
  expect_true(mod_w_include$check_syntax())

})

test_that("check_syntax() works with include_paths on compiled model", {
  stan_program_w_include <- testing_stan_file("bernoulli_include")

  mod_w_include <- cmdstan_model(stan_file = stan_program_w_include,
                                 include_paths = test_path("resources", "stan"))
  expect_true(mod_w_include$check_syntax())

})

test_that("check_syntax() and format() allow undefined functions with a user header", {
  stan_file <- file.path(
    withr::local_tempdir(), "bernoulli_external.stan"
  )
  file.copy(testing_stan_file("bernoulli_external"), stan_file)
  # Stanc does not read the header, so an empty one is enough.
  user_header <- withr::local_tempfile(lines = "", fileext = ".hpp")
  mod <- mock_cmdstan_model(stan_file, user_header = user_header)

  expect_true(mod$check_syntax(quiet = TRUE))
  expect_output(mod$format(), "make_odds", fixed = TRUE)
})

test_that("building errors on removed syntax", {
  model_code <- "
  transformed data {
    real a;
    a <- 3;
  }
  "
  stan_file <- write_stan_file(model_code)
  expect_error(cmdstan_model(stan_file), "Syntax error.*(line 4|:4:)")
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
    STAN_THREADS = TRUE
  )
  expect_equal(cpp_options_to_compile_flags(options), "STAN_THREADS=TRUE")
  options = list(
    STAN_THREADS = TRUE,
    STANC2 = TRUE
  )
  expect_equal(cpp_options_to_compile_flags(options), c("STAN_THREADS=TRUE", "STANC2=TRUE"))
  options = list()
  expect_equal(cpp_options_to_compile_flags(options), NULL)

  # FALSE and NULL both ask for the option off, which make spells as an empty
  # assignment. The string "FALSE" is a value like any other.
  expect_equal(
    cpp_options_to_compile_flags(list(STAN_THREADS = FALSE)),
    "STAN_THREADS="
  )
  expect_equal(
    cpp_options_to_compile_flags(list(STAN_THREADS = NULL)),
    "STAN_THREADS="
  )
  expect_equal(
    cpp_options_to_compile_flags(list(STAN_THREADS = "FALSE")),
    "STAN_THREADS=FALSE"
  )
  expect_equal(
    cpp_options_to_compile_flags(list(STAN_THREADS = c(TRUE, FALSE))),
    c("STAN_THREADS=TRUE", "STAN_THREADS=")
  )
})

test_that("cpp_options() reports the options that were supplied", {
  expect_equal(mod$cpp_options(), structure(list(), names = character()))

  stan_file <- file.path(withr::local_tempdir(), "bernoulli.stan")
  file.copy(testing_stan_file("bernoulli"), stan_file)
  threaded <- mock_cmdstan_model(
    stan_file, cpp_options = list(stan_threads = TRUE)
  )
  expect_equal(threaded$cpp_options(), list(STAN_THREADS = "TRUE"))
})

test_that("cmdstan_version() reports the version that built an adopted executable", {
  stan_file <- file.path(withr::local_tempdir(), "bernoulli.stan")
  file.copy(testing_stan_file("bernoulli"), stan_file)
  older <- paste0(
    "stan_version_major=2\nstan_version_minor=35\nstan_version_patch=0"
  )
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 0, stdout = older),
    code = mod <- cmdstan_model(stan_file, force_recompile = TRUE)
  )
  # Built here, so the session's CmdStan built it whatever info reports.
  expect_equal(mod$cmdstan_version(), cmdstan_version())

  # The record beside it says what built it, so the binary is not asked.
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 0, stdout = older),
    code = adopted <- cmdstan_model(exe_file = mod$exe_file())
  )
  expect_equal(adopted$cmdstan_version(), cmdstan_version())

  # Without a record, the executable is run with 'info' to learn what it
  # was built with.
  alone_exe <- file.path(withr::local_tempdir(), basename(mod$exe_file()))
  file.copy(mod$exe_file(), alone_exe)
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 0, stdout = older),
    code = alone <- cmdstan_model(exe_file = alone_exe)
  )
  expect_equal(alone$cmdstan_version(), "2.35.0")

  # An executable that reports no version is not a CmdStan executable.
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 0, stdout = "STAN_THREADS=false"),
    code = expect_error(
      cmdstan_model(exe_file = alone_exe),
      "did not report a Stan version",
      fixed = TRUE
    )
  )
})

test_that("cmdstan_model takes a stan_file or an exe_file, not both", {
  expect_error(
    cmdstan_model(stan_file = stan_program, exe_file = mod$exe_file()),
    "`stan_file` and `exe_file` cannot both be supplied.",
    fixed = TRUE
  )

  adopted <- cmdstan_model(exe_file = mod$exe_file())
  expect_equal(adopted$exe_file(), mod$exe_file())
  expect_false(adopted$has_stan_file())
})

test_that("build configuration cannot accompany an executable-only model", {
  exe <- mod$exe_file()

  expect_error(
    cmdstan_model(exe_file = exe, cpp_options = list(stan_threads = TRUE)),
    "`cpp_options` cannot be supplied",
    fixed = TRUE
  )
  expect_error(
    cmdstan_model(exe_file = exe, stanc_options = list("O1")),
    "`stanc_options` cannot be supplied",
    fixed = TRUE
  )
  expect_error(
    cmdstan_model(exe_file = exe, include_paths = tempdir()),
    "`include_paths` cannot be supplied",
    fixed = TRUE
  )
  expect_error(
    cmdstan_model(exe_file = exe, user_header = tempfile(fileext = ".hpp")),
    "`user_header` cannot be supplied",
    fixed = TRUE
  )
  expect_error(
    cmdstan_model(exe_file = exe, force_recompile = TRUE),
    "`force_recompile` cannot be supplied",
    fixed = TRUE
  )
  expect_error(
    cmdstan_model(exe_file = exe, force_recompile = FALSE),
    "`force_recompile` cannot be supplied",
    fixed = TRUE
  )
  expect_error(
    cmdstan_model(exe_file = exe, pedantic = TRUE),
    "`pedantic` cannot be supplied",
    fixed = TRUE
  )
  expect_error(
    cmdstan_model(exe_file = exe, dir = tempdir()),
    "`dir` cannot be supplied",
    fixed = TRUE
  )

  mod_exe <- cmdstan_model(
    exe_file = exe,
    cpp_options = NULL,
    stanc_options = NULL,
    include_paths = NULL,
    user_header = NULL,
    force_recompile = NULL,
    pedantic = NULL,
    dir = NULL
  )
  expect_false(mod_exe$has_stan_file())

  withr::local_options(cmdstanr_force_recompile = TRUE)
  expect_no_error(cmdstan_model(exe_file = exe))

  expect_error(
    cmdstan_model(exe_file = file.path(tempdir(), "missing"), pedantic = TRUE),
    "`pedantic` cannot be supplied",
    fixed = TRUE
  )
})

test_that("a model created only with exe_file refuses what needs a Stan file", {
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
    mod_exe$hpp_file(),
    "'$hpp_file()' cannot be used because the 'CmdStanModel' was not created with a Stan file.",
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
  stan_file <- testing_stan_file("bernoulli_external")

  # No stanc_options here: the user header argument must enable allow-undefined
  # on its own (#1227)
  mod <- expect_compilation(
    cmdstan_model(stan_file = stan_file, user_header = tmpfile)
  )
  expect_equal(mod$user_header(), resolve_path(tmpfile))
  expect_false("USER_HEADER" %in% names(mod$cpp_options()))

  expect_no_recompilation(
    cmdstan_model(stan_file = stan_file, user_header = tmpfile)
  )

  # Check recompilation upon changing header
  cat("\n", file = tmpfile, append = TRUE)
  expect_compilation(
    cmdstan_model(stan_file = stan_file, user_header = tmpfile, quiet = TRUE)
  )

  # Error messages
  expect_error(
    cmdstan_model(
      stan_file = stan_file,
      user_header = "non_existent.hpp"
    ),
    "header file '[^']*' does not exist"
  )
})

test_that("cpp_options names reach make uppercased and values verbatim", {
  file <- file.path(cmdstan_path(), "examples", "bernoulli", "bernoulli.stan")
  expect_error(
    cmdstan_model(
      file,
      cpp_options = list("CXXFLAGS_OPTIM += -Dsomething_not_used"),
      force_recompile = TRUE
    ),
    "cmdstan_make_local(cpp_options = list(\"CXXFLAGS_OPTIM += -Dsomething_not_used\"))",
    fixed = TRUE
  )

  withr::with_options(list("cmdstanr_verbose" = TRUE),
    out <- utils::capture.output(
      mod <- cmdstan_model(
        file,
        cpp_options = list(CXXFLAGS_OPTIM = "-Dsomething_not_used"),
        force_recompile = TRUE
      )
    )
  )
  expect_output(print(out), "CXXFLAGS_OPTIM=-Dsomething_not_used", fixed = TRUE)
})

test_that("format(overwrite_file = TRUE) leaves the object's code and variables alone", {
  model_dir <- withr::local_tempdir()
  stan_file <- write_stan_file(
    "parameters { real alpha; } model { alpha ~ std_normal(); }",
    dir = model_dir,
    basename = "reformat.stan"
  )
  model <- mock_cmdstan_model(stan_file)
  expect_equal(names(model$variables()$parameters), "alpha")

  writeLines(
    "parameters { real beta; } model { beta ~ std_normal(); }",
    stan_file
  )
  model$format(overwrite_file = TRUE, quiet = TRUE)

  expect_match(paste(readLines(stan_file), collapse = " "), "beta")
  expect_equal(names(model$variables()$parameters), "alpha")
  code <- paste(model$code(), collapse = " ")
  expect_match(code, "alpha")
  expect_false(grepl("beta", code, fixed = TRUE))
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
  mod_1 <- local_source_only_model(write_stan_file(code))

  expect_error(
    mod_1$format(),
    "Syntax error found! See the message above for more information.",
    fixed = TRUE
  )

  mod_2 <- local_source_only_model(testing_stan_file("bernoulli_external"))
  expect_output(
    mod_2$format(),
    "make_odds(theta);",
    fixed = TRUE
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
  mod_removed_stan_file <- mock_cmdstan_model(stan_file_tmp)
  file.remove(stan_file_tmp)
  expect_error(
    mod_removed_stan_file$format(),
    "this model was created from no longer exists", fixed = TRUE
  )
  mod_exe <- cmdstan_model(exe_file = mod_removed_stan_file$exe_file())
  expect_error(
    mod_exe$format(),
    "'$format()' cannot be used because the 'CmdStanModel' was not created with a Stan file.",
    fixed = TRUE
  )
})

test_that("source-only operations do not need a user header to allow undefined functions", {
  mod <- local_source_only_model(testing_stan_file("bernoulli_external"))
  expect_message(
    expect_true(mod$check_syntax()),
    "Stan program is syntactically correct"
  )
  expect_output(
    mod$format(),
    "make_odds(theta);",
    fixed = TRUE
  )
})

test_that("format() works with include_paths", {
  include_model <- local_include_model_with_spaces()

  mod_w_include <- mock_cmdstan_model(
    stan_file = include_model$stan_file,
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

  mod_w_include <- cmdstan_model(stan_file = stan_program_w_include,
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
  mod_1 <- mock_cmdstan_model(stan_file_tmp)
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

  mod_tmp <- mock_cmdstan_model(stan_file)
  expect_s3_class(mod_tmp, "CmdStanModel")
  expect_true(mod_tmp$check_syntax())
  utils::capture.output(expect_true(mod_tmp$format()))
})

test_that("STANCFLAGS from get_cmdstan_flags() are included in compile output", {
  local_reproducible_output()
  real_get_cmdstan_flags <- get_cmdstan_flags
  local_mocked_bindings(
    get_cmdstan_flags = function(flag_name, ...) {
      if (identical(flag_name, "STANCFLAGS")) {
        c("--O1", "--warn-pedantic")
      } else {
        real_get_cmdstan_flags(flag_name, ...)
      }
    }
  )
  out <- utils::capture.output(
    cmdstan_model(stan_program, quiet = FALSE, force_recompile = TRUE)
  )
  if(os_is_windows() && !os_is_wsl()) {
    out_w_flags <- "bin/stanc.exe --name=bernoulli_model[[:space:]]+--filename-in-msg=[^[:space:]]+[[:space:]]+--O1[[:space:]]+--warn-pedantic[[:space:]]+--o"
  } else {
    out_w_flags <- "bin/stanc --name=bernoulli_model[[:space:]]+--filename-in-msg=[^[:space:]]+[[:space:]]+--O1[[:space:]]+--warn-pedantic[[:space:]]+--o"
  }
  expect_output(print(out), out_w_flags)

  # The call emits --warn-pedantic, so make/local's copy is dropped and the
  # stanc command line make prints holds the flag once.
  out <- utils::capture.output(
    cmdstan_model(stan_program, pedantic = TRUE, quiet = FALSE,
                  force_recompile = TRUE)
  )
  stanc_lines <- out[grepl("bin/stanc", out)]
  expect_gt(length(stanc_lines), 0)
  expect_equal(
    lengths(regmatches(
      stanc_lines,
      gregexpr("--warn-pedantic", stanc_lines, fixed = TRUE)
    )),
    rep(1L, length(stanc_lines))
  )
})

test_that("a quoted make/local flag the call emits is dropped whole (#1232)", {
  # Nothing is mocked. The call always emits --filename-in-msg, so the
  # make/local copy is dropped, and it has to go as one argument: a split at
  # the space would leave a stray "model.stan'" word for stanc to choke on.
  local_reproducible_output()
  local_cmdstan_make_local(
    cpp_options = list("STANCFLAGS += --filename-in-msg='/my dir/model.stan'")
  )
  expect_equal(get_cmdstan_flags("STANCFLAGS"), "--filename-in-msg=/my dir/model.stan")

  stan_file <- file.path(withr::local_tempdir(), "bernoulli.stan")
  file.copy(stan_program, stan_file)
  out <- utils::capture.output(
    mod_local <- cmdstan_model(stan_file, quiet = FALSE)
  )
  expect_true(file.exists(mod_local$exe_file()))
  stanc_line <- grep("bin/stanc", out, value = TRUE)
  expect_length(stanc_line, 1)
  expect_match(stanc_line, wsl_safe_path(mod_local$stan_file()), fixed = TRUE)
  expect_false(grepl("my dir", stanc_line, fixed = TRUE))
  expect_false(grepl("model.stan'", stanc_line, fixed = TRUE))
})

test_that("include paths in make/local STANCFLAGS stop the build", {
  # Use a temporary copy because mocked compiles install executables.
  model_dir <- withr::local_tempdir()
  stan_file <- file.path(model_dir, "bernoulli.stan")
  file.copy(testing_stan_file("bernoulli"), stan_file)
  local_flags <- NULL
  received_stancflags <- list()
  local_mocked_bindings(
    get_cmdstan_flags = function(flag_name, ...) {
      if (identical(flag_name, "STANCFLAGS")) local_flags else character()
    },
    get_standalone_hpp = function(stan_file, stancflags, ...) {
      received_stancflags <<- append(received_stancflags, list(stancflags))
      ""
    }
  )

  for (flags in list("--include-paths=/b", c("-I", "/b"), "-I/b")) {
    local_flags <- flags
    received_stancflags <- list()
    with_mocked_cli(
      compile_ret = list(status = 0),
      info_ret = list(status = 1),
      code = expect_error(
        cmdstan_model(stan_file),
        "pass the directories with the `include_paths` argument",
        fixed = TRUE
      )
    )
    # The build stops before stanc is called at all.
    expect_length(received_stancflags, 0)
  }
})

test_that("a flag the call emits reaches stanc once when make/local sets it too", {
  model_dir <- withr::local_tempdir()
  stan_file <- file.path(model_dir, "bernoulli.stan")
  file.copy(testing_stan_file("bernoulli"), stan_file)
  local_flags <- NULL
  received_stancflags <- list()
  local_mocked_bindings(
    get_cmdstan_flags = function(flag_name, ...) {
      if (identical(flag_name, "STANCFLAGS")) local_flags else character()
    },
    get_standalone_hpp = function(stan_file, stancflags, ...) {
      received_stancflags <<- append(received_stancflags, list(stancflags))
      ""
    }
  )

  # The next word is another flag, so it survives the drop.
  local_flags <- c("--warn-pedantic", "-fno-soa")
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = cmdstan_model(stan_file, pedantic = TRUE, force_recompile = TRUE)
  )
  expect_length(received_stancflags, 1)
  expect_equal(sum(received_stancflags[[1]] == "--warn-pedantic"), 1L)
  expect_true("-fno-soa" %in% received_stancflags[[1]])

  local_flags <- "--O1"
  received_stancflags <- list()
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = cmdstan_model(stan_file, stanc_options = list("O1"),
                         force_recompile = TRUE)
  )
  expect_length(received_stancflags, 1)
  expect_equal(sum(received_stancflags[[1]] == "--O1"), 1L)

  # The value given as a separate word goes with the flag it belongs to.
  local_flags <- c("--filename-in-msg", "published.stan")
  received_stancflags <- list()
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = cmdstan_model(
      stan_file,
      stanc_options = list("filename-in-msg" = "x.stan"),
      force_recompile = TRUE
    )
  )
  expect_true("--filename-in-msg=x.stan" %in% received_stancflags[[1]])
  expect_false("published.stan" %in% received_stancflags[[1]])
})

test_that("the generated C++ names the source, not the copy stanc compiled", {
  stan_file <- file.path(withr::local_tempdir(), "bernoulli.stan")
  file.copy(testing_stan_file("bernoulli"), stan_file)
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = mod <- cmdstan_model(stan_file, force_recompile = TRUE)
  )
  hpp <- paste(readLines(mod$hpp_file()), collapse = "\n")
  expect_match(hpp, wsl_safe_path(mod$stan_file()), fixed = TRUE)
  expect_no_match(hpp, "model-[0-9a-f]+\\.stan")

  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = mod <- cmdstan_model(
      stan_file,
      stanc_options = list("filename-in-msg" = "published.stan"),
      force_recompile = TRUE
    )
  )
  hpp <- paste(readLines(mod$hpp_file()), collapse = "\n")
  expect_match(hpp, "published.stan", fixed = TRUE)
  expect_no_match(hpp, wsl_safe_path(mod$stan_file()), fixed = TRUE)
})

test_that("named stanc options reach direct calls unquoted", {
  model_dir <- withr::local_tempdir()
  stan_file <- file.path(model_dir, "bernoulli.stan")
  file.copy(testing_stan_file("bernoulli"), stan_file)
  received_stancflags <- list()
  local_mocked_bindings(
    get_cmdstan_flags = function(flag_name, ...) character(),
    get_standalone_hpp = function(stan_file, stancflags, ...) {
      received_stancflags <<- append(received_stancflags, list(stancflags))
      ""
    }
  )

  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = cmdstan_model(
      stan_file,
      stanc_options = list(
        canonicalize = "deprecations",
        "filename-in-msg" = "model filename with spaces.stan"
      )
    )
  )

  expected <- c(
    "--canonicalize=deprecations",
    "--filename-in-msg=model filename with spaces.stan"
  )
  expect_length(received_stancflags, 1)
  expect_equal(
    grep("^--(canonicalize|filename-in-msg)=", received_stancflags[[1]], value = TRUE),
    expected
  )
  expect_equal(
    grep("'", unlist(received_stancflags), fixed = TRUE, value = TRUE),
    character()
  )
})

test_that("a build works with named stanc option values", {
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

  expect_compilation(
    cmdstan_model(
      stan_file,
      stanc_options = list(
        canonicalize = "deprecations",
        "filename-in-msg" = "model filename with spaces.stan"
      )
    )
  )
})

test_that("a build detects stan_opencl without case or partial matching", {
  model_dir <- withr::local_tempdir()
  stan_file <- file.path(model_dir, "bernoulli.stan")
  file.copy(testing_stan_file("bernoulli"), stan_file)
  received_stancflags <- list()
  local_mocked_bindings(
    get_cmdstan_flags = function(flag_name, ...) character(),
    get_standalone_hpp = function(stan_file, stancflags, ...) {
      received_stancflags <<- append(received_stancflags, list(stancflags))
      ""
    }
  )

  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = cmdstan_model(stan_file, cpp_options = list(STAN_OPENCL = TRUE),
                         force_recompile = TRUE)
  )
  expect_length(received_stancflags, 1)
  expect_true("--use-opencl" %in% received_stancflags[[1]])

  received_stancflags <- list()
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = cmdstan_model(stan_file, cpp_options = list(stan_opencl_x = TRUE),
                         force_recompile = TRUE)
  )
  expect_length(received_stancflags, 1)
  expect_false("--use-opencl" %in% received_stancflags[[1]])
})

test_that("a build resolves make/local STANCFLAGS with its cpp_options applied", {
  local_cmdstan_make_local(list(STAN_OPENCL = TRUE))
  model_dir <- withr::local_tempdir()
  stan_file <- file.path(model_dir, "bernoulli.stan")
  file.copy(testing_stan_file("bernoulli"), stan_file)
  received <- list()
  local_mocked_bindings(
    get_standalone_hpp = function(stan_file, stancflags, ...) {
      received <<- append(received, list(stancflags))
      ""
    }
  )
  build <- function(...) {
    with_mocked_cli(
      compile_ret = list(status = 0),
      info_ret = list(status = 1),
      code = cmdstan_model(stan_file, ..., force_recompile = TRUE)
    )
  }

  build(cpp_options = list(stan_opencl = FALSE))
  expect_length(received, 1)
  expect_equal(sum(received[[1]] == "--use-opencl"), 0L)

  received <- list()
  build(cpp_options = list(stan_opencl = TRUE))
  expect_length(received, 1)
  expect_equal(sum(received[[1]] == "--use-opencl"), 1L)

  received <- list()
  build()
  expect_length(received, 1)
  expect_equal(sum(received[[1]] == "--use-opencl"), 1L)
})

test_that("a build resolves make/local STANCFLAGS with its user_header applied", {
  local_cmdstan_make_local(list(
    "ifeq ($(origin USER_HEADER),command line)",
    "STANCFLAGS += --O1",
    "else",
    "STANCFLAGS += --O0",
    "endif"
  ), append = FALSE)
  model_dir <- withr::local_tempdir()
  stan_file <- file.path(model_dir, "bernoulli.stan")
  file.copy(testing_stan_file("bernoulli"), stan_file)
  header <- withr::local_tempfile(fileext = ".hpp")
  writeLines("", header)
  received <- list()
  local_mocked_bindings(
    get_standalone_hpp = function(stan_file, stancflags, ...) {
      received <<- append(received, list(stancflags))
      ""
    }
  )
  build <- function(...) {
    with_mocked_cli(
      compile_ret = list(status = 0),
      info_ret = list(status = 1),
      code = cmdstan_model(stan_file, ..., force_recompile = TRUE)
    )
  }

  build(user_header = header)
  expect_length(received, 1)
  expect_true("--O1" %in% received[[1]])
  expect_false("--O0" %in% received[[1]])

  received <- list()
  build(user_header = NULL)
  expect_length(received, 1)
  expect_true("--O0" %in% received[[1]])
  expect_false("--O1" %in% received[[1]])
})

test_that("a build ignores directory chatter from MAKEFLAGS", {
  withr::local_envvar(MAKEFLAGS = "-w -j 4")
  expect_compilation(
    cmdstan_model(stan_program, quiet = TRUE, force_recompile = TRUE)
  )
})

test_that("a build installs the artifact it just built, not the previous one", {
  model_dir <- withr::local_tempdir()
  stan_file <- file.path(model_dir, "bernoulli.stan")
  file.copy(testing_stan_file("bernoulli"), stan_file)
  exe <- cmdstan_ext(strip_ext(stan_file))

  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = {
      cmdstan_model(stan_file)
      first <- readLines(exe)
      cmdstan_model(stan_file, force_recompile = TRUE)
      second <- readLines(exe)
    }
  )
  expect_false(identical(first, second))
})
