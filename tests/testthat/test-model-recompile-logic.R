# Use a temporary copy because mocked compiles install executables.
model_dir <- withr::local_tempdir()
stan_program <- file.path(model_dir, "bernoulli.stan")
file.copy(cmdstan_example_file(), stan_program)
# Keep the source older than executables used by no-op tests.
Sys.setFileTime(stan_program, Sys.time() - 60)

file_that_doesnt_exist <- withr::local_tempfile(pattern = "placeholder_doesnt_exist")
file_that_exists <- withr::local_tempfile(pattern = "placeholder_exists")
file.create(file_that_exists)

skip_message <- "To be fixed in a later version. See #1019."

test_that("warning when no recompile and no info", {
  skip(skip_message)
  with_mocked_cli(
    compile_ret = list(),
    info_ret = list(status = 1),
    code = expect_warning({
      mod <- cmdstan_model(
        stan_file = stan_program,
        exe_file = file_that_exists,
        compile = FALSE
      )
    }, "Recompiling is recommended.")
  )
})

test_that("recompiles when force_recompile flag set",
  with_mocked_cli(
    compile_ret = list(status = 0),
    # Report executable metadata as unavailable.
    info_ret = list(status = 1),
    code = expect_mock_compile({
      mod <- cmdstan_model(stan_file = stan_program, force_recompile = TRUE)
    })
  )
)

test_that("a no-op compile preserves what the previous compilation recorded", {
  stan_file <- file.path(withr::local_tempdir(), "bernoulli.stan")
  file.copy(stan_program, stan_file)

  mod <- cmdstan_model(stan_file, compile = FALSE)
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = mod$compile(
      cpp_options = list(stan_threads = TRUE),
      force_recompile = TRUE
    )
  )
  expect_true(mod$cpp_options()$stan_threads)
  expect_false(mod$functions$existing_exe)

  # A no-op must preserve build options and local-build provenance.
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = expect_no_mock_compile(mod$compile())
  )
  expect_true(mod$cpp_options()$stan_threads)
  expect_false(mod$functions$existing_exe)
})

test_that("a no-op compile does not record cpp_options the executable lacks", {
  # A real executable, up to date and built without threading.
  testing_model("bernoulli")

  expect_warning(
    mod <- cmdstan_model(
      testing_stan_file("bernoulli"),
      cpp_options = list(stan_threads = TRUE)
    ),
    "do not match the ones requested"
  )

  # The unapplied threading request must not affect ordinary sampling (#1019).
  expect_false(isTRUE(mod$cpp_options()$stan_threads))
  expect_no_error(
    mod$sample(
      data = testing_data("bernoulli"),
      chains = 1,
      iter_warmup = 10,
      iter_sampling = 10,
      refresh = 0,
      diagnostics = NULL,
      show_messages = FALSE
    )
  )
})

test_that("changing include_paths forces recompilation", {
  model_dir <- withr::local_tempdir()
  dir_a <- file.path(model_dir, "a")
  dir_b <- file.path(model_dir, "b")
  dir.create(dir_a)
  dir.create(dir_b)
  # One directive, two directories, two different programs.
  writeLines("parameters { real alpha; }", file.path(dir_a, "params.stan"))
  writeLines("parameters { real beta; }", file.path(dir_b, "params.stan"))
  stan_file <- file.path(model_dir, "included.stan")
  writeLines(c("#include params.stan", "model { target += 0; }"), stan_file)
  Sys.setFileTime(stan_file, Sys.time() - 60)

  mod <- with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = cmdstan_model(stan_file, include_paths = dir_a, force_recompile = TRUE)
  )
  expect_equal(names(mod$variables()$parameters), "alpha")

  # The same directive resolves to a different program in dir_b (#1228).
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = expect_mock_compile(mod$compile(include_paths = dir_b))
  )
  expect_equal(mod$include_paths(), resolve_path(dir_b))
  expect_equal(names(mod$variables()$parameters), "beta")

  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = expect_no_mock_compile(mod$compile())
  )

  # Same directory, different spelling: not a change.
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = expect_no_mock_compile(
      mod$compile(include_paths = file.path(dir_b, "."))
    )
  )
})

test_that("a no-op compile adopts an executable the object did not build", {
  stan_file <- file.path(withr::local_tempdir(), "bernoulli.stan")
  file.copy(stan_program, stan_file)
  exe <- cmdstan_ext(strip_ext(stan_file))

  # Build with one object, then adopt the executable with another.
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = cmdstan_model(stan_file, force_recompile = TRUE)
  )
  mod <- cmdstan_model(stan_file, compile = FALSE)
  expect_length(mod$exe_file(), 0)

  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(
      status = 0,
      stdout = "stan_version_major=2\nstan_version_minor=39\nstan_version_patch=0\nSTAN_THREADS=true\nSTAN_OPENCL=false"
    ),
    code = expect_no_mock_compile(mod$compile())
  )

  expect_equal(mod$exe_file(), exe)
  expect_true(mod$functions$existing_exe)
  # Record enabled flags only and omit STAN_VERSION (not a make option).
  expect_true(mod$cpp_options()$STAN_THREADS)
  expect_null(mod$cpp_options()$STAN_OPENCL)
  expect_null(mod$cpp_options()$STAN_VERSION)
})

test_that("adopting an executable describes the binary, not the request", {
  stan_file <- file.path(withr::local_tempdir(), "bernoulli.stan")
  file.copy(stan_program, stan_file)

  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = cmdstan_model(stan_file, force_recompile = TRUE)
  )

  # The executable is up to date but was not built with threading. Until
  # cmdstanr rebuilds on a cpp_options mismatch (#1019), the request describes
  # an executable that does not exist, so it is reported as a warning rather
  # than recorded as fact.
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(
      status = 0,
      stdout = "stan_version_major=2\nstan_version_minor=39\nstan_version_patch=0\nSTAN_THREADS=false"
    ),
    code = expect_warning(
      mod <- cmdstan_model(stan_file, cpp_options = list(stan_threads = TRUE)),
      "do not match the ones requested"
    )
  )

  expect_null(mod$cpp_options()$stan_threads)
  expect_true(mod$functions$existing_exe)
})

test_that("a no-op compile does not adopt options the executable lacks", {
  stan_file <- file.path(withr::local_tempdir(), "bernoulli.stan")
  file.copy(stan_program, stan_file)

  mod <- with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = cmdstan_model(stan_file)
  )
  expect_null(mod$cpp_options()$stan_threads)

  # A no-op warns without changing the options recorded for the executable.
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(
      status = 0,
      stdout = "stan_version_major=2\nstan_version_minor=39\nstan_version_patch=0\nSTAN_THREADS=false"
    ),
    code = expect_no_mock_compile(
      expect_warning(
        mod$compile(cpp_options = list(stan_threads = TRUE)),
        "do not match the ones requested"
      )
    )
  )
  expect_null(mod$cpp_options()$stan_threads)
})

test_that("a no-op compile warns about options the executable cannot report", {
  stan_file <- file.path(withr::local_tempdir(), "bernoulli.stan")
  file.copy(stan_program, stan_file)

  mod <- with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = cmdstan_model(stan_file, force_recompile = TRUE)
  )

  # The executable does not report STAN_CPP_OPTIMS or arbitrary make variables.
  # Because this object built it, the recorded options can still detect mismatches.
  info <- paste0(
    "stan_version_major=2\nstan_version_minor=39\nstan_version_patch=0\n",
    "STAN_THREADS=false"
  )
  for (requested in list(
    list(stan_cpp_optims = TRUE),
    list(my_custom_make_flag = "1")
  )) {
    with_mocked_cli(
      compile_ret = list(status = 0),
      info_ret = list(status = 0, stdout = info),
      code = expect_no_mock_compile(
        expect_warning(
          mod$compile(cpp_options = requested),
          "do not match the ones requested"
        )
      )
    )
    expect_null(cpp_option_value(mod$cpp_options(), names(requested)))
  }
})

test_that("a no-op compile stays quiet about options it was built with", {
  stan_file <- file.path(withr::local_tempdir(), "bernoulli.stan")
  file.copy(stan_program, stan_file)

  mod <- with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = cmdstan_model(
      stan_file,
      cpp_options = list(stan_cpp_optims = TRUE, my_custom_make_flag = "1"),
      force_recompile = TRUE
    )
  )

  # Same unreportable options, but this executable really was built with them,
  # so re-supplying them is ordinary reuse and must not warn.
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = expect_no_mock_compile(
      expect_no_warning(
        mod$compile(
          cpp_options = list(stan_cpp_optims = TRUE, my_custom_make_flag = "1")
        )
      )
    )
  )
})

test_that("option comparison ignores spelling but not an empty assignment", {
  stan_file <- file.path(withr::local_tempdir(), "bernoulli.stan")
  file.copy(stan_program, stan_file)

  mod <- with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = cmdstan_model(
      stan_file,
      cpp_options = list(STAN_CPP_OPTIMS = TRUE),
      force_recompile = TRUE
    )
  )

  quietly <- function(requested) {
    with_mocked_cli(
      compile_ret = list(status = 0),
      info_ret = list(status = 1),
      code = expect_no_mock_compile(
        expect_no_warning(mod$compile(cpp_options = requested))
      )
    )
  }
  # Same option, other spelling, and the string a makefile would carry.
  quietly(list(stan_cpp_optims = TRUE))
  quietly(list(stan_cpp_optims = "TRUE"))

  # NULL is not omission either: it reaches make as an empty STAN_THREADS=,
  # which overrides whatever make/local sets rather than leaving it alone.
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = expect_no_mock_compile(
      expect_warning(
        mod$compile(cpp_options = list(stan_cpp_optims = TRUE, stan_threads = NULL)),
        "do not match the ones requested"
      )
    )
  )

  # Dropping a recorded option is still a change: cpp_options are one-shot, so
  # recompiling with this list would build without STAN_CPP_OPTIMS.
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = expect_no_mock_compile(
      expect_warning(
        mod$compile(cpp_options = list(stan_threads = TRUE)),
        "do not match the ones requested"
      )
    )
  )
})

test_that("option comparison follows what make is actually given", {
  stan_file <- file.path(withr::local_tempdir(), "bernoulli.stan")
  file.copy(stan_program, stan_file)

  mod <- with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = cmdstan_model(
      stan_file,
      cpp_options = list(stan_cpp_optims = TRUE),
      force_recompile = TRUE
    )
  )
  no_op <- function(requested, expectation) {
    with_mocked_cli(
      compile_ret = list(status = 0),
      info_ret = list(status = 1),
      code = expect_no_mock_compile(expectation(mod$compile(cpp_options = requested)))
    )
  }
  warns <- function(requested) {
    no_op(requested, function(code) {
      expect_warning(code, "do not match the ones requested")
    })
  }
  quietly <- function(requested) no_op(requested, expect_no_warning)

  # FALSE is not omission. It reaches make as STAN_CPP_OPTIMS=FALSE, and CmdStan
  # enables some options whenever their make variable is non-empty, so asking
  # for it would build a different executable than the recorded TRUE did.
  warns(list(stan_cpp_optims = FALSE))
  warns(list(stan_cpp_optims = TRUE, stan_threads = FALSE))

  # Every duplicate reaches make, and a makefile takes the last.
  quietly(list(stan_cpp_optims = FALSE, stan_cpp_optims = TRUE))
  warns(list(stan_cpp_optims = TRUE, stan_cpp_optims = FALSE))

  # An unnamed entry is a raw make argument rather than something to skip.
  warns(list("STAN_THREADS=TRUE"))

  # Order survives normalization: these reach make as the same two assignments
  # in opposite orders, so exactly one of them matches the recorded TRUE.
  quietly(list("STAN_CPP_OPTIMS=FALSE", "STAN_CPP_OPTIMS=TRUE"))
  warns(list("STAN_CPP_OPTIMS=TRUE", "STAN_CPP_OPTIMS=FALSE"))

  # The same, across the boundary between a named entry and a raw one.
  quietly(structure(
    list(FALSE, "STAN_CPP_OPTIMS=TRUE"),
    names = c("stan_cpp_optims", "")
  ))
  warns(structure(
    list("STAN_CPP_OPTIMS=TRUE", FALSE),
    names = c("", "stan_cpp_optims")
  ))

  # A vector value expands into one assignment per element, so it is the last
  # element that decides, not the vector as a whole.
  quietly(list(stan_cpp_optims = c(FALSE, TRUE)))
  warns(list(stan_cpp_optims = c(TRUE, FALSE)))
})

test_that("a raw make argument round-trips through the option comparison", {
  stan_file <- file.path(withr::local_tempdir(), "bernoulli.stan")
  file.copy(stan_program, stan_file)

  mod <- with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = cmdstan_model(
      stan_file,
      cpp_options = list("STAN_CPP_OPTIMS=TRUE"),
      force_recompile = TRUE
    )
  )

  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = expect_no_mock_compile(
      expect_no_warning(mod$compile(cpp_options = list("STAN_CPP_OPTIMS=TRUE")))
    )
  )
})

test_that("a successful compile records options only the executable reports", {
  stan_file <- file.path(withr::local_tempdir(), "bernoulli.stan")
  file.copy(stan_program, stan_file)
  # Stands in for STAN_THREADS=true in make/local: nothing was passed to
  # $compile(), but the binary reports threading.
  threaded <- paste0(
    "stan_version_major=2\nstan_version_minor=39\nstan_version_patch=0\n",
    "STAN_THREADS=true"
  )

  mod <- cmdstan_model(stan_file, compile = FALSE)
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 0, stdout = threaded),
    code = mod$compile(force_recompile = TRUE)
  )
  expect_true(cpp_option_value(mod$cpp_options(), "stan_threads"))
  expect_silent(assert_valid_threads(2, mod$cpp_options(), multiple_chains = TRUE))

  # What was passed to Make keeps only the request, so a later no-op can tell
  # inherited options from explicit ones.
  built <- mod$.__enclos_env__$private$built_cpp_options_
  expect_null(cpp_option_value(built, "stan_threads"))

  # Unreadable metadata leaves the request in place rather than erroring.
  mod_blind <- cmdstan_model(stan_file, compile = FALSE)
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = mod_blind$compile(
      cpp_options = list(stan_cpp_optims = TRUE),
      force_recompile = TRUE
    )
  )
  expect_true(cpp_option_value(mod_blind$cpp_options(), "stan_cpp_optims"))
  expect_null(cpp_option_value(mod_blind$cpp_options(), "stan_threads"))
})

test_that("cmdstan_model() reads the executable metadata once, whatever the path", {
  stan_file <- file.path(withr::local_tempdir(), "bernoulli.stan")
  file.copy(stan_program, stan_file)
  info <- "stan_version_major=2\nstan_version_minor=39\nstan_version_patch=0"
  real_model_compile_info <- model_compile_info
  reads <- 0L
  local_mocked_bindings(
    model_compile_info = function(...) {
      reads <<- reads + 1L
      real_model_compile_info(...)
    }
  )

  # Fresh compile.
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 0, stdout = info),
    code = mod <- cmdstan_model(stan_file)
  )
  expect_equal(reads, 1L)

  # Executable already up to date.
  reads <- 0L
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 0, stdout = info),
    code = cmdstan_model(stan_file)
  )
  expect_equal(reads, 1L)

  # Executable adopted without a Stan file.
  reads <- 0L
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 0, stdout = info),
    code = cmdstan_model(exe_file = mod$exe_file())
  )
  expect_equal(reads, 1L)
})

test_that("options inherited from make/local are learned, not warned about", {
  stan_file <- file.path(withr::local_tempdir(), "bernoulli.stan")
  file.copy(stan_program, stan_file)
  # make/local supplies STAN_THREADS=true, so the executable is threaded even
  # though nothing was passed to $compile().
  threaded <- paste0(
    "stan_version_major=2\nstan_version_minor=39\nstan_version_patch=0\n",
    "STAN_THREADS=true"
  )

  mod <- cmdstan_model(stan_file, compile = FALSE)
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 0, stdout = threaded),
    code = mod$compile(force_recompile = TRUE)
  )
  expect_true(cpp_option_value(mod$cpp_options(), "stan_threads"))

  # A no-op keeps them recorded, and asking for what the executable already has
  # is not a mismatch.
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 0, stdout = threaded),
    code = expect_no_mock_compile(
      expect_no_warning(mod$compile(cpp_options = list(stan_threads = TRUE)))
    )
  )
  expect_true(cpp_option_value(mod$cpp_options(), "stan_threads"))

  # An option only the record knows about still combines with one only the
  # metadata knows about.
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 0, stdout = threaded),
    code = mod$compile(
      cpp_options = list(stan_cpp_optims = TRUE),
      force_recompile = TRUE
    )
  )
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 0, stdout = threaded),
    code = expect_no_mock_compile(
      expect_no_warning(
        mod$compile(cpp_options = list(stan_cpp_optims = TRUE, stan_threads = TRUE))
      )
    )
  )
  # Changing the unreported option still warns.
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 0, stdout = threaded),
    code = expect_no_mock_compile(
      expect_warning(
        mod$compile(cpp_options = list(stan_cpp_optims = FALSE, stan_threads = TRUE)),
        "do not match the ones requested"
      )
    )
  )

  # Without metadata, compare the options recorded during compilation.
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = expect_no_mock_compile(
      expect_no_warning(mod$compile(cpp_options = list(stan_cpp_optims = TRUE)))
    )
  )
})

test_that("an explicitly passed raw assignment is not taken for make/local", {
  stan_file <- file.path(withr::local_tempdir(), "bernoulli.stan")
  file.copy(stan_program, stan_file)
  threaded <- paste0(
    "stan_version_major=2\nstan_version_minor=39\nstan_version_patch=0\n",
    "STAN_THREADS=true"
  )

  mod <- cmdstan_model(stan_file, compile = FALSE)
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 0, stdout = threaded),
    code = mod$compile(
      cpp_options = structure(
        list("STAN_THREADS=TRUE", TRUE),
        names = c("", "stan_cpp_optims")
      ),
      force_recompile = TRUE
    )
  )

  # Raw STAN_THREADS=TRUE is explicit, not inherited from make/local.
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 0, stdout = threaded),
    code = expect_no_mock_compile(
      expect_warning(
        mod$compile(cpp_options = list(stan_cpp_optims = TRUE)),
        "do not match the ones requested"
      )
    )
  )
})

test_that("an executable built with an explicit NULL accepts NULL again", {
  stan_file <- file.path(withr::local_tempdir(), "bernoulli.stan")
  file.copy(stan_program, stan_file)

  # Reported FALSE leaves the explicit NULL assignment intact.
  disabled <- paste0(
    "stan_version_major=2\nstan_version_minor=39\nstan_version_patch=0\n",
    "STAN_THREADS=false"
  )

  mod <- cmdstan_model(stan_file, compile = FALSE)
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 0, stdout = disabled),
    code = mod$compile(
      cpp_options = list(stan_threads = NULL),
      force_recompile = TRUE
    )
  )

  # An empty STAN_THREADS= is what was built with, so re-stating it matches.
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 0, stdout = disabled),
    code = expect_no_mock_compile(
      expect_no_warning(mod$compile(cpp_options = list(stan_threads = NULL)))
    )
  )

  # Omission is a different request: it would leave make/local in force rather
  # than overriding it, so it does not match a build that overrode it.
  mod_omitted <- cmdstan_model(stan_file, compile = FALSE)
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = mod_omitted$compile(force_recompile = TRUE)
  )
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = expect_no_mock_compile(
      expect_warning(
        mod_omitted$compile(cpp_options = list(stan_threads = NULL)),
        "do not match the ones requested"
      )
    )
  )
})

test_that("an adopted executable stays silent about options it cannot report", {
  stan_file <- file.path(withr::local_tempdir(), "bernoulli.stan")
  file.copy(stan_program, stan_file)

  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = cmdstan_model(stan_file, force_recompile = TRUE)
  )

  # An adopted executable cannot verify unreported options, so it neither warns
  # nor records the request (#1238).
  info <- paste0(
    "stan_version_major=2\nstan_version_minor=39\nstan_version_patch=0\n",
    "STAN_THREADS=false"
  )
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 0, stdout = info),
    code = expect_no_mock_compile(
      expect_no_warning(
        mod <- cmdstan_model(
          stan_file,
          cpp_options = list(stan_cpp_optims = TRUE)
        )
      )
    )
  )
  expect_null(mod$cpp_options()$stan_cpp_optims)
})

test_that("no mismatch warning when the executable already has the options", {
  stan_file <- file.path(withr::local_tempdir(), "bernoulli.stan")
  file.copy(stan_program, stan_file)

  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = cmdstan_model(
      stan_file,
      cpp_options = list(stan_threads = TRUE),
      force_recompile = TRUE
    )
  )

  # The adopted executable reports the requested threading option, so ordinary
  # reuse must not warn.
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(
      status = 0,
      stdout = "stan_version_major=2\nstan_version_minor=39\nstan_version_patch=0\nSTAN_THREADS=true"
    ),
    code = expect_no_warning(
      mod <- cmdstan_model(stan_file, cpp_options = list(stan_threads = TRUE))
    )
  )
  # cpp_option_value() handles the metadata's uppercase spelling.
  expect_true(cpp_option_value(mod$cpp_options(), "stan_threads"))
})

test_that("a no-op compile tolerates an executable it cannot query", {
  stan_file <- file.path(withr::local_tempdir(), "bernoulli.stan")
  file.copy(stan_program, stan_file)

  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = cmdstan_model(stan_file, force_recompile = TRUE)
  )
  mod <- cmdstan_model(stan_file, compile = FALSE)

  # The mocked executable cannot answer info queries, but adoption is best-effort.
  expect_no_error(mod$compile())
  expect_true(mod$functions$existing_exe)
})

test_that("compiling into a directory with a different executable recompiles", {
  stan_file <- file.path(withr::local_tempdir(), "bernoulli.stan")
  file.copy(stan_program, stan_file)

  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = mod <- cmdstan_model(stan_file)
  )

  # Do not adopt an unrelated executable from a new directory.
  other_dir <- withr::local_tempdir()
  other_exe <- cmdstan_ext(file.path(other_dir, "bernoulli"))
  file.create(other_exe)

  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = expect_mock_compile(mod$compile(dir = other_dir))
  )
  expect_equal(mod$exe_file(), other_exe)
})

test_that("a mocked failed compile installs no executable", {
  stan_file <- file.path(withr::local_tempdir(), "bernoulli.stan")
  file.copy(stan_program, stan_file)
  exe <- cmdstan_ext(strip_ext(stan_file))

  with_mocked_cli(
    compile_ret = list(status = 1),
    info_ret = list(status = 1),
    code = expect_error(
      cmdstan_model(stan_file = stan_file, force_recompile = TRUE),
      "An error occurred during compilation"
    )
  )

  expect_false(file.exists(exe))
})

test_that("no mismatch results in no recompile", with_mocked_cli(
  compile_ret = list(status = 0),
  info_ret = list(
    status = 0,
    stdout = "
      stan_version_major = 2
      stan_version_minor = 35
      stan_version_patch = 0
      STAN_THREADS=false
      STAN_MPI=false
      STAN_OPENCL=false
      STAN_NO_RANGE_CHECKS=false
      STAN_CPP_OPTIMS=false
    "
  ),
  code = expect_no_mock_compile({
    mod <- cmdstan_model(stan_file = stan_program, exe_file = file_that_exists)
  })
))

test_that("mismatch results in recompile.", {
  skip(skip_message)
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(
      status = 0,
      stdout =  "
        stan_version_major = 2
        stan_version_minor = 35
        stan_version_patch = 0
        STAN_THREADS=false
        STAN_MPI=false
        STAN_OPENCL=false
        STAN_NO_RANGE_CHECKS=false
        STAN_CPP_OPTIMS=false
      "
    ),
    code = expect_mock_compile({
      mod <- cmdstan_model(
        stan_file = stan_program,
        exe_file = file_that_exists,
        cpp_options = list(stan_threads = TRUE)
      )
    })
  )
})

test_that("recompile when cpp args don't match binary", {
  skip(skip_message)
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(
      status = 0,
      stdout = "
        stan_version_major = 2
        stan_version_minor = 38
        stan_version_patch = 0
        STAN_THREADS=false
        STAN_MPI=false
        STAN_OPENCL=true
        STAN_NO_RANGE_CHECKS=false
        STAN_CPP_OPTIMS=false
      "
    ),
    expect_mock_compile({
      mod_gq <- cmdstan_model(
        testing_stan_file("bernoulli_ppc"),
        exe_file = file_that_exists,
        cpp_options = list(stan_threads = TRUE)
      )
    })
  )
})
