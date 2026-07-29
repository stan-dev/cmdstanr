# A mocked compile produces a real (empty) executable at the destination, so the
# model compiled here must be a temporary copy. Compiling the installed example
# in place would replace the CmdStan installation's own executable.
example_exe <- cmdstan_ext(strip_ext(cmdstan_example_file()))
example_exe_before <- file.info(example_exe)[, c("size", "mtime")]

model_dir <- withr::local_tempdir()
stan_program <- file.path(model_dir, "bernoulli.stan")
file.copy(cmdstan_example_file(), stan_program)
# Keep the program older than the placeholder executables created below, so the
# up-to-date check doesn't force a recompile on timestamp resolution alone.
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
    # The mocked compile now leaves an executable behind, so the constructor
    # queries it for compilation info. Report a failure rather than an empty
    # list, which model_compile_info() cannot interpret.
    info_ret = list(status = 1),
    code = expect_mock_compile({
      mod <- cmdstan_model(stan_file = stan_program, force_recompile = TRUE)
    })
  )
)

test_that("a mocked successful compile installs an executable", {
  stan_file <- file.path(withr::local_tempdir(), "bernoulli.stan")
  file.copy(stan_program, stan_file)
  exe <- cmdstan_ext(strip_ext(stan_file))

  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = mod <- cmdstan_model(stan_file = stan_file, force_recompile = TRUE)
  )

  expect_equal(mod$exe_file(), exe)
  expect_true(file.exists(exe))
})

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

  # The second call finds the executable up to date and compiles nothing, so it
  # must not discard the options the executable was actually built with: an
  # erased stan_threads makes assert_valid_threads() drop 'threads' and run a
  # threaded executable single-threaded. It must not claim the executable is
  # pre-compiled either, or $expose_functions() fails on a model that built
  # itself.
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = expect_no_mock_compile(mod$compile())
  )
  expect_true(mod$cpp_options()$stan_threads)
  expect_false(mod$functions$existing_exe)
  # Standalone functions are rejected outright on WSL, before existing_exe is
  # consulted, so only there can this consequence not be observed.
  if (!os_is_wsl()) {
    expect_warning(mod$expose_functions(), "No standalone functions found")
  }
})

test_that("a no-op compile does not record cpp_options the executable lacks", {
  # A real executable, up to date and built without threading.
  testing_model("bernoulli")

  expect_warning(
    mod <- cmdstan_model(
      testing_stan_file("bernoulli"),
      cpp_options = list(stan_threads = TRUE)
    ),
    "was not built with the requested"
  )

  # Nothing was rebuilt, so the request describes no executable that exists.
  # Recording it anyway left assert_valid_threads() trusting it, and a plain
  # $sample() then failed with "The model executable was built with threading
  # enabled but 'threads_per_chain' was not set!" -- an error that is both
  # false and inescapable without recompiling. (#1019)
  expect_false(isTRUE(mod$cpp_options()$stan_threads))
  expect_no_error(
    mod$sample(
      data = testing_data("bernoulli"),
      chains = 1,
      iter_warmup = 100,
      iter_sampling = 100,
      refresh = 0,
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
  stan_file <- file.path(model_dir, "bernoulli.stan")
  file.copy(stan_program, stan_file)
  Sys.setFileTime(stan_file, Sys.time() - 60)

  mod <- with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = cmdstan_model(stan_file, include_paths = dir_a, force_recompile = TRUE)
  )

  # The same #include directive can resolve to a different file under a
  # different include directory, so an executable built against one does not
  # describe the program the other produces. Without this the object reported
  # the new paths and the new $variables() while still running the old binary,
  # which is the stale-validation failure #1228 is about.
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = expect_mock_compile(mod$compile(include_paths = dir_b))
  )
  expect_equal(mod$include_paths(), resolve_path(dir_b))

  # The recorded paths are now the ones just built against, so a bare call has
  # nothing new to build.
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

  # Build the executable through one object, then let a second, freshly
  # constructed object find it up to date.
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
  # Hydrated from the executable itself: STAN_THREADS is reported, STAN_OPENCL
  # is reported as FALSE (i.e. never set) and STAN_VERSION is not a make option.
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
      "was not built with the requested"
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

  # Same object, same executable, but this call explicitly asks for threading.
  # Nothing was rebuilt, so what is recorded still has to describe the binary
  # on disk; the caller learns their request had no effect from the warning.
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(
      status = 0,
      stdout = "stan_version_major=2\nstan_version_minor=39\nstan_version_patch=0\nSTAN_THREADS=false"
    ),
    code = expect_no_mock_compile(
      expect_warning(
        mod$compile(cpp_options = list(stan_threads = TRUE)),
        "was not built with the requested"
      )
    )
  )
  expect_null(mod$cpp_options()$stan_threads)
})

test_that("no mismatch warning when the executable already has the options", {
  stan_file <- file.path(withr::local_tempdir(), "bernoulli.stan")
  file.copy(stan_program, stan_file)

  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = mod <- cmdstan_model(stan_file, force_recompile = TRUE)
  )

  # The executable reports exactly what is being asked for, so re-stating it
  # must stay quiet -- otherwise the warning fires on ordinary reuse.
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(
      status = 0,
      stdout = "stan_version_major=2\nstan_version_minor=39\nstan_version_patch=0\nSTAN_THREADS=true"
    ),
    code = expect_no_warning(
      mod$compile(cpp_options = list(stan_threads = TRUE))
    )
  )
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

  # The mocked executable is empty, so running it errors rather than returning
  # a non-zero status. Adopting it is best-effort and must still succeed.
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

  # A current executable already sits in the target directory. Adopting it would
  # leave the object describing this program's C++ while running that binary, so
  # the model is rebuilt there instead.
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

# Deliberately the last test in this file: it checks that none of the mocked
# compiles above installed anything over the CmdStan installation's own example
# executable. A git diff would not catch this, since that executable is not part
# of the repository, and a truncating overwrite changes no tracked file at all.
test_that("mocked compiles leave the CmdStan installation untouched", {
  expect_equal(
    file.info(example_exe)[, c("size", "mtime")],
    example_exe_before
  )
})
