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
    "do not match the ones requested"
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

  # The same #include directive resolves to a different file under the other
  # directory, so an executable built against one does not describe the program
  # the other produces. Without this the object reported the new paths and the
  # new $variables() while still running the old binary, which is the
  # stale-validation failure #1228 is about.
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = expect_mock_compile(mod$compile(include_paths = dir_b))
  )
  expect_equal(mod$include_paths(), resolve_path(dir_b))
  expect_equal(names(mod$variables()$parameters), "beta")

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

  # CmdStan 2.39 reports no STAN_CPP_OPTIMS, and reports nothing at all about
  # arbitrary make variables, so the binary's own metadata can neither confirm
  # nor deny either request. This object compiled this executable, though, so
  # what it was built with is known exactly and both requests plainly disagree
  # with it. Detecting these through the metadata alone silently ignored them.
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

  # Re-supplying exactly what the executable was built with is ordinary reuse,
  # even when the option never had a name to compare by.
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = expect_no_mock_compile(
      expect_no_warning(mod$compile(cpp_options = list("STAN_CPP_OPTIMS=TRUE")))
    )
  )
})

test_that("options inherited from make/local are learned, not warned about", {
  stan_file <- file.path(withr::local_tempdir(), "bernoulli.stan")
  file.copy(stan_program, stan_file)
  # make/local supplies STAN_THREADS=true, so the executable is threaded even
  # though nothing was passed to $compile() and nothing could be recorded.
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
  expect_null(mod$cpp_options()$stan_threads)

  # Comparing against the record alone reported a mismatch for a binary that
  # does have threading. The binary's own account fills the gap, and is kept:
  # suppressing the warning without recording what it revealed would leave
  # assert_valid_threads() still dropping 'threads_per_chain'.
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
  # ...and changing the unreportable one is still caught.
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

  # With no metadata to be had, the record is all there is and still answers
  # for the option it holds.
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

  # The binary reports threading and this call did not name stan_threads, but it
  # did pass STAN_THREADS=TRUE as a raw assignment, so the flag is not inherited
  # from make/local and omitting it would drop it. Reading names() rather than
  # what make was given missed that and stayed quiet.
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

  # Metadata reporting threading off, rather than no metadata at all, so the
  # merge is exercised: a reported FALSE is skipped, leaving the explicit NULL
  # to stand as the empty assignment it is.
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

  # A second object adopts that executable and holds no generated C++ for it,
  # so the binary's own metadata is the only description available and it
  # reports nothing about STAN_CPP_OPTIMS. Unverifiable is not a mismatch, so
  # this neither warns nor records the request. (#1238)
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

  # Adopted by a second object, so the binary's metadata is the only account of
  # it available -- an object that compiled the executable is answered from what
  # it recorded instead. The metadata reports exactly what is being asked for,
  # so re-stating it must stay quiet, otherwise the warning fires on ordinary
  # reuse.
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
  # Hydrated from metadata, so it carries the metadata's spelling; the accessor
  # the fitting methods use is case-insensitive.
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
