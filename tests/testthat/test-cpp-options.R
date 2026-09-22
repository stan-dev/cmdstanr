set_cmdstan_path()

test_that("assert_valid_cpp_options passes options through", {
  expect_equal(assert_valid_cpp_options(NULL), list())
  expect_identical(
    assert_valid_cpp_options(list(stan_threads = TRUE, Stan_OpenCL = TRUE)),
    list(STAN_THREADS = TRUE, STAN_OPENCL = TRUE)
  )
  expect_identical(
    assert_valid_cpp_options(list(STAN_OPENCL = NULL)),
    list(STAN_OPENCL = NULL)
  )
})

test_that("assert_valid_cpp_options rejects a user header", {
  for (option_name in c("USER_HEADER", "user_header", "User_Header")) {
    expect_error(
      assert_valid_cpp_options(setNames(list("header.hpp"), option_name)),
      paste0(
        "The user header cannot be set through `cpp_options`. ",
        "Pass it with the `user_header` argument: `user_header = \"header.hpp\"`."
      ),
      fixed = TRUE
    )
    expect_error(
      assert_valid_cpp_options(setNames(list(c("a.hpp", "b.hpp")), option_name)),
      paste0(
        "The user header cannot be set through `cpp_options`. ",
        "Pass it with the `user_header` argument."
      ),
      fixed = TRUE
    )
  }
})

test_that("assert_valid_cpp_options points an empty user header at NULL", {
  expected_null <- paste0(
    "The user header cannot be set through `cpp_options`. ",
    "Pass it with the `user_header` argument: `user_header = NULL`."
  )
  expect_error(
    assert_valid_cpp_options(list("USER_HEADER=")),
    expected_null,
    fixed = TRUE
  )
  expect_error(
    assert_valid_cpp_options(list(USER_HEADER = FALSE)),
    expected_null,
    fixed = TRUE
  )
  expect_error(
    assert_valid_cpp_options(list(USER_HEADER = NULL)),
    expected_null,
    fixed = TRUE
  )
  expect_error(
    assert_valid_cpp_options(list("USER_HEADER=C:\\h\\f.hpp")),
    paste0(
      "The user header cannot be set through `cpp_options`. ",
      "Pass it with the `user_header` argument: `user_header = \"C:\\\\h\\\\f.hpp\"`."
    ),
    fixed = TRUE
  )
})

test_that("assert_valid_cpp_options wants a literal TBB directory", {
  expect_error(
    assert_valid_cpp_options(list(tbb_bin = "$(MATH)lib/tbb")),
    paste(
      "`TBB_BIN` must be a literal directory. cmdstanr records it to launch",
      "the model with the right TBB and doesn't expand make expressions",
      "like `$(MATH)lib/tbb`."
    ),
    fixed = TRUE
  )
  expect_error(
    assert_valid_cpp_options(list(TBB_LIB = "${HOME}/tbb")),
    "`TBB_LIB` must be a literal directory.",
    fixed = TRUE
  )
  expect_identical(
    assert_valid_cpp_options(list(TBB_LIB = "/opt/tbb", TBB_BIN = FALSE)),
    list(TBB_LIB = "/opt/tbb", TBB_BIN = FALSE)
  )
})

test_that("assert_valid_cpp_options rejects an unnamed assignment", {
  expect_error(
    assert_valid_cpp_options(list("FOO=1")),
    "Write `list(FOO = \"1\")` instead of `\"FOO=1\"`.",
    fixed = TRUE
  )
  expect_error(
    assert_valid_cpp_options(list("foo=1")),
    "Write `list(FOO = \"1\")` instead of `\"foo=1\"`.",
    fixed = TRUE
  )
  expect_error(
    assert_valid_cpp_options(list("STAN_THREADS=TRUE")),
    "Write `list(STAN_THREADS = \"TRUE\")` instead of `\"STAN_THREADS=TRUE\"`.",
    fixed = TRUE
  )
})

test_that("assert_valid_cpp_options sends an unnamed entry to the right argument", {
  for (entry in c("USER_HEADER=h", "user_header=h")) {
    expect_error(
      assert_valid_cpp_options(list(entry)),
      "Pass it with the `user_header` argument: `user_header = \"h\"`.",
      fixed = TRUE
    )
  }
  for (entry in c("STANCFLAGS=--O1", "STANCFLAGS += --O1")) {
    expect_error(
      assert_valid_cpp_options(list(entry)),
      "Pass stanc flags with the `stanc_options` argument.",
      fixed = TRUE
    )
  }
})

test_that("assert_valid_cpp_options sends makefile syntax to make/local", {
  for (entry in c("CXXFLAGS += -x", "CXXFLAGS+=-x")) {
    expect_error(
      assert_valid_cpp_options(list(entry)),
      paste0(
        "`\"", entry, "\"` is makefile syntax and cannot be passed through ",
        "`cpp_options`. To set it in `make/local` use ",
        "`cmdstan_make_local(cpp_options = list(\"", entry, "\"))`."
      ),
      fixed = TRUE
    )
  }
  for (entry in c("-j4", "--eval=STAN_OPENCL=1")) {
    expect_error(
      assert_valid_cpp_options(list(entry)),
      paste0(
        "Make flags cannot be passed through `cpp_options`. Set them in ",
        "`make/local` with `cmdstan_make_local()`, for example ",
        "`MAKEFLAGS += -j4`."
      ),
      fixed = TRUE
    )
  }
  expect_error(
    assert_valid_cpp_options(list(1)),
    "`cpp_options` entries must be named: `list(NAME = value)`.",
    fixed = TRUE
  )
})

test_that("assert_valid_cpp_options rejects an unnamed assignment quoted as an R literal", {
  expect_error(
    assert_valid_cpp_options(list('CXXFLAGS=-DVERSION="foo"')),
    '`cpp_options` entries must be named. Write `list(CXXFLAGS = "-DVERSION=\\"foo\\"")` instead of `"CXXFLAGS=-DVERSION=\\"foo\\""`.',
    fixed = TRUE
  )
})

test_that("assert_valid_cpp_options rejects -B and --always-make", {
  for (entry in c("-B", "--always-make")) {
    expect_error(
      assert_valid_cpp_options(list(entry)),
      paste0(
        "Make flags cannot be passed through `cpp_options`. `", entry,
        "` rebuilds everything; pass `force_recompile = TRUE` instead."
      ),
      fixed = TRUE
    )
  }
})

test_that("assert_valid_cpp_options sends -f entries to make/local's include", {
  for (entry in c("-f other.mk", "-fother.mk", "--file=other.mk", "--makefile=other.mk")) {
    expect_error(
      assert_valid_cpp_options(list(entry)),
      paste0(
        "Make flags cannot be passed through `cpp_options`. To read another ",
        "makefile add `include other.mk` to `make/local`, for example ",
        "`cmdstan_make_local(cpp_options = list(\"include other.mk\"))`."
      ),
      fixed = TRUE
    )
  }
  expect_error(
    assert_valid_cpp_options(list("-f")),
    paste0(
      "Make flags cannot be passed through `cpp_options`. Set them in ",
      "`make/local` with `cmdstan_make_local()`, for example ",
      "`MAKEFLAGS += -j4`."
    ),
    fixed = TRUE
  )
})

test_that("assert_valid_cpp_options requires Make variable names", {
  for (options in list(list("CXXFLAGS+" = "-x"), list("2FOO" = 1))) {
    expect_error(
      assert_valid_cpp_options(options),
      paste0(
        "`cpp_options` names must be Make variable names, made of letters, ",
        "digits and underscores and not starting with a digit. `",
        names(options), "` is not one."
      ),
      fixed = TRUE
    )
  }
})

test_that("assert_valid_cpp_options rejects a named STANCFLAGS", {
  for (options in list(list(STANCFLAGS = "--O1"), list(stancflags = "--O1"))) {
    expect_error(
      assert_valid_cpp_options(options),
      paste0(
        "`STANCFLAGS` cannot be set through `cpp_options`. ",
        "Pass stanc flags with the `stanc_options` argument."
      ),
      fixed = TRUE
    )
  }
})

test_that("cpp option lookup is exact and case-insensitive", {
  cpp_options <- list(STAN_THREADS = TRUE)
  expect_identical(cpp_option_value(cpp_options, "stan_threads"), TRUE)
  expect_named(cpp_options, "STAN_THREADS")

  expect_identical(
    cpp_option_value(
      list(stan_threads = FALSE, STAN_THREADS = TRUE),
      "stan_threads"
    ),
    TRUE
  )
  expect_identical(
    cpp_option_value(
      list(STAN_THREADS = TRUE, stan_threads = FALSE),
      "stan_threads"
    ),
    FALSE
  )
  expect_null(
    cpp_option_value(
      list(STAN_OPENCL = TRUE, stan_opencl = NULL),
      "stan_opencl"
    )
  )
  expect_null(cpp_option_value(list(stan_opencl_x = TRUE), "stan_opencl"))
})

test_that("a thread request needs threading reported on", {
  on <- list(stan_threads = TRUE)
  off <- list(stan_threads = FALSE)
  unknown <- list()

  expect_identical(assert_valid_threads(2L, on), 2L)
  expect_error(
    assert_valid_threads(2L, off),
    "does not report threading as enabled", fixed = TRUE
  )
  expect_error(
    assert_valid_threads(2L, unknown),
    "does not report threading as enabled", fixed = TRUE
  )
  expect_error(
    assert_valid_threads(2L, off, multiple_chains = TRUE),
    "'threads_per_chain'", fixed = TRUE
  )

  for (features in list(on, off, unknown)) {
    expect_identical(assert_valid_threads(1L, features), 1L)
    expect_null(assert_valid_threads(NULL, features))
  }
})

test_that("an OpenCL device request needs OpenCL reported on", {
  expect_identical(
    assert_valid_opencl(c(0L, 0L), list(stan_opencl = TRUE)),
    c(0L, 0L)
  )
  expect_error(
    assert_valid_opencl(c(0L, 0L), list(stan_opencl = FALSE)),
    "does not report OpenCL as enabled", fixed = TRUE
  )
  expect_error(
    assert_valid_opencl(c(0L, 0L), list()),
    "does not report OpenCL as enabled", fixed = TRUE
  )
  expect_null(assert_valid_opencl(NULL, list()))
})

test_that("feature lookups do not use partial matching", {
  expect_error(
    assert_valid_opencl(c(0L, 0L), list(stan_opencl_x = TRUE)),
    "does not report OpenCL as enabled", fixed = TRUE
  )
  expect_error(
    assert_valid_threads(2L, list(stan_threads_x = TRUE)),
    "does not report threading as enabled", fixed = TRUE
  )
})

stan_program <- testing_stan_file("bernoulli")
data_file <- test_path("resources", "data", "bernoulli.data.json")
features <- function(mod) mod$.__enclos_env__$private$reported_features_

test_that("a feature inherited from make/local is reported, not requested", {
  stan_file <- file.path(withr::local_tempdir(), "bernoulli.stan")
  file.copy(stan_program, stan_file)
  threaded <- paste0(
    "stan_version_major=2\nstan_version_minor=39\nstan_version_patch=0\n",
    "STAN_THREADS=true"
  )
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 0, stdout = threaded),
    code = {
      mod <- cmdstan_model(stan_file, force_recompile = TRUE)
      expect_true(features(mod)$stan_threads)
      expect_identical(
        assert_valid_threads(4, features(mod), multiple_chains = TRUE), 4
      )

      mod2 <- cmdstan_model(stan_file)
      expect_true(features(mod2)$stan_threads)
      expect_identical(
        assert_valid_threads(4, features(mod2), multiple_chains = TRUE), 4
      )
    }
  )
})

test_that("an executable reporting no threading flag is unknown, not off", {
  stan_file <- file.path(withr::local_tempdir(), "bernoulli.stan")
  file.copy(stan_program, stan_file)
  info <- "stan_version_major=2\nstan_version_minor=39\nstan_version_patch=0"
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 0, stdout = info),
    code = {
      mod <- cmdstan_model(stan_file, force_recompile = TRUE)
      adopted <- cmdstan_model(exe_file = mod$exe_file())
    }
  )
  expect_null(features(adopted)$stan_threads)
  expect_error(
    adopted$sample(data = data_file, threads_per_chain = 2),
    "does not report threading as enabled", fixed = TRUE
  )
})

test_that("an executable adopted from live info reports threading as unknown", {
  stan_file <- file.path(withr::local_tempdir(), "bernoulli.stan")
  file.copy(stan_program, stan_file)
  info <- "stan_version_major=2\nstan_version_minor=39\nstan_version_patch=0"
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 0, stdout = info),
    code = {
      mod <- cmdstan_model(stan_file, force_recompile = TRUE)
      file.remove(build_record_path(mod$exe_file()))
      adopted <- cmdstan_model(exe_file = mod$exe_file())
    }
  )
  expect_null(features(adopted)$stan_threads)
  expect_error(
    adopted$sample(data = data_file, threads_per_chain = 2),
    "does not report threading as enabled", fixed = TRUE
  )
})
