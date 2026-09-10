test_that("parse_exe_info_string works", {
  expect_equal_ignore_order(
    parse_exe_info_string("
      stan_version_major = 2
      stan_version_minor = 38
      stan_version_patch = 0
      STAN_THREADS=false
      STAN_MPI=false
      STAN_OPENCL=true
      STAN_NO_RANGE_CHECKS=false
      STAN_CPP_OPTIMS=false
    "),
    list(
      stan_version = "2.38.0",
      stan_threads = FALSE,
      stan_mpi = FALSE,
      stan_opencl = TRUE,
      stan_no_range_checks = FALSE,
      stan_cpp_optims = FALSE
    )
  )
})

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

test_that("cpp option checks are case-insensitive", {
  expect_identical(
    assert_valid_threads(2L, list(STAN_THREADS = TRUE)),
    2L
  )
  expect_identical(
    assert_valid_threads(2L, list(stan_threads = TRUE)),
    2L
  )
  expect_identical(
    assert_valid_opencl(c(0L, 0L), list(STAN_OPENCL = TRUE)),
    c(0L, 0L)
  )
  expect_identical(
    assert_valid_opencl(c(0L, 0L), list(stan_opencl = TRUE)),
    c(0L, 0L)
  )
})

test_that("lowercase stan_threads behavior remains unchanged", {
  expect_null(assert_valid_threads(NULL, list(stan_threads = FALSE)))
  expect_null(assert_valid_threads(NULL, list(stan_threads = "dummy string")))
  expect_snapshot({
    assert_valid_threads(2L, list(stan_threads = FALSE))
    assert_valid_threads(2L, list(stan_threads = "dummy string"))
  })
})

test_that("cpp option checks prefer the last case-insensitive match", {
  expect_identical(
    assert_valid_threads(
      2L,
      list(stan_threads = FALSE, STAN_THREADS = TRUE)
    ),
    2L
  )
  expect_identical(
    assert_valid_opencl(
      c(0L, 0L),
      list(stan_opencl = NULL, STAN_OPENCL = TRUE)
    ),
    c(0L, 0L)
  )
})

test_that("uppercase stan_threads requires a thread count", {
  expect_snapshot(
    error = TRUE,
    assert_valid_threads(NULL, list(STAN_THREADS = TRUE))
  )
})

test_that("cpp option checks do not use partial matching", {
  expect_snapshot(
    error = TRUE,
    assert_valid_opencl(c(0L, 0L), list(stan_opencl_x = TRUE))
  )
})

test_that("option comparison keeps the Stan version like any other option", {
  expect_true(cpp_options_disagree(
    list(STAN_VERSION = "9.9"),
    list(STAN_VERSION = "8.8")
  ))
})

test_that("exe_info cpp_options comparison works", {
  exe_info_all_flags_off <- exe_info_style_cpp_options(list())
  exe_info_all_flags_off[["STAN_VERSION"]] <- "35.0.0"

  expect_true(exe_info_reflects_cpp_options(
    exe_info_all_flags_off,
    list()
  ))
  expect_true(exe_info_reflects_cpp_options(
    list(STAN_OPENCL = FALSE),
    list(STAN_OPENCL = NULL)
  ))
  expect_true(exe_info_reflects_cpp_options(
    list(STAN_OPENCL = FALSE),
    list(STAN_OPENCL = FALSE)
  ))
  expect_not_true(exe_info_reflects_cpp_options(
    list(STAN_OPENCL = FALSE, STAN_THREADS = FALSE),
    list(STAN_OPENCL = NULL, STAN_THREADS = TRUE)
  ))
  expect_not_true(exe_info_reflects_cpp_options(
    list(STAN_OPENCL = FALSE, STAN_THREADS = FALSE),
    list(STAN_OPENCL = NULL, STAN_THREADS = TRUE, EXTRA_ARG = TRUE)
  ))

  # no exe_info -> no recompile based on cpp info
  expect_warning(
    expect_true(exe_info_reflects_cpp_options(list(), list())),
    "Recompiling is recommended"
  )
})

test_that("exe_info comparison reads cpp_options the way make does", {
  # Upper-case, as model_compile_info() reports it and as
  # assert_valid_cpp_options() hands the request over.
  disabled <- list(STAN_THREADS = FALSE)

  # Every duplicate reaches make and a makefile takes the last, so the order
  # decides which of these agrees.
  expect_true(exe_info_reflects_cpp_options(
    disabled,
    list(STAN_THREADS = TRUE, STAN_THREADS = NULL)
  ))
  expect_not_true(exe_info_reflects_cpp_options(
    disabled,
    list(STAN_THREADS = NULL, STAN_THREADS = TRUE)
  ))

  # A vector value expands into one assignment per element, and the last one
  # decides.
  expect_not_true(exe_info_reflects_cpp_options(
    disabled,
    list(STAN_THREADS = c(FALSE, TRUE))
  ))

  # FALSE reaches make as an empty assignment, so it asks for the option off.
  expect_true(
    exe_info_reflects_cpp_options(disabled, list(STAN_THREADS = FALSE))
  )

  # An option the binary cannot report is unverifiable, not a mismatch.
  expect_true(
    exe_info_reflects_cpp_options(disabled, list(MY_CUSTOM_MAKE_FLAG = TRUE))
  )
})
