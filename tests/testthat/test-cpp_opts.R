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

test_that("read_exe_info fails loudly when threading capability is unavailable", {
  local_mocked_bindings(
    run_info_cli = function(exe_file) list(status = 1L, stdout = "", stderr = "boom")
  )
  expect_error(
    read_exe_info("model"),
    "Unable to inspect CmdStan executable.*boom"
  )

  local_mocked_bindings(
    run_info_cli = function(exe_file) {
      list(status = 0L, stdout = "stan_version_major=2\nstan_version_minor=38\nstan_version_patch=0")
    }
  )
  expect_error(
    read_exe_info("model"),
    "does not report 'stan_threads'"
  )
})

test_that("validate_cpp_options works", {
  expect_equal_ignore_order(
    validate_cpp_options(list(
      Stan_Threads = TRUE,
      STAN_OPENCL = NULL,
      aBc = FALSE
    )),
    list(
      stan_threads = TRUE,
      stan_opencl = NULL,
      abc = FALSE
    )
  )
  expect_warning(validate_cpp_options(list(STAN_OPENCL = FALSE)))
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

test_that("uppercase stan_threads uses the default thread count", {
  expect_identical(
    assert_valid_threads(NULL, list(STAN_THREADS = TRUE)),
    1L
  )
})

test_that("cpp option checks do not use partial matching", {
  expect_snapshot(
    error = TRUE,
    assert_valid_opencl(c(0L, 0L), list(stan_opencl_x = TRUE))
  )
})

test_that("exe_info cpp_options comparison works", {
  exe_info_all_flags_off <- exe_info_style_cpp_options(list())
  exe_info_all_flags_off[["stan_version"]] <- "35.0.0"

  expect_true(exe_info_reflects_cpp_options(
    exe_info_all_flags_off,
    list()
  ))
  expect_true(exe_info_reflects_cpp_options(
    list(stan_opencl = FALSE),
    list(stan_opencl = NULL)
  ))
  expect_not_true(exe_info_reflects_cpp_options(
    list(stan_opencl = FALSE),
    list(stan_opencl = FALSE)
  ))
  expect_not_true(exe_info_reflects_cpp_options(
    list(stan_opencl = FALSE, stan_threads = FALSE),
    list(stan_opencl = NULL, stan_threads = TRUE)
  ))
  expect_not_true(exe_info_reflects_cpp_options(
    list(stan_opencl = FALSE, stan_threads = FALSE),
    list(stan_opencl = NULL, stan_threads = TRUE, EXTRA_ARG = TRUE)
  ))

  # no exe_info -> no recompile based on cpp info
  expect_warning(
    expect_true(exe_info_reflects_cpp_options(list(), list())),
    "Recompiling is recommended"
  )
})
