# A result built from `example_record()`, written beside a fake executable
# and optionally edited first. `run_info_cli` stops if launched, so this is
# always a genuinely available record read straight from disk.
available_result <- function(edit = identity) {
  exe <- local_fake_exe()
  write_build_record(edit(example_record(exe)), exe)
  local_mocked_bindings(
    run_info_cli = function(...) stop("launched the executable"),
    .package = "cmdstanr"
  )
  stan_build_info(exe)
}

test_that("an available build record is read in full without launching the executable", {
  result <- available_result()

  expect_identical(class(result), "stan_build_info")
  expect_named(
    result,
    c(
      "record", "reported_features", "configuration", "dependencies",
      "cmdstan", "untracked_dependencies"
    )
  )

  expect_named(result$record, c("status", "reason"))
  expect_identical(result$record$status, "available")
  expect_null(result$record$reason)

  features <- result$reported_features
  expect_named(
    features,
    c(
      "stan_threads", "stan_mpi", "stan_opencl", "stan_no_range_checks",
      "stan_version"
    )
  )
  expect_type(features$stan_threads, "logical")
  expect_type(features$stan_mpi, "logical")
  expect_type(features$stan_opencl, "logical")
  expect_type(features$stan_no_range_checks, "logical")
  expect_type(features$stan_version, "character")
  expect_true(features$stan_threads)
  expect_identical(features$stan_mpi, NA)
  expect_false(features$stan_opencl)
  expect_identical(features$stan_no_range_checks, NA)
  expect_identical(features$stan_version, "2.39.0")

  expect_named(
    result$configuration, c("cpp_options", "stanc_options", "include_paths")
  )
  expect_equal(result$configuration$cpp_options, list(STAN_THREADS = "true"))
  expect_equal(result$configuration$stanc_options, list("--O1"))
  expect_type(result$configuration$include_paths, "character")

  expect_named(
    result$dependencies, c("stan_file", "included_files", "user_header", "make_local")
  )
  expect_named(result$dependencies$stan_file, c("built_from", "exists"))
  expect_equal(
    result$dependencies$stan_file,
    list(built_from = "bernoulli.stan", exists = FALSE)
  )
  expect_equal(
    result$dependencies$included_files,
    list(list(built_from = "helpers.stan", exists = FALSE))
  )
  expect_null(result$dependencies$user_header)
  expect_equal(
    result$dependencies$make_local,
    list(built_from = "make/local", exists = FALSE)
  )

  expect_named(result$cmdstan, c("path", "version", "exists"))
  expect_equal(
    result$cmdstan,
    list(path = "/opt/cmdstan-2.39.0", version = "2.39.0", exists = FALSE)
  )

  expect_equal(
    result$untracked_dependencies,
    list(list(kind = "make_local_include", detected_in = "make/local"))
  )
})

test_that("fields the record withholds are absent from the result", {
  result <- available_result()

  expect_false("executable_hash" %in% names(result))
  expect_false("tbb_dir" %in% names(result))

  expect_false("hash" %in% names(result$dependencies$stan_file))
  expect_false("hash" %in% names(result$dependencies$included_files[[1]]))
  expect_false("hash" %in% names(result$dependencies$make_local))
  expect_named(result$dependencies$stan_file, c("built_from", "exists"))
  expect_named(result$dependencies$included_files[[1]], c("built_from", "exists"))
  expect_named(result$dependencies$make_local, c("built_from", "exists"))

  expect_false("stanc_options_added" %in% names(result$configuration))
  expect_false("stanc_options_from_make" %in% names(result$configuration))
  expect_false("stanc_name" %in% names(result$configuration))
  expect_named(
    result$configuration, c("cpp_options", "stanc_options", "include_paths")
  )
})

test_that("a missing build record falls back to the executable's own info", {
  exe <- local_fake_exe()
  local_mocked_bindings(
    run_info_cli = function(...) default_info_ret,
    .package = "cmdstanr"
  )
  result <- stan_build_info(exe)

  expect_identical(class(result), "stan_build_info")
  expect_named(result, c("record", "reported_features"))
  expect_equal(result$record, list(status = "unavailable", reason = "missing"))
  expect_named(result$record, c("status", "reason"))
  expect_identical(result$reported_features$stan_version, "2.39.0")
  expect_false("format_version" %in% names(result))
})

test_that("an unreadable build record falls back to the executable's own info", {
  exe <- local_fake_exe()
  record <- example_record(exe)
  record$configuration$stanc_name <- ""
  jsonlite::write_json(record, build_record_path(exe), auto_unbox = TRUE)

  local_mocked_bindings(
    run_info_cli = function(...) default_info_ret,
    .package = "cmdstanr"
  )
  result <- stan_build_info(exe)

  expect_identical(class(result), "stan_build_info")
  expect_named(result, c("record", "reported_features"))
  expect_equal(result$record, list(status = "unavailable", reason = "unreadable"))
  expect_identical(result$reported_features$stan_version, "2.39.0")
  expect_false("format_version" %in% names(result))
})

test_that("an unsupported record format reports its version and nothing else", {
  local_mocked_bindings(
    run_info_cli = function(...) default_info_ret,
    .package = "cmdstanr"
  )

  newer_exe <- local_fake_exe("newer")
  newer <- example_record(newer_exe)
  newer$format_version <- 2L
  newer$configuration <- NULL
  jsonlite::write_json(newer, build_record_path(newer_exe), auto_unbox = TRUE)
  newer_result <- stan_build_info(newer_exe)

  expect_identical(class(newer_result), "stan_build_info")
  expect_named(newer_result, c("record", "reported_features", "format_version"))
  expect_equal(
    newer_result$record, list(status = "unavailable", reason = "unsupported_format")
  )
  expect_identical(newer_result$format_version, 2L)

  older_exe <- local_fake_exe("older")
  older <- example_record(older_exe)
  older$format_version <- 0L
  older$configuration <- NULL
  jsonlite::write_json(older, build_record_path(older_exe), auto_unbox = TRUE)
  older_result <- stan_build_info(older_exe)

  expect_equal(
    older_result$record, list(status = "unavailable", reason = "unsupported_format")
  )
  expect_identical(older_result$format_version, 0L)
})

test_that("an executable mismatch discards the recorded features for the binary's own", {
  exe <- local_fake_exe()
  record <- example_record(exe)
  record$executable_hash <- "0000"
  write_build_record(record, exe)

  mismatched_info <- default_info_ret
  mismatched_info$stdout <- sub(
    "STAN_THREADS=true", "STAN_THREADS=false", mismatched_info$stdout, fixed = TRUE
  )
  local_mocked_bindings(
    run_info_cli = function(...) mismatched_info,
    .package = "cmdstanr"
  )
  result <- stan_build_info(exe)

  expect_identical(class(result), "stan_build_info")
  expect_named(result, c("record", "reported_features"))
  expect_equal(
    result$record, list(status = "unavailable", reason = "executable_mismatch")
  )
  expect_false(result$reported_features$stan_threads)
  expect_null(result$configuration)
  expect_null(result$dependencies)
  expect_null(result$cmdstan)
  expect_null(result$untracked_dependencies)
  expect_false("format_version" %in% names(result))
})

test_that("a dependency's built_from path is checked for existing on disk", {
  gone <- NULL
  expect_no_warning(expect_no_message(gone <- available_result()))
  expect_false(gone$dependencies$stan_file$exists)

  present_file <- withr::local_tempfile()
  file.create(present_file)
  present <- available_result(function(record) {
    record$dependencies$stan_file$built_from <- present_file
    record
  })
  expect_true(present$dependencies$stan_file$exists)
})

test_that("the recorded cmdstan installation is checked for existing on disk", {
  gone <- available_result()
  expect_false(gone$cmdstan$exists)

  present_dir <- withr::local_tempdir()
  present <- available_result(function(record) {
    record$cmdstan$path <- present_dir
    record
  })
  expect_true(present$cmdstan$exists)
})

test_that("a record without a user header or make/local reports them as NULL", {
  result <- available_result(function(record) {
    record$dependencies$make_local <- NULL
    record
  })

  expect_named(
    result$dependencies, c("stan_file", "included_files", "user_header", "make_local")
  )
  expect_null(result$dependencies$user_header)
  expect_null(result$dependencies$make_local)
  expect_true("make_local" %in% names(result$dependencies))
})

test_that("an empty untracked dependencies list differs from having no record at all", {
  result <- available_result(function(record) {
    record$untracked_dependencies <- list()
    record
  })
  expect_equal(result$untracked_dependencies, list())

  exe <- local_fake_exe()
  local_mocked_bindings(
    run_info_cli = function(...) default_info_ret,
    .package = "cmdstanr"
  )
  missing_result <- stan_build_info(exe)
  expect_false("untracked_dependencies" %in% names(missing_result))
})

test_that("untracked dependencies are ordered by kind and deduplicated", {
  result <- available_result(function(record) {
    record$untracked_dependencies <- list(
      list(kind = "user_header_include", detected_in = "user.hpp"),
      list(kind = "make_local_include", detected_in = "make/local"),
      list(kind = "user_header_include", detected_in = "user.hpp")
    )
    record
  })

  expect_equal(
    result$untracked_dependencies,
    list(
      list(kind = "make_local_include", detected_in = "make/local"),
      list(kind = "user_header_include", detected_in = "user.hpp")
    )
  )
})

test_that("a real user header is reported under dependencies and nowhere else", {
  stan_file <- file.path(withr::local_tempdir(), "bernoulli_external.stan")
  file.copy(testing_stan_file("bernoulli_external"), stan_file)
  header <- withr::local_tempfile(
    lines = c('#include "a.hpp"', '#include "b.hpp"'),
    fileext = ".hpp"
  )
  mod <- mock_cmdstan_model(stan_file, user_header = header)

  local_mocked_bindings(
    run_info_cli = function(...) stop("launched the executable"),
    .package = "cmdstanr"
  )
  result <- stan_build_info(mod$exe_file())
  expect_identical(result, mod$build_info())

  header_path <- resolve_path(header)
  expect_equal(
    result$dependencies$user_header,
    list(built_from = header_path, exists = TRUE)
  )
  expect_equal(
    result$untracked_dependencies,
    list(list(kind = "user_header_include", detected_in = header_path))
  )

  # The header's path is reported through `dependencies` (as `built_from`,
  # and again as `detected_in` because the header itself has untracked
  # includes) and nowhere else, e.g. not folded into `configuration`.
  elsewhere <- result
  elsewhere$dependencies$user_header <- NULL
  elsewhere$untracked_dependencies <- NULL
  expect_false(header_path %in% unlist(elsewhere))
})

test_that("stan_build_info() errors on unusable paths and unidentifiable executables", {
  missing_path <- withr::local_tempfile()
  expect_error(stan_build_info(missing_path), "does not exist")

  expect_error(stan_build_info(withr::local_tempdir()), "directory")

  failed_exe <- local_fake_exe("failed")
  local_mocked_bindings(
    run_info_cli = function(...) list(status = 1, stdout = ""),
    .package = "cmdstanr"
  )
  expect_error(
    stan_build_info(failed_exe),
    paste0(
      "Running '", failed_exe, "' with the argument 'info' did not report a ",
      "Stan version, so it is either not a CmdStan executable or cannot be ",
      "run."
    ),
    fixed = TRUE
  )

  no_version_exe <- local_fake_exe("no_version")
  local_mocked_bindings(
    run_info_cli = function(...) list(status = 0, stdout = "STAN_THREADS=true\n"),
    .package = "cmdstanr"
  )
  expect_error(
    stan_build_info(no_version_exe),
    paste0(
      "Running '", no_version_exe, "' with the argument 'info' did not ",
      "report a Stan version, so it is either not a CmdStan executable or ",
      "cannot be run."
    ),
    fixed = TRUE
  )
})

test_that("features reported by the binary have the fixed shape", {
  exe <- local_fake_exe()
  local_mocked_bindings(
    run_info_cli = function(...) default_info_ret,
    .package = "cmdstanr"
  )
  result <- stan_build_info(exe)
  features <- result$reported_features

  expect_named(
    features,
    c(
      "stan_threads", "stan_mpi", "stan_opencl", "stan_no_range_checks",
      "stan_version"
    )
  )
  expect_type(features$stan_threads, "logical")
  expect_type(features$stan_mpi, "logical")
  expect_type(features$stan_opencl, "logical")
  expect_type(features$stan_no_range_checks, "logical")
  expect_type(features$stan_version, "character")
  expect_identical(features$stan_version, "2.39.0")
})

test_that("print.stan_build_info() shows the fragments the spec pins", {
  unavailable_features <- list(
    stan_threads = NA, stan_mpi = NA, stan_opencl = NA,
    stan_no_range_checks = NA, stan_version = NA_character_
  )

  available_x <- structure(
    list(
      record = list(status = "available", reason = NULL),
      reported_features = list(
        stan_threads = TRUE, stan_mpi = NA, stan_opencl = FALSE,
        stan_no_range_checks = NA, stan_version = "2.39.0"
      ),
      configuration = list(
        cpp_options = list(STAN_THREADS = "true"),
        stanc_options = list("--O1"),
        include_paths = character(0)
      ),
      dependencies = list(
        stan_file = list(built_from = "bernoulli.stan", exists = FALSE),
        included_files = list(),
        user_header = NULL,
        make_local = NULL
      ),
      cmdstan = list(path = "/opt/cmdstan-2.39.0", version = "2.39.0", exists = FALSE),
      untracked_dependencies = list(
        list(kind = "make_local_include", detected_in = "make/local")
      )
    ),
    class = "stan_build_info"
  )
  missing_x <- structure(
    list(
      record = list(status = "unavailable", reason = "missing"),
      reported_features = unavailable_features
    ),
    class = "stan_build_info"
  )
  unreadable_x <- structure(
    list(
      record = list(status = "unavailable", reason = "unreadable"),
      reported_features = unavailable_features
    ),
    class = "stan_build_info"
  )
  mismatch_x <- structure(
    list(
      record = list(status = "unavailable", reason = "executable_mismatch"),
      reported_features = unavailable_features
    ),
    class = "stan_build_info"
  )
  newer_x <- structure(
    list(
      record = list(status = "unavailable", reason = "unsupported_format"),
      reported_features = unavailable_features,
      format_version = 2L
    ),
    class = "stan_build_info"
  )
  older_x <- structure(
    list(
      record = list(status = "unavailable", reason = "unsupported_format"),
      reported_features = unavailable_features,
      format_version = 0L
    ),
    class = "stan_build_info"
  )

  expect_output(print(available_x), "available", fixed = TRUE)
  expect_output(print(missing_x), "no build record", fixed = TRUE)
  expect_output(print(unreadable_x), "could not be read", fixed = TRUE)
  expect_output(print(mismatch_x), "does not match", fixed = TRUE)
  expect_output(print(newer_x), "newer version of cmdstanr", fixed = TRUE)
  expect_output(print(older_x), "rebuild", fixed = TRUE)
  expect_output(print(available_x), "stan_threads: TRUE", fixed = TRUE)
  expect_output(print(available_x), "stan_mpi: unknown", fixed = TRUE)
  expect_output(print(available_x), "no longer exists", fixed = TRUE)
  expect_output(
    print(available_x), "make/local includes another makefile", fixed = TRUE
  )
  expect_output(expect_invisible(print(available_x)))
})
