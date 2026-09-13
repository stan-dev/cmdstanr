test_that("build_record_path names the record after the executable file", {
  exe <- local_fake_exe()
  expect_equal(
    build_record_path(exe),
    file.path(dirname(exe), ".bernoulli.cmdstanr.json")
  )

  windows_exe <- local_fake_exe("bernoulli.exe")
  expect_equal(
    build_record_path(windows_exe),
    file.path(dirname(windows_exe), ".bernoulli.exe.cmdstanr.json")
  )

  spaced <- local_fake_exe("my model")
  expect_equal(
    build_record_path(spaced),
    file.path(dirname(spaced), ".my model.cmdstanr.json")
  )
})

test_that("a written record reads back unchanged", {
  exe <- local_fake_exe()
  record <- example_record(exe)
  path <- write_build_record(record, exe)

  result <- read_build_record(exe)
  expect_equal(result$status, "available")
  expect_false("reason" %in% names(result))
  expect_equal(result$record, record)

  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_false(grepl("null", text, fixed = TRUE))
  expect_match(text, '"included_files"\\s*:\\s*\\[')
})

test_that("an empty object writes as {} and an empty array as []", {
  exe <- local_fake_exe()
  record <- example_record(exe)
  record$request$cpp_options_supplied <- structure(list(), names = character())
  record$dependencies$included_files <- list()
  path <- write_build_record(record, exe)

  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(text, '"cpp_options_supplied"\\s*:\\s*\\{\\s*\\}')
  expect_match(text, '"included_files"\\s*:\\s*\\[\\s*\\]')

  result <- read_build_record(exe)
  expect_equal(
    result$record$request$cpp_options_supplied,
    structure(list(), names = character())
  )
  expect_equal(result$record$dependencies$included_files, list())
})

test_that("an executable with no record beside it is missing", {
  exe <- local_fake_exe()
  expect_equal(
    read_build_record(exe),
    list(status = "unavailable", reason = "missing")
  )
})

test_that("a record that is not JSON is unreadable", {
  exe <- local_fake_exe()
  writeLines("{not json", build_record_path(exe))

  result <- read_build_record(exe)
  expect_equal(result$reason, "unreadable")
  expect_false("record" %in% names(result))
  expect_false("format_version" %in% names(result))
})

test_that("a record with a field of the wrong type is unreadable", {
  exe <- local_fake_exe()
  record <- example_record(exe)
  record$builder$version <- 42
  jsonlite::write_json(
    record, build_record_path(exe),
    auto_unbox = TRUE, pretty = TRUE, digits = NA
  )

  result <- read_build_record(exe)
  expect_equal(result$reason, "unreadable")
  expect_false("record" %in% names(result))
  expect_false("format_version" %in% names(result))
})

test_that("a record repeating a member at its top level is unreadable", {
  exe <- local_fake_exe()
  json <- jsonlite::toJSON(example_record(exe), auto_unbox = TRUE, digits = NA)
  writeLines(
    sub("}$", ",\"artifact\":\"different\"}", json),
    build_record_path(exe)
  )

  result <- read_build_record(exe)
  expect_equal(result$reason, "unreadable")
  expect_false("record" %in% names(result))
  expect_false("format_version" %in% names(result))
})

test_that("a dependency missing its hash is unreadable", {
  exe <- local_fake_exe()
  record <- example_record(exe)
  record$dependencies$stan_file$hash <- NULL
  jsonlite::write_json(
    record, build_record_path(exe),
    auto_unbox = TRUE, pretty = TRUE, digits = NA
  )

  result <- read_build_record(exe)
  expect_equal(result$reason, "unreadable")
  expect_false("record" %in% names(result))
  expect_false("format_version" %in% names(result))
})

test_that("a feature written as null is unreadable", {
  exe <- local_fake_exe()
  record <- example_record(exe)
  record$reported_features$stan_threads <- NA
  jsonlite::write_json(
    record, build_record_path(exe),
    auto_unbox = TRUE, pretty = TRUE, digits = NA
  )

  result <- read_build_record(exe)
  expect_equal(result$reason, "unreadable")
  expect_false("record" %in% names(result))
  expect_false("format_version" %in% names(result))
})

test_that("a record in a format we do not read is checked on its version alone", {
  exe <- local_fake_exe()
  record <- example_record(exe)
  record$format_version <- 99L
  record$builder <- "garbage"
  jsonlite::write_json(
    record, build_record_path(exe),
    auto_unbox = TRUE, pretty = TRUE, digits = NA
  )

  result <- read_build_record(exe)
  expect_equal(result$reason, "unsupported_format")
  expect_equal(result$format_version, 99)
  expect_false("record" %in% names(result))
})

test_that("a record whose format_version is not an integer is unreadable", {
  exe <- local_fake_exe()
  record <- example_record(exe)
  record$format_version <- 1.000000001
  jsonlite::write_json(
    record, build_record_path(exe),
    auto_unbox = TRUE, pretty = TRUE, digits = NA
  )

  result <- read_build_record(exe)
  expect_equal(result$reason, "unreadable")
  expect_false("record" %in% names(result))
  expect_false("format_version" %in% names(result))
})

test_that("a record whose hash does not match the executable is a mismatch", {
  exe <- local_fake_exe()
  write_build_record(example_record(exe), exe)
  writeBin(as.raw(c(0x7f, 0x45, 0x4c, 0x46, 0x00)), exe)

  result <- read_build_record(exe)
  expect_equal(result$reason, "artifact_mismatch")
  expect_false("record" %in% names(result))
  expect_false("format_version" %in% names(result))
})

test_that("reported features keep enabled, disabled and unknown apart", {
  write_features <- function(exe, features) {
    record <- example_record(exe)
    record$reported_features <- features
    write_build_record(record, exe)
    read_build_record(exe)
  }

  on_exe <- local_fake_exe("threads_on")
  off_exe <- local_fake_exe("threads_off")
  silent_exe <- local_fake_exe("threads_unreported")

  on <- write_features(
    on_exe, list(stan_threads = TRUE, stan_version = "2.39.0")
  )
  off <- write_features(
    off_exe, list(stan_threads = FALSE, stan_version = "2.39.0")
  )
  silent <- write_features(silent_exe, list(stan_version = "2.39.0"))

  expect_equal(on$status, "available")
  expect_equal(off$status, "available")
  expect_equal(silent$status, "available")

  expect_true(on$record$reported_features$stan_threads)
  expect_false(off$record$reported_features$stan_threads)
  expect_null(silent$record$reported_features$stan_threads)

  expect_true("stan_threads" %in% names(on$record$reported_features))
  expect_true("stan_threads" %in% names(off$record$reported_features))
  expect_false("stan_threads" %in% names(silent$record$reported_features))
})

test_that("the validator names the field that fails", {
  exe <- local_fake_exe()
  base <- example_record(exe)
  rebuild <- function(record) do.call(new_build_record, record[-1])

  bad_version <- base
  bad_version$builder$version <- "2.39"
  expect_error(rebuild(bad_version), "`builder.version`", fixed = TRUE)

  no_name <- base
  no_name$request$stanc_name <- ""
  expect_error(rebuild(no_name), "`request.stanc_name`", fixed = TRUE)

  logical_option <- base
  logical_option$request$cpp_options_supplied <- list(STAN_THREADS = TRUE)
  expect_error(
    rebuild(logical_option),
    "`request.cpp_options_supplied.STAN_THREADS`",
    fixed = TRUE
  )

  repeated_option <- base
  repeated_option$request$cpp_options_supplied <- list(
    STAN_THREADS = "false", STAN_THREADS = "true"
  )
  expect_error(
    rebuild(repeated_option), "`request.cpp_options_supplied`", fixed = TRUE
  )

  unknown_kind <- base
  unknown_kind$known_untracked_dependencies <- list(
    list(kind = "mystery", detected_in = "make/local")
  )
  expect_error(
    rebuild(unknown_kind),
    "`known_untracked_dependencies[[1]].kind`",
    fixed = TRUE
  )

  odd_feature <- base
  odd_feature$reported_features$stan_threads <- "yes"
  expect_error(
    rebuild(odd_feature), "`reported_features.stan_threads`", fixed = TRUE
  )

  no_source <- base
  no_source$dependencies$stan_file <- NULL
  expect_error(rebuild(no_source), "`dependencies.stan_file`", fixed = TRUE)
})

test_that("a record missing a required field is unreadable whole", {
  exe <- local_fake_exe()
  path <- write_build_record(example_record(exe), exe)
  edited <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  edited$tbb_dir <- NULL
  jsonlite::write_json(
    edited, path, auto_unbox = TRUE, pretty = TRUE, digits = NA
  )

  result <- read_build_record(exe)
  expect_equal(result$reason, "unreadable")
  expect_false("format_version" %in% names(result))
  expect_false("record" %in% names(result))
})

test_that("a record carrying a member the schema does not name still reads", {
  exe <- local_fake_exe()
  path <- write_build_record(example_record(exe), exe)
  edited <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  edited$extra <- "written by a later cmdstanr"
  jsonlite::write_json(
    edited, path, auto_unbox = TRUE, pretty = TRUE, digits = NA
  )

  expect_equal(read_build_record(exe)$status, "available")
})

test_that("the hash catches what ordering cannot", {
  path <- local_fake_exe()
  exe_a <- as.raw(c(0x7f, 0x45, 0x4c, 0x46))
  exe_b <- as.raw(c(0x4d, 0x5a, 0x90, 0x00))
  writeBin(exe_a, path)
  record_a <- example_record(path)
  writeBin(exe_b, path)
  record_b <- example_record(path)

  # Two builds reach one path, each writing its executable before its record.
  # Every write lands in an order the transaction allows, and the build that
  # wrote the last executable is not the one that wrote the last record.
  writeBin(exe_a, path)
  writeBin(exe_b, path)
  write_build_record(record_b, path)
  write_build_record(record_a, path)

  expect_error(verify_build_record(path), "artifact_mismatch", fixed = TRUE)
  expect_equal(read_build_record(path)$reason, "artifact_mismatch")
})

test_that("two identical build records compare with no differences", {
  exe <- local_fake_exe()
  recorded <- example_record(exe)
  current <- example_record(exe)
  expect_equal(compare_build_records(recorded, current), character(0))
})

test_that("a changed cpp option value differs as cpp_options", {
  exe <- local_fake_exe()
  recorded <- example_record(exe)
  current <- example_record(exe)
  current$request$cpp_options_supplied <- list(STAN_THREADS = "false")
  expect_equal(compare_build_records(recorded, current), "cpp_options")
})

test_that("a reordered stanc_options_supplied differs as stanc_options", {
  exe <- local_fake_exe()
  recorded <- example_record(exe)
  current <- example_record(exe)
  recorded$request$stanc_options_supplied <- list("--O0", "--O1")
  current$request$stanc_options_supplied <- list("--O1", "--O0")
  expect_equal(compare_build_records(recorded, current), "stanc_options")
})

test_that("a changed stanc_name differs as stanc_name", {
  exe <- local_fake_exe()
  recorded <- example_record(exe)
  current <- example_record(exe)
  current$request$stanc_name <- "other_model"
  expect_equal(compare_build_records(recorded, current), "stanc_name")
})

test_that("a changed stan_file hash differs as stan_file", {
  exe <- local_fake_exe()
  recorded <- example_record(exe)
  current <- example_record(exe)
  current$dependencies$stan_file$hash <- "ffff"
  expect_equal(compare_build_records(recorded, current), "stan_file")
})

test_that("a reordered included_files differs as included_files", {
  exe <- local_fake_exe()
  recorded <- example_record(exe)
  current <- example_record(exe)
  recorded$dependencies$included_files <- list(
    list(hash = "aaaa", built_from = "one.stan"),
    list(hash = "bbbb", built_from = "two.stan")
  )
  current$dependencies$included_files <- list(
    list(hash = "bbbb", built_from = "two.stan"),
    list(hash = "aaaa", built_from = "one.stan")
  )
  expect_equal(compare_build_records(recorded, current), "included_files")
})

test_that("a user_header present on only one side differs as user_header", {
  exe <- local_fake_exe()
  recorded <- example_record(exe)
  current <- example_record(exe)
  current$dependencies$user_header <- list(hash = "cccc", built_from = "header.hpp")
  expect_equal(compare_build_records(recorded, current), "user_header")
})

test_that("a user_header with the same hash but a different built_from differs as user_header", {
  exe <- local_fake_exe()
  recorded <- example_record(exe)
  current <- example_record(exe)
  recorded$dependencies$user_header <- list(hash = "cccc", built_from = "header.hpp")
  current$dependencies$user_header <- list(hash = "cccc", built_from = "other/header.hpp")
  expect_equal(compare_build_records(recorded, current), "user_header")
})

test_that("a changed user_header hash at the same built_from differs as user_header", {
  exe <- local_fake_exe()
  recorded <- example_record(exe)
  current <- example_record(exe)
  recorded$dependencies$user_header <- list(hash = "cccc", built_from = "header.hpp")
  current$dependencies$user_header <- list(hash = "dddd", built_from = "header.hpp")
  expect_equal(compare_build_records(recorded, current), "user_header")
})

test_that("a make_local present on only one side differs as make_local", {
  exe <- local_fake_exe()
  recorded <- example_record(exe)
  current <- example_record(exe)
  current$dependencies$make_local <- NULL
  expect_equal(compare_build_records(recorded, current), "make_local")
})

test_that("a changed make_local hash differs as make_local", {
  exe <- local_fake_exe()
  recorded <- example_record(exe)
  current <- example_record(exe)
  current$dependencies$make_local$hash <- "ffff"
  expect_equal(compare_build_records(recorded, current), "make_local")
})

test_that("a changed artifact differs as artifact", {
  exe <- local_fake_exe()
  recorded <- example_record(exe)
  current <- example_record(exe)
  current$artifact <- "deadbeef"
  expect_equal(compare_build_records(recorded, current), "artifact")
})

test_that("a changed builder version differs as builder", {
  exe <- local_fake_exe()
  recorded <- example_record(exe)
  current <- example_record(exe)
  current$builder$version <- "2.40.0"
  expect_equal(compare_build_records(recorded, current), "builder")
})

test_that("a changed builder path differs as builder", {
  exe <- local_fake_exe()
  recorded <- example_record(exe)
  current <- example_record(exe)
  current$builder$path <- "/opt/cmdstan-2.40.0"
  expect_equal(compare_build_records(recorded, current), "builder")
})

test_that("two differences are both reported, in table order", {
  exe <- local_fake_exe()
  recorded <- example_record(exe)
  current <- example_record(exe)
  current$dependencies$stan_file$hash <- "ffff"
  current$builder$version <- "2.40.0"
  expect_equal(compare_build_records(recorded, current), c("stan_file", "builder"))
})

test_that("the same cpp options in a different assignment order do not differ", {
  exe <- local_fake_exe()
  recorded <- example_record(exe)
  current <- example_record(exe)
  recorded$request$cpp_options_supplied <- list(
    STAN_THREADS = "true", STAN_NO_RANGE_CHECKS = "true"
  )
  current$request$cpp_options_supplied <- list(
    STAN_NO_RANGE_CHECKS = "true", STAN_THREADS = "true"
  )
  expect_equal(compare_build_records(recorded, current), character(0))
})

test_that("an included file with the same hash and a different built_from does not differ", {
  exe <- local_fake_exe()
  recorded <- example_record(exe)
  current <- example_record(exe)
  recorded$dependencies$included_files <- list(
    list(hash = "aaaa", built_from = "one.stan")
  )
  current$dependencies$included_files <- list(
    list(hash = "aaaa", built_from = "elsewhere/one.stan")
  )
  expect_equal(compare_build_records(recorded, current), character(0))
})

test_that("a make_local with the same hash and a different built_from does not differ", {
  exe <- local_fake_exe()
  recorded <- example_record(exe)
  current <- example_record(exe)
  recorded$dependencies$make_local <- list(hash = "4b5a", built_from = "make/local")
  current$dependencies$make_local <- list(hash = "4b5a", built_from = "other/make/local")
  expect_equal(compare_build_records(recorded, current), character(0))
})

test_that("differences outside the comparison table never count", {
  exe <- local_fake_exe()
  recorded <- example_record(exe)
  current <- example_record(exe)
  current$request$stanc_options_injected <- list("--name=other_model")
  current$request$include_paths <- list("/some/other/path")
  current$reported_features$stan_opencl <- TRUE
  current$tbb_dir <- "/opt/other/tbb"
  current$known_untracked_dependencies <- list(
    list(kind = "user_header_include", detected_in = "other.hpp")
  )
  expect_equal(compare_build_records(recorded, current), character(0))
})

test_that("both detectors fire on their patterns and on nothing else", {
  dir <- withr::local_tempdir()
  make_local <- file.path(dir, "local")
  user_header <- file.path(dir, "user.hpp")
  from_make_local <- function(line) {
    writeLines(line, make_local)
    untracked_dependencies(make_local = make_local)
  }
  from_user_header <- function(line) {
    writeLines(line, user_header)
    untracked_dependencies(user_header = user_header)
  }
  make_local_hit <- list(
    list(kind = "make_local_include", detected_in = make_local)
  )
  user_header_hit <- list(
    list(kind = "user_header_include", detected_in = user_header)
  )

  expect_equal(from_make_local("include other.mk"), make_local_hit)
  expect_equal(from_make_local("-include other.mk"), make_local_hit)
  expect_equal(from_make_local("sinclude other.mk"), make_local_hit)
  expect_equal(from_make_local(" include other.mk"), make_local_hit)
  expect_equal(from_make_local("# include other.mk"), list())
  expect_equal(from_make_local("INCLUDE_DIR = x"), list())
  expect_equal(from_make_local("CXXFLAGS += -include foo.h"), list())

  expect_equal(from_user_header("#include \"a.hpp\""), user_header_hit)
  expect_equal(from_user_header("  #  include   \"a.hpp\""), user_header_hit)
  expect_equal(from_user_header("#include <vector>"), list())
  expect_equal(from_user_header("// #include \"a.hpp\""), list())

  expect_equal(untracked_dependencies(), list())
})

test_that("a failed write leaves no staging file behind", {
  exe <- local_fake_exe()
  local_mocked_bindings(
    write_json = function(x, path, ...) {
      writeLines("{", path)
      stop("disk full")
    },
    .package = "jsonlite"
  )

  expect_error(write_build_record(example_record(exe), exe), "disk full")
  expect_equal(
    list.files(dirname(exe), all.files = TRUE, no.. = TRUE),
    basename(exe)
  )
})

test_that("reported features the record cannot hold are left unknown", {
  exe <- local_fake_exe()
  # A nameless entry and a missing version are both dropped, not written.
  local_mocked_bindings(
    run_info_cli = function(...) {
      list(status = 0, stdout = " = true\nSTAN_THREADS=true\n")
    }
  )

  expect_equal(reported_features_from_exe(exe), list(stan_threads = TRUE))
})
