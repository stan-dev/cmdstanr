test_that("a matching pair is current", {
  exe <- local_fake_exe()
  record <- example_record(exe)
  expected <- example_expected(record)
  current <- example_current(record)
  expect_equal(assess_build(expected, current), character(0))
})

test_that("every changed row is reported, resolved dependencies included", {
  exe <- local_fake_exe()
  record <- example_record(exe)
  expected <- example_expected(record)
  current <- example_current(record)
  expected$configuration$stanc_name <- "other_model"
  current$dependencies$stan_file$hash <- "1e0f"
  current$dependencies$included_files[[1]]$hash <- "3c2d"
  current$dependencies$user_header <- list(hash = "cccc", built_from = "h.hpp")
  current$dependencies$make_local$hash <- "5a4b"
  expect_equal(
    assess_build(expected, current),
    c("stanc_name", "stan_file", "included_files", "user_header", "make_local")
  )
})

test_that("an unusable record is the only reason", {
  exe <- local_fake_exe()
  record <- example_record(exe)
  expected <- example_expected(record)
  current <- example_current(record)
  expected$configuration$stanc_name <- "other_model"
  reasons <- c(
    "missing", "unreadable", "unsupported_format", "executable_mismatch"
  )
  for (reason in reasons) {
    current$record <- list(status = "unavailable", reason = reason)
    expect_equal(assess_build(expected, current), reason)
  }
})

test_that("a replaced executable is caught only by the object's own hash", {
  exe <- local_fake_exe()
  record <- example_record(exe)
  expected <- example_expected(record)
  current <- example_current(record)
  expected$executable_hash <- "0000"
  expect_equal(assess_build(expected, current), "executable")
  expected$executable_hash <- NULL
  expect_equal(assess_build(expected, current), character(0))
})

test_that("unresolved dependencies are skipped, not read as empty", {
  exe <- local_fake_exe()
  record <- example_record(exe)
  expected <- example_expected(record)
  current <- example_current(record)
  current$dependencies <- NULL
  current$cmdstan$path <- "/opt/cmdstan-2.40.0"
  expect_equal(assess_build(expected, current), "cmdstan")
  expected$configuration$stanc_name <- "other_model"
  expect_equal(assess_build(expected, current), c("stanc_name", "cmdstan"))
})

test_that("a gone CmdStan is not a trigger", {
  exe <- local_fake_exe()
  record <- example_record(exe)
  gone <- file.path(withr::local_tempdir(), "cmdstan")
  expect_false(dir.exists(gone))
  record$cmdstan$path <- gone
  expected <- example_expected(record)
  current <- example_current(record)
  expect_equal(assess_build(expected, current), character(0))
  current$cmdstan$path <- withr::local_tempdir()
  expect_equal(assess_build(expected, current), "cmdstan")
})
