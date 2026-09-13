test_that("a matching pair is current", {
  exe <- local_fake_exe()
  record <- example_record(exe)
  expected <- example_expected(record)
  observed <- example_observed(record)
  expect_equal(assess_build(expected, observed), character(0))
})

test_that("an unusable record is the only reason", {
  exe <- local_fake_exe()
  record <- example_record(exe)
  expected <- example_expected(record)
  observed <- example_observed(record)
  expected$request$stanc_name <- "other_model"
  reasons <- c(
    "missing", "unreadable", "unsupported_format", "artifact_mismatch"
  )
  for (reason in reasons) {
    observed$record <- list(status = "unavailable", reason = reason)
    expect_equal(assess_build(expected, observed), reason)
  }
})

test_that("a replaced executable is caught only by the object's own hash", {
  exe <- local_fake_exe()
  record <- example_record(exe)
  expected <- example_expected(record)
  observed <- example_observed(record)
  expected$artifact <- "0000"
  expect_equal(assess_build(expected, observed), "artifact")
  expected$artifact <- NULL
  expect_equal(assess_build(expected, observed), character(0))
})

test_that("unresolved dependencies are skipped, not read as empty", {
  exe <- local_fake_exe()
  record <- example_record(exe)
  expected <- example_expected(record)
  observed <- example_observed(record)
  observed$dependencies <- NULL
  observed$builder$path <- "/opt/cmdstan-2.40.0"
  expect_equal(assess_build(expected, observed), "builder")
  expected$request$stanc_name <- "other_model"
  expect_equal(assess_build(expected, observed), c("stanc_name", "builder"))
})

test_that("a gone builder is not a trigger", {
  exe <- local_fake_exe()
  record <- example_record(exe)
  gone <- file.path(withr::local_tempdir(), "cmdstan")
  expect_false(dir.exists(gone))
  record$builder$path <- gone
  expected <- example_expected(record)
  observed <- example_observed(record)
  expect_equal(assess_build(expected, observed), character(0))
  observed$builder$path <- withr::local_tempdir()
  expect_equal(assess_build(expected, observed), "builder")
})
