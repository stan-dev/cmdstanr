stan_code <- "
parameters {
  real y;
}
model {
  y ~ std_normal();
}
"
stan_file <- write_stan_file(stan_code)

test_that("print_stan_file() prints plain code outside of knitr", {
  out <- capture.output(print_stan_file(stan_file))
  expect_snapshot(cat(out, sep = "\n"), cran = TRUE)
})

test_that("print_stan_file() returns file path invisibly", {
  out <- withr::with_output_sink(tempfile(), print_stan_file(stan_file))
  expect_identical(out, stan_file)
})

test_that("print_stan_file() outputs fenced code block in knitr with results='asis'", {
  out <- with_mocked_bindings(
    capture.output(print_stan_file(stan_file)),
    is_knitr_asis_output = function() TRUE
  )
  expect_snapshot(cat(out, sep = "\n"), cran = TRUE)
})

test_that("print_stan_file() wraps in <details> when fold=TRUE", {
  out <- with_mocked_bindings(
    capture.output(print_stan_file(stan_file, fold = TRUE)),
    is_knitr_asis_output = function() TRUE
  )
  expect_snapshot(cat(out, sep = "\n"), cran = TRUE)
})

test_that("print_stan_file() uses custom summary text", {
  out <- with_mocked_bindings(
    capture.output(print_stan_file(stan_file, fold = TRUE, summary = "My Stan Code")),
    is_knitr_asis_output = function() TRUE
  )
  expect_snapshot(cat(out, sep = "\n"), cran = TRUE)
})

test_that("print_stan_file() does not fold when fold=FALSE in knitr", {
  out <- with_mocked_bindings(
    capture.output(print_stan_file(stan_file, fold = FALSE)),
    is_knitr_asis_output = function() TRUE
  )
  expect_snapshot(cat(out, sep = "\n"), cran = TRUE)
})

test_that("print_stan_file() falls back to plain text without results='asis'", {
  out <- with_mocked_bindings(
    capture.output(print_stan_file(stan_file)),
    is_knitr_asis_output = function() FALSE
  )
  expect_snapshot(cat(out, sep = "\n"), cran = TRUE)
})

test_that("print_stan_file() falls back to plain text without knitr.in.progress", {
  withr::local_options(knitr.in.progress = FALSE)
  out <- capture.output(print_stan_file(stan_file))
  expect_snapshot(cat(out, sep = "\n"), cran = TRUE)
})
