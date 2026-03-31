stan_code <- "
parameters {
  real y;
}
model {
  y ~ std_normal();
}
"
stan_file <- write_stan_file(stan_code)
stan_lines <- readLines(stan_file)

test_that("print_stan_file() prints plain code outside of knitr", {
  out <- capture.output(print_stan_file(stan_file))
  expect_identical(out, stan_lines)
})

test_that("print_stan_file() returns file path invisibly", {
  out <- withr::with_output_sink(tempfile(), print_stan_file(stan_file))
  expect_identical(out, stan_file)
})

test_that("print_stan_file() outputs fenced code block in knitr with results='asis'", {
  out <- with_mocked_bindings(
    paste(capture.output(print_stan_file(stan_file)), collapse = "\n"),
    is_knitr_asis_output = function() TRUE
  )
  expect_match(out, "^```stan\n")
  expect_match(out, "\n```$")
  expect_match(out, paste(stan_lines, collapse = "\n"), fixed = TRUE)
})

test_that("print_stan_file() wraps in <details> when fold=TRUE", {
  out <- with_mocked_bindings(
    capture.output(print_stan_file(stan_file, fold = TRUE)),
    is_knitr_asis_output = function() TRUE
  )
  expect_match(out[1], "<details><summary>Stan model code</summary>")
  expect_match(out[length(out)], "</details>")
})

test_that("print_stan_file() uses custom summary text", {
  out <- with_mocked_bindings(
    capture.output(print_stan_file(stan_file, fold = TRUE, summary = "My Stan Code")),
    is_knitr_asis_output = function() TRUE
  )
  expect_match(out[1], "<details><summary>My Stan Code</summary>")
})

test_that("print_stan_file() does not fold when fold=FALSE in knitr", {
  out <- with_mocked_bindings(
    capture.output(print_stan_file(stan_file, fold = FALSE)),
    is_knitr_asis_output = function() TRUE
  )
  expect_no_match(paste(out, collapse = "\n"), "<details>", fixed = TRUE)
  expect_no_match(paste(out, collapse = "\n"), "</details>", fixed = TRUE)
})

test_that("print_stan_file() falls back to plain text without results='asis'", {
  out <- with_mocked_bindings(
    capture.output(print_stan_file(stan_file)),
    is_knitr_asis_output = function() FALSE
  )
  expect_identical(out, stan_lines)
})

test_that("print_stan_file() falls back to plain text without knitr.in.progress", {
  withr::local_options(knitr.in.progress = FALSE)
  out <- capture.output(print_stan_file(stan_file))
  expect_identical(out, stan_lines)
})
