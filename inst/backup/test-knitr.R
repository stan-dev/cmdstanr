context("knitr engine")

test_that("eng_cmdstan throws correct errors", {
  skip_if_not_installed("knitr")
  expect_error(eng_cmdstan(list(output.var = 1)), "must be a character string")
  expect_error(eng_cmdstan(list(output.var = c("A", "B"))), "must be a character string")
})

test_that("eng_cmdstan works", {
  skip_if_not_installed("knitr")
  code <- "
  parameters {
    real y;
  }
  model {
    y ~ std_normal();
  }
  "
  opts <- knitr::opts_chunk$merge(list(
    output.var = "ABC",
    code = code,
    cache = TRUE,
    cache.path = tempdir()
  ))
  expect_interactive_message(eng_cmdstan(opts), "Compiling Stan program")
  opts$eval <- FALSE
  expect_silent(eng_cmdstan(opts))
})

test_that("register_knitr_engine works with and without override", {
  skip_if_not_installed("knitr")

  knitr::knit_engines$delete(keys = "stan")
  expect_false("stan" %in% names(knitr::knit_engines$get()))

  register_knitr_engine(override = TRUE)
  expect_false("cmdstan" %in% names(knitr::knit_engines$get()))
  expect_true("stan" %in% names(knitr::knit_engines$get()))

  register_knitr_engine(override = FALSE)
  expect_true("cmdstan" %in% names(knitr::knit_engines$get()))

  knitr::knit_engines$restore()
})
