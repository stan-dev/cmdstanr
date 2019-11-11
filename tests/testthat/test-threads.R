context("threads")

Sys.unsetenv("STAN_NUM_THREADS")

test_that("num_threads() is NA if not set", {
  expect_identical(num_threads(), NA_integer_)
})

test_that("Setting STAN_NUM_THREADS", {
  set_num_threads(10)
  threads <- Sys.getenv("STAN_NUM_THREADS")
  expect_identical(threads, "10")
  expect_identical(as.integer(threads), num_threads())

  set_num_threads(4)
  expect_equal(num_threads(), 4)
})

test_that("set_num_threads() throws correct errors", {
  bad <- list("10", 1.5, -1, FALSE)
  for (j in seq_along(bad)) {
    expect_error(set_num_threads(!! bad[[j]]), "valid number of threads")
  }
})
