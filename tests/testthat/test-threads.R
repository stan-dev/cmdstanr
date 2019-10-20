# Setup -------------------------------------------------------------------
NOT_CRAN <-
  identical(Sys.getenv("NOT_CRAN"), "true") ||
  identical(Sys.getenv("TRAVIS"), "true")

test_that("Setting STAN_NUM_THREADS", {
  set_num_threads(10)
  threads = as.integer(Sys.getenv("STAN_NUM_THREADS"))
  expect_equal(threads, 10)
  expect_equal(threads, num_threads())

  set_num_threads(4)
  threads = as.integer(Sys.getenv("STAN_NUM_THREADS"))
  expect_equal(threads, 4)
  expect_equal(threads, num_threads())
})

test_that("Setting STAN_NUM_THREADS", {
  set_num_threads(10)
  expect_equal(num_threads(), 10)
  expect_equal(num_threads(), as.integer(Sys.getenv("STAN_NUM_THREADS")))
})
