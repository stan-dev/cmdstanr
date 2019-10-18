# Setup -------------------------------------------------------------------
NOT_CRAN <-
  identical(Sys.getenv("NOT_CRAN"), "true") ||
  identical(Sys.getenv("TRAVIS"), "true")

test_that("Setting STAN_NUM_THREADS", {
  before_test_threads <- get_num_threads()

  set_num_threads(10)
  threads = as.numeric(Sys.getenv("STAN_NUM_THREADS"))
  expect_equal(threads, 10)
  expect_equal(threads, get_num_threads())

  set_num_threads(4)
  threads = as.numeric(Sys.getenv("STAN_NUM_THREADS"))
  expect_equal(threads, 4)
  expect_equal(threads, get_num_threads())

  #restore previous state
  set_num_threads(before_test_threads)
})

test_that("Setting STAN_NUM_THREADS", {
  before_test_threads <- get_num_threads()
  set_num_threads(10)
  expect_equal(get_num_threads(), 10)
  expect_equal(get_num_threads(), as.numeric(Sys.getenv("STAN_NUM_THREADS")))

  #restore previous state
  set_num_threads(before_test_threads)
})
