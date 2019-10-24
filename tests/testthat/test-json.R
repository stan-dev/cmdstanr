# Setup -------------------------------------------------------------------
NOT_CRAN <-
  identical(Sys.getenv("NOT_CRAN"), "true") ||
  identical(Sys.getenv("TRAVIS"), "true")


if (NOT_CRAN) {
  set_cmdstan_path()
}


test_that("test JSON output unboxing", {
  skip_on_cran()
  temp_file <- tempfile()
  N <- 10
  write_stan_json(list(N = N), file = temp_file)
  json_output <- readLines(temp_file)
  expect_equal(json_output, "{\"N\":10}")
})

test_that("test JSON output boolean", {
  skip_on_cran()
  temp_file <- tempfile()
  N <- c(TRUE, FALSE, TRUE)
  write_stan_json(list(N = N), file = temp_file)
  json_output <- readLines(temp_file)
  expect_equal(json_output, "{\"N\":[1,0,1]}")
})

test_that("test JSON output factor", {
  skip_on_cran()
  temp_file <- tempfile()
  N = factor(c(0,1,2,2,1,0), labels = c("c1", "c2", "c3"))
  write_stan_json(list(N = N), file = temp_file)
  json_output <- readLines(temp_file)
  expect_equal(json_output, "{\"N\":[1,2,3,3,2,1]}")
})

test_that("test JSON output integer vector", {
  skip_on_cran()
  temp_file <- tempfile()
  N = c(1.0, 2.0, 3, 4)

  write_stan_json(list(N = N), file = temp_file)
  json_output <- readLines(temp_file)
  expect_equal(json_output, "{\"N\":[1,2,3,4]}")
})

test_that("test JSON output lists", {
  skip_on_cran()
  temp_file <- tempfile()
  I <- 2; J <- 3;
  N <- lapply(1:I, function(i) rep(26, J))


  write_stan_json(list(N = N), file = temp_file)
  json_output <- readLines(temp_file)
  expect_equal(json_output, "{\"N\":[[26,26,26],[26,26,26]]}")
})

test_that("test JSON errors", {
  skip_on_cran()
  temp_file <- tempfile()
  N = c(1.0, 2.0, 3, 4)
  expect_error(
    write_stan_json(list(N = N), file = c(1,2)),
    "The supplied filename is invalid!"
  )
  expect_error(
    write_stan_json(list(N = N), file = ""),
    "The supplied filename is invalid!"
  )
  I <- 2; J <- 3;
  N <- lapply(1:I, function(i) {
    if (i==1)
      rep(26, J)
    else
      rep(26, J-1)
  })
  expect_error(
    write_stan_json(list(N = N), file = "abc.txt"),
    "All matrices/vectors in the list must be the same size!"
  )
  N <- lapply(1:I, function(i) {
    if (i==1)
      rep(26, J)
    else
      J
  })
  expect_error(
    write_stan_json(list(N = N), file = "abc.txt"),
    "All matrices/vectors in the list must be the same size!"
  )
  I <- 2; J <- 3;
  N <- lapply(1:I, function(i) {
    if (i==1)
      rep(26, J)
    else
      NULL
  })
  expect_error(
    write_stan_json(list(N = N), file = "abc.txt"),
    "All matrices/vectors in the list must be the same size!"
  )
})
